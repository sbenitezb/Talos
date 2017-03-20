(in-package #:talos)

(defvar *initial-wait* 2) ;; Tiempo de espera inicial en minutos

(defclass fix-manager ()
  ((batch-size :initarg :batch-size :reader fix-manager-batch-size)
   (fixing-interval :initarg :fixing-interval :accessor fix-manager-fixing-interval)
   (thread :reader fix-manager-thread :initform (ccl:make-process 'fix-manager))
   (lock :accessor fix-manager-lock :initform (ccl:make-lock))
   (monitors :accessor fix-manager-monitors :initform '()))
  (:default-initargs
   :batch-size 10
    :fixing-interval 30))

(defmethod initialize-instance :after ((manager fix-manager) &key &allow-other-keys)
  (ccl:process-preset (fix-manager-thread manager) #'schedule-client-fixes manager)
  (ccl:process-enable (fix-manager-thread manager) 5)
  (log-message :notice "Iniciando Fix Manager"))

(defmethod cancel-monitors ((manager fix-manager))
  "Cancela todos los monitores activos"
  (with-accessors ((lock fix-manager-lock) (monitors fix-manager-monitors)) manager
    (ccl:with-lock-grabbed (lock)
      (dolist (monitor monitors)
        (ccl:process-kill monitor)
        (delete monitor monitors)
        (setf monitor nil)))))

(defun schedule-client-fixes (manager)
  (log-message :info "Esperando ~d minutos para comenzar a procesar" *initial-wait*)
  (sleep (* 60 *initial-wait*))
  (loop
     (let ((monitor-count (length (fix-manager-monitors manager))))
       (when (> monitor-count 0)
         (log-message :info "Cancelando ~d monitores activos" monitor-count)
         (cancel-monitors manager)))
     (flet ((make-batches (clients batch-size)
              ;; Agrupa los clientes en lotes de una cantidad fija
              (loop with client-count = (length clients)
                 for start from 0 to (1- client-count) by batch-size
                 for end = (min client-count (+ start batch-size))
                 collecting (subseq clients start end))))
       (let* ((clients (fetch-clients))
              (batch-size (fix-manager-batch-size manager))
              (batches (make-batches clients batch-size)))
         (log-message :info "Procesando ~d lotes de ~d unidades"
                      (length batches) batch-size)
         ;; Procesar lotes
         (mapc #'(lambda (b) (prepare-batch b manager)) batches)))
     (log-message :info "Esperando el próximo intervalo de ~d minutos"
                  (fix-manager-fixing-interval manager))
     (sleep (* (fix-manager-fixing-interval manager) 60))))

(defun prepare-batch (batch manager)
  (multiple-value-bind (inactive active reachable unreachable)
      (filter-batch batch)
    ;; Excluye clientes inactivos de la base y los agrega en el archivo
    ;; disabled.txt en la carpeta privada del programa.
    (when (> (length inactive) 0)
      (exclude-inactive-clients inactive))

    ;; Marca en la base los equipos que no resuelven IP o no responden
    ;; al ping.
    (when (> (length unreachable) 0)
      (mark-unreachable-clients unreachable))

    ;; Marca en la base los equipos accesibles como no reparados
    (when (> (length reachable) 0)
      (mark-unrepaired-clients reachable)
      (process-batch reachable manager))))

(defun filter-batch (batch)
  (log-message :debug "Filtrando clientes inactivos o inaccesibles")
  (let* ((active (remove-if #'client-disabled-p batch))
         (inactive (remove-if (complement #'client-disabled-p) batch))
         (reachable (remove-if (complement #'client-reachable-p) active))
         (unreachable (remove-if #'client-reachable-p active)))
    (log-message :debug "En este lote: ~d inactivos, ~d activos, ~d accesibles"
                 (length inactive) (length active) (length reachable))
    (values inactive active reachable unreachable)))

(defun process-batch (batch manager)
  (log-message :info "Procesando lote (~d clientes)" (length batch))
  (mapc #'fix-client batch)
  ;; Monitorea el proceso de reparación en un thread aparte por cada batch
  (with-accessors ((lock fix-manager-lock) (monitors fix-manager-monitors)) manager
    (ccl:with-lock-grabbed (lock)
      (push (ccl:process-run-function 'batch-monitor #'monitor-fixes batch)
            monitors))))

(defun client-reachable-p (client)
  "Verifica si el cliente responde al ping"
  (with-slots (name) client
    (eq (ping name) :success)))

(defun client-disabled-p (client)
  "Verifica si el cliente está inhabilitado en AD"
  (flet ((dsquery (name)
           (with-output-to-string (stream)
             (let ((proc (ccl:run-program "dsquery"
                                          (list "computer" "-name" (coerce name 'simple-string) "-disabled" "-q")
                                          :output stream)))
               (multiple-value-bind (status code)
                   (ccl:external-process-status proc)
                 (when (eq status :error)
                   (log-message :critical "dsquery no está disponible")
                   (error "dsquery no es un comando válido en esta versión de Windows")))))))
    (with-slots (name) client
      ;; Si dsquery no encuentra un equipo con este nombre y deshabilitado, la
      ;; salida es nula.
      (> (length (dsquery name)) 0))))

(defun fix-client (client)
  "Mata procesos en ejecución que interfieren con FixCliente y finalmente lanza
el proceso FixCliente en el equipo remoto."
  (with-slots (name) client
    (macrolet ((build-params (params)
                 `(list (format nil "\\\\~a" name) ,@params))
               ;; Sustituir / por \ en la ruta del ejecutable
               (build-exepath (exe)
                 `(substitute #\\ #\/ (namestring (merge-pathnames-as-file
                                                   (bin-folder) ,exe)))))
      (let ((pskill (build-exepath "pskill.exe"))
            (psexec (build-exepath "psexec.exe"))
            (fixclient (build-exepath "FixCliente.exe")))
        (log-message :debug "Procesando cliente ~a" name)
        (ccl:run-program pskill (build-params ("cscript.exe")))
        (ccl:run-program pskill (build-params ("FixCliente.exe")))
        (ccl:run-program pskill (build-params ("gpupdate.exe")))
        (ccl:run-program psexec (build-params ("-s" "-d" "-f" "-c" fixclient))
                         :wait nil)))))

(defun monitor-fixes (batch)
  "Monitorea el proceso de reparación"
  (let ((monitored batch))
    (log-message :debug "Monitoreando reparación del lote")
    (loop
       (if (> (length monitored) 0)
           (progn
             (dolist (client batch)
               (sleep 10)))
           (return)))))

(defun monitor-fix (client)
  (with-slots (name) client
    ))

(defun exclude-inactive-clients (clients)
  ;; Guardar en un archivo la lista de clientes inactivos y remover el
  ;; cliente de la base.
  (with-open-file (file (merge-pathnames-as-file (private-folder) "disabled.txt")
                        :direction :output :if-exists :append
                        :if-does-not-exist :create)
    (multiple-value-bind (sec min hour date month year day dlp tz) (get-decoded-time)
      (log-message :debug "Excluyendo ~d clientes inactivos" (length clients))
      (dolist (client clients)
        (with-slots (name) client
          (format file "~4d-~2,'0d-~2,'0d : ~a~%" year month date name)
          ;; Actualiza la base dando de baja el equipo
          (mark-obsolete name))))))

(defun mark-unreachable-clients (clients)
  (log-message :debug "Marcando ~d clientes como inaccesibles" (length clients))
  (dolist (client clients)
    (with-slots (name) client
      (mark-unreachable name))))

(defun mark-unrepaired-clients (clients)
  (log-message :debug "Marcando ~d clientes como no reparados" (length clients))
  (dolist (client clients)
    (with-slots (name) client
      (mark-unrepaired name))))
