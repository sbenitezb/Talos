(in-package #:talos)

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
        (mapc #'ccl:process-kill monitor)
        (delete monitor monitors)
        (setf monitor nil)))))

(defun schedule-client-fixes (manager)
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
  (multiple-value-bind (inactive active reachable)
      (filter-batch batch)
    (when (> (length reachable) 0)
      (process-batch reachable manager))))

(defun filter-batch (batch)
  (log-message :debug "Filtrando clientes inactivos o inaccesibles")
  (let* ((active (remove-if #'client-disabled-p batch))
         (inactive (set-difference batch active))
         (reachable (remove-if (complement #'client-reachable-p) active)))
    (values inactive active reachable)))

(defun process-batch (batch manager)
  (log-message :debug "Procesando lote (~d clientes)" (length batch))
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

