(in-package #:talos)

(defclass fix-manager ()
  ((batch-size :initarg :batch-size :reader fix-manager-batch-size)
   (fixing-interval :initarg :fixing-interval :accessor fix-manager-fixing-interval)
   (thread :reader fix-manager-thread :initform (ccl:make-process 'fix-manager))
   (lock :accessor fix-manager-lock :initform (ccl:make-lock)))
  (:default-initargs
   :batch-size 10
    :fixing-interval 30))

(defmethod initialize-instance :after ((manager fix-manager) &key &allow-other-keys)
  (ccl:process-preset (fix-manager-thread manager) #'schedule-client-fixes manager)
  (ccl:process-enable (fix-manager-thread manager) 5)
  (log-message :info "Iniciando Fix Manager"))

(defun schedule-client-fixes (manager)
  (loop
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
         (mapc #'process-batch batches)))
     (log-message :info "Esperando próximo intervalo")
     (sleep (* (fix-manager-fixing-interval manager) 60 60))))

(defun process-batch (batch)
  (log-message :info "Procesando lote")
  (mapc #'fix-client batch))

(defun fix-client (client)
  "Mata procesos en ejecución que interfieren con FixCliente y finalmente lanza
el proceso FixCliente en el equipo remoto."
  (with-slots (name) client
    (when (eq (ping name) :success)
      (macrolet ((build-params (params)
                   `(list (format nil "\\\\~a" name) ,@params))
                 (build-exepath (exe)
                   `(substitute #\\ #\/ (namestring (merge-pathnames-as-file
                                                     (bin-folder) ,exe)))))
        (let ((pskill (build-exepath "pskill.exe"))
              (psexec (build-exepath "psexec.exe"))
              (fixclient (build-exepath "FixCliente.exe")))
          (log-message :info "Procesando cliente ~a" name)
          (ccl:run-program pskill (build-params ("cscript.exe")))
          (ccl:run-program pskill (build-params ("FixCliente.exe")))
          (ccl:run-program pskill (build-params ("gpupdate.exe")))
          (ccl:run-program psexec (build-params ("-s" "-d" "-f" "-c" fixclient))
                           :wait nil))))))

(defun monitor-fix (client))
