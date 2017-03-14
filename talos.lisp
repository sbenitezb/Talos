;;;; talos.lisp

(in-package #:talos)

;;; "talos" goes here. Hacks and glory await!

(defcategory :critical)
(defcategory :error (or :error :critical))
(defcategory :warning (or :warning :error))
(defcategory :notice (or :notice :warning))
(defcategory :info (or :info :notice))
(defcategory :debug (or :debug :info))

;;; Configurar el logger para que formatee los mensajes con la fecha ISO.
(defmethod format-message ((self formatted-message))
  (multiple-value-bind (second minute hour date month year dow dst-p tz)
      (decode-universal-time (timestamp-universal-time (message-timestamp self)))
    (format nil  "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d+~2,'0d:00 ~a ~?~&"
            year month date hour minute second tz
            (message-category self)
            (message-description self)
            (message-arguments self))))

(defun setup-logger ()
  (setf (log-manager)
        (make-instance 'log-manager :message-class 'formatted-message))
  ;; TODO: redirigir a archivo?
  (start-messenger 'text-stream-messenger :stream *standard-output*))

(defun configure-defaults ()
  (push :PUT hunchentoot:*methods-for-post-parameters*)
  (push :DELETE hunchentoot:*methods-for-post-parameters*)
  (setf *default-template-pathname* (config-templates-path *talos-config*))
  (setf *ignore-empty-lines* t)
  (setf *default-content-type* "text/html; charset=UTF-8"))

(defun start-server (port)
  (log-message :info "Iniciando el servicio web")
  (configure-defaults)
  (setf (html-mode) :html5)
  (setf server (start (make-instance 'easy-acceptor :port port))))
  
(defun stop-server ()
  (log-message :info "Deteniendo el servicio web")
  (stop server))

(defun config-error-handler (error-message)
  (with-html-output-to-string (s)
    (:html
     (:body
      (:h2 "No se pudo leer o procesar el archivo de
configuración talos.cfg en el directorio local de la aplicación.")
      (:h3 (fmt "~A" error-message)))))
  (log-message :critical "~a" error-message))

(defun reload-config ()
  "Vuelve a leer el archivo de configuración. Si no se puede leer,
añade un interceptor de HANDLERS a la tabla dispatch para que los REQUEST
devuelvan siempre un error."
  (handler-case
      (progn
        (log-message :info "Leyendo configuración")
        (read-config-file (merge-pathnames (private-folder) "talos.cfg"))
        ;; Remueve el primer handler de la lista, a menos que sea
        ;; un símbolo, en este caso DISPATCH-EASY-HANDLERS.
        (unless (symbolp (first *dispatch-table*))
          (pop *dispatch-table*)))
    (error (c)
      ;; Añade un nuevo handler al comienzo de la lista dispatch, que será
      ;; el que muestre el mensaje de error.
      (let ((handler (create-regex-dispatcher
                      "^*$" #'(lambda () (config-error-handler c)))))
        (pushnew handler *dispatch-table*)))))

(defun init (port)
  (ensure-directories-exist (private-folder))
  (setup-logger)
  (log-message :notice "Talos server")
  (reload-config)

  ;; Verificar la conexión con Postgres y configurar.
  ;;(handler-case
  ;;    (with-slots (hostname port database user password) *talos-config*
  ;;        (setup-postgres hostname port database user password))
  ;;  (postmodern:database-error (e)
  ;;    (log-message :critical "No se pudo establecer conexión con Postgres. Abortando.")
  ;;    (return-from init :abort)))
  
  ;; Seleccionar el modo de inicio, que puede ser :production o :development
  ;; Para el modo :development se agrega un handler para servir el directorio static.
  ;; En modo production conviene utilizar Nginx como reverse proxy.
  (case (config-launch-mode *talos-config*)
    (:development
     (log-message :info "Lanzando en modo desarrollo")
     (setf *show-lisp-errors-p* t)
     (setf *show-lisp-backtraces-p* t))
    (:production
     (log-message :info "Lanzando en modo producción")
     (setf *show-lisp-errors-p* nil)
     (setf *show-lisp-backtraces-p* nil))
    (otherwise
     (log-message :warning "Se especificó un valor incorrecto de configuración para launch-mode")))

  (pushnew (create-folder-dispatcher-and-handler
            "/static/"
            (config-docbase *talos-config*))
           *dispatch-table*)
  (start-server port))

(defun main (&key (port 9090))
  (case (init port)
    (:abort (exit 1))
    (otherwise)))
    
