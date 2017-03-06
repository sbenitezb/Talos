;;;; talos.lisp

(in-package #:talos)

;;; "talos" goes here. Hacks and glory await!

(let ((server ()))
  (defun start-server (port)
    (setf *default-content-type* "text/html; charset=UTF-8")
    (setf (html-mode) :html5)
    (setf server (start (make-instance 'easy-acceptor :port port))))
  (defun stop-server()
    (stop server)))

(defun config-error-handler (error-message)
  (with-html-output-to-string (s)
    (:html
     (:body
      (:h2 "No se pudo leer o procesar el archivo de
configuración talos.cfg en el directorio local de la aplicación.")
      (:h3 (fmt "~A" error-message))))))

(defun reload-config ()
  "Vuelve a leer el archivo de configuración. Si no se puede leer,
añade un interceptor de HANDLERS a la tabla dispatch para que los REQUEST
devuelvan siempre un error."
  (handler-case
      (progn
        (read-config-file "./talos.cfg")
        ;; Remueve el primer handler de la lista, a menos que sea
        ;; un símbolo, en este caso DISPATCH-EASY-HANDLERS.
        (unless (symbolp (first *dispatch-table*))
          (pop *dispatch-table*)))
    (error (c)
      ;; Añade un nuevo handler al comienzo de la lista dispatch, que será
      ;; el que muestre el mensaje de error.
      (let ((handler (create-regex-dispatcher
                      "^*$" #'(lambda () (funcall #'config-error-handler c)))))
        (pushnew handler *dispatch-table*)))))

(defun main (&key (port 8080))
  (reload-config)
  (pushnew (create-folder-dispatcher-and-handler
            "/static/"
            (config-docbase *talos-config*))
           *dispatch-table*)
  (start-server port))
