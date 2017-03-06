(in-package #:talos)

(defclass config ()
  ((hostname :initarg :hostname
             :accessor config-hostname
             :initform "localhost")
   (port :initarg :port
         :accessor config-port
         :initform 5432
         :type fixnum)
   (database :initarg :database
             :accessor config-database)
   (user :initarg :user
         :accessor config-user
         :initform "talos")
   (password :initarg :password
             :accessor config-password)
   (docbase :initarg :docbase
            :accessor config-docbase)))

(defvar *talos-config* (make-instance 'config))

(defun read-config-file (filespec)
  "Lee el archivo de configuración desde la ruta FILESPEC y reemplaza
la configuración global de la variable *TALOS-CONFIG*."
  (with-open-file (s filespec :direction :input)
    ;; Lee las expresiones desde el STREAM, descartando lo que no sea LIST.
    (flet ((read-exps (s)
             (loop for exp = (read s nil)
                while exp
                when (listp exp) append exp)))
      (dolist (cons (read-exps s))
        (let ((sym (car cons)))
          (unless
              (and (symbolp sym)
                   (slot-exists-p *talos-config* sym))
            (error "'~A' no es del tipo SYMBOL o no se reconoce como ~
variable de configuración. Formato incorrecto en la configuración." sym))
          (setf (slot-value *talos-config* sym) (cdr cons)))))))
