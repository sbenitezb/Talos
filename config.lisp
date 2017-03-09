(in-package #:talos)

(defvar +app-folder+ (make-pathname :directory '(:relative "talos")))
(defvar +cfg-spec+ (make-pathname :name :wild :type "cfg"))

(defclass config ()
  ((hostname
    :initarg :hostname
    :accessor config-hostname
    :initform "localhost")
   (port
    :initarg :port
    :accessor config-port
    :initform 5432
    :type fixnum)
   (database
    :initarg :database
    :accessor config-database)
   (user
    :initarg :user
    :accessor config-user
    :initform "talos")
   (password
    :initarg :password
    :accessor config-password)
   (launch-mode
    :accessor config-launch-mode
    :initform :production)
   (docbase
    :accessor config-docbase
    :initform "./static/")
   (templates-path
    :accessor config-templates-path
    :initform "./tmpl/")))

(defvar *talos-config* (make-instance 'config))

(defun private-folder ()
  (merge-pathnames (merge-pathnames-as-directory (user-homedir-pathname) +app-folder+)))
  
(defun read-config-file (filespec)
  "Lee el archivo de configuración desde la ruta FILESPEC y reemplaza
la configuración global de la variable *TALOS-CONFIG*."
  (with-open-file (s filespec :direction :input)
    ;; Lee las expresiones desde el stream, descartando lo que no sea una lista.
    (let ((*package* (find-package :talos)))
      (flet ((read-exps (s)
               (loop for exp = (read s nil)
                  while exp
                  when (listp exp) append exp)))
        (dolist (cons (read-exps s))
          (let ((sym (car cons)))
            (unless
                (and (symbolp sym)
                     (slot-exists-p *talos-config* sym))
              (error "'~a' no es del tipo SYMBOL o no se reconoce como ~
variable de configuración. Formato incorrecto en la configuración." sym))
            (setf (slot-value *talos-config* sym) (cdr cons))))))))

