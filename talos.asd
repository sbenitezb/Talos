;;;; talos.asd

(asdf:defsystem #:talos
  :serial t
  :description "Describe talos here"
  :author "Sebastián Benítez <sebenitez@trabajo.gob.ar>"
  :license "MIT"
  :depends-on (#:hunchentoot
               #:cl-who
               #:postmodern
               #:uri-template
               #:cl-interpol
               #:parenscript
               #:cl-ppcre
               #:cl-json
               #:cl-utilities)
  :components ((:file "package")
               (:file "macros")
               (:file "config")
               (:file "database")
               (:file "auth")
               (:file "pagegen")
               (:file "handlers")
               (:file "talos")))

