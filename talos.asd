;;;; talos.asd

(asdf:defsystem #:talos
  :serial t
  :description "Control de reparación de clientes ConfigMgr"
  :author "Sebastián Benítez <sebenitez@trabajo.gob.ar>"
  :license "MIT"
  :depends-on (#:hunchentoot
               #:cl-who
               #:cl-log
               #:cl-fad
               #:postmodern
               #:uri-template
               #:cl-interpol
               #:cl-ppcre
               #:cl-json
               #:alexandria
               #:cffi)
  :components ((:file "package")
               (:file "macros")
               (:file "config")
               (:file "database")
               (:file "auth")
               (:file "pagegen")
               (:file "handlers")
               (:file "icmp")
               (:file "fixer")
               (:file "talos")))

