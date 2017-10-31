;;;; talos.asd

(asdf:defsystem #:talos
  :serial t
  :description "Control de reparación de clientes ConfigMgr"
  :author "Sebastián Benítez <sebenitez@trabajo.gob.ar>"
  :version "1.2.2"
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
               #:split-sequence
               #:cffi)
  :components ((:file "package")
               (:file "macros")
               (:file "util")
               (:file "config")
               (:file "database")
               (:file "auth")
               (:file "pagegen")
               (:file "handlers")
               (:file "icmp")
               (:file "fixer")
               (:file "talos")))

