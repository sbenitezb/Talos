;;;; package.lisp

(defpackage #:talos
  (:use #:cl
        #:hunchentoot
        #:cl-who
        #:cl-interpol
        #:cl-utilities
        #:uri-template)
  (:export #:main
           #:stop-server
           #:*talos-config*))
