;;;; package.lisp

(defpackage #:talos
  (:use #:cl
        #:hunchentoot
        #:cl-who
        #:cl-log
        #:cl-fad
        #:cl-interpol
        #:alexandria
        #:uri-template)
  (:shadowing-import-from #:cl-fad #:copy-stream #:copy-file)
  (:export #:main
           #:stop-server
           #:*talos-config*))
