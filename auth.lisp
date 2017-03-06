(in-package #:talos)

;;(defmethod handle-request :before ((*acceptor* acceptor) (*request* request))
;;  "Añade autorización HTTP básica a todos los requests."
;;  (multiple-value-bind (user password) (authorization)
;;    (unless (and (string= user "value")
;;                 (string= password "bind"))
;;      (require-authorization "talos"))))
