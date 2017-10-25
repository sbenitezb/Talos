(in-package #:talos)

(defun req-match (regex req)
  "Devuelve T si el nombre del script del request REQ corresponde a la expresión
regular REGEX."
  (let ((scanner (ppcre:create-scanner regex)))
    (ppcre:scan scanner (script-name req))))

(defun paginate (max page)
  (declare (type fixnum max page))
  (let ((max (if (or (null max) (< max 1)) 25 max))
        (page (if (or (null page) (< page 1)) 1 page)))
    (values max (* max (- page 1)))))

(define-easy-handler (handle-noop :uri "/noop") ()
  "noop")

;;; Define un handler que maneja solicitudes a la URL que muestra la lista
;;; de clientes. Soporta los métodos GET y POST.
(define-easy-handler (handle-clients :uri "/clients") (client)
  ;; Redireccionar a la página de información del cliente si el
  ;; campo de búsqueda CLIENT no está vacío y el método es GET,
  ;; o agregar un nuevo cliente a la cola si el método es POST.
  (unless (null client)
    (case (request-method*)
      (:get (redirect #u/clients/{client}))
      (:post (append-to-queue (escape-for-html client) *fix-manager*)
             (redirect #u/clients))))
  
  ;; Mostrar una lista con el estado de los clientes que faltan reparar.
  (let ((clients (fetch-clients)))
    (render-page (list #'(lambda () (render-clients #uclients clients))))))

;;; Define un handler que maneja solicitudes a la URL que muestra
;;; la lista de reparaciones.
(define-easy-handler (handle-fixes :uri "/fixes" :default-request-type :get) ((max :parameter-type 'integer) (page :parameter-type 'integer))
  (multiple-value-bind (max rpage) (paginate max page)
    (let* ((fixes (fetch-max-fixes :max max :from rpage))
           (pages (ceiling (/ (count-fixes) max))))
      (render-page (list #'(lambda () (render-all-fixes #ufixes fixes max (or page 1) pages)))))))

;;; Define un handler que maneja solicitudes al raíz.
(define-easy-handler (handle-root :uri "/" :default-request-type :get) ()
  (render-page (list #'(lambda () (render-home)))))

;;; Define un handler que maneja solicitudes para mostrar información
;;; sobre las reparaciones de un cliente específico.
(define-easy-handler (handle-client
                      :uri #'(lambda (req)
                               (req-match "^/clients/[^\/]+$" req))
                      :default-request-type :get) ()
  (let* ((client (first (split-sequence #\/ (script-name*) :count 1 :from-end t)))
         (fixes (fetch-fixes :client client)))
    (render-page (list #'(lambda () (render-fixes #u/clients/ client fixes))))))

;;; Define un handler que maneja solicitudes para marcar un cliente
;;; como reparado.
(define-easy-handler (handle-mark-fixed
                      :uri #'(lambda (req)
                               (req-match "^/clients/.+/mark-fixed$" req))
                      :default-request-type :get) ()
  (let ((client (first (split-sequence #\/ (script-name*) :count 2 :from-end t))))
    (mark-fixed client)
    (redirect #u/clients)))

;;; Define un handler que maneja solicitudes para marcar un cliente
;;; como obsoleto.
(define-easy-handler (handle-mark-obsolete
                      :uri #'(lambda (req)
                               (req-match "^/clients/.+/mark-obsolete$" req))
                      :default-request-type :get) ()
  (let ((client (first (split-sequence #\/ (script-name*) :count 2 :from-end t))))
    (mark-obsolete client)
    (redirect #u/clients)))

;;; Define un handler para la ruta que genera estadísticas.
(define-easy-handler (handle-stats :uri "/stats" :default-request-type :get) ()
  (render-page (list #'(lambda () (render-stats)))))
