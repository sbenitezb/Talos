(in-package #:talos)

(defmacro with-database (&body body)
  `(with-slots (hostname port database user password) *talos-config*
     (postmodern:with-connection `(,database ,user ,password ,hostname)
       (progn ,@body))))

(defclass client ()
  ((id :initarg :id
       :col-type integer
       :accessor client-id)
   (name :initarg :name
         :col-type string
         :accessor client-name)
   (status :initarg :status
           :col-type integer
           :accessor client-status)
   (status-description :initarg :descr
                       :col-type string
                       :accessor client-status-description)
   (first-seen :initarg :first-seen
               :col-type integer
               :accessor client-first-seen)
   (last-seen :initarg :last-seen
              :col-type integer
              :accessor client-last-seen)
   (reachable :initarg :reachable
              :col-type boolean
              :accessor client-reachable))
  (:metaclass postmodern:dao-class)
  (:table-name "check_clients")
  (:keys id name))

(defclass fix ()
  ((id :initarg :id
       :col-type integer
       :accessor fix-id)
   (client :initarg :client
           :col-type string
           :accessor fix-client)
   (date-fixed :initarg :date-fixed
               :col-type integer
               :accessor fix-date-fixed))
  (:metaclass postmodern:dao-class)
  (:table-name "fixes")
  (:keys id client))

(defun fetch-clients (&key (filter t) (sort 'name))
  "Devuelve una lista de CLIENT."
  (with-database
    (postmodern:select-dao 'client filter sort)))

(defun fetch-fixes (&key client)
  "Devuelve una lista de FIX para un cliente CLIENT específico."
  (with-database
    (postmodern:query-dao
     'fix (:select 'f.id (:as 'c.name 'client) 'f.date_fixed
                   :from (:as 'fixes 'f)
                   :inner-join (:as 'clients 'c)
                   :on (:= 'f.client 'c.id)
                   :where (:= 'c.name client)))))

(defun count-fixes ()
  "Devuelve la cantidad total de fixes."
  (with-database
    (postmodern:query "SELECT count(*) FROM fixes" :single)))

(defun fetch-max-fixes (&key (max 25) (from 0) (sort 'date_fixed) (order :desc))
  "Devuelve una lista de FIX, limitada a una cantidad específica de filas para
paginación, opcionalmente ordenando por un juego de columnas
especificadas en SORT."
  (with-database
    (postmodern:query-dao
     'fix (:limit
           (:order-by
            (:select 'f.id (:as 'c.name 'client) 'f.date_fixed
                     :from (:as 'fixes 'f)
                     :inner-join (:as 'clients 'c)
                     :on (:= 'f.client 'c.id))
            (:desc sort)) max from))))

(defun add-client (name)
  "Agrega o actualiza un cliente existente para que se vuelva a verificar."
  (with-database
    (postmodern:query "SELECT update_client($1::varchar, 2::smallint, 'No reparado', true)" name)))

(defun mark-fixed (client)
  "Marca un cliente CLIENT como arreglado en la base."
  (with-database
    (postmodern:query "SELECT mark_fixed($1::varchar)" client)))

(defun mark-obsolete (client)
  "Marca un cliente CLIENT como dado de baja en la base."
  (with-database
    (postmodern:query "SELECT mark_obsolete($1::varchar)" client)))

(defun mark-unreachable (client)
  "Marca un cliente CLIENT como inaccesible en la base."
  (with-database
    (postmodern:query "SELECT mark_unreachable($1::varchar)" client)))

(defun mark-unrepaired (client)
  "Marca un cliente CLIENT como no reparado en la base."
  (with-database
    (postmodern:query "SELECT mark_unrepaired($1::varchar)" client)))
