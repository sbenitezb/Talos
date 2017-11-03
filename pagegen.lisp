(in-package #:talos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-uri-template-syntax)
  (enable-interpol-syntax))

(defun render-paginator (uri max page pages)
  (with-html-output-to-string (s)
    (:ul :id "paging"
         (:li "Página ")
         (when (> page 1)
           (let ((next (1- page)))
             (htm
              (:li (:a :href #u{uri}?max={max}&page={next} (str "< Anterior"))))))
         (loop for p from (max 1 (- page 5)) to (min pages (+ page 5))
            if (eq page p) do
              (htm (:li :class "current-page" (str #?"$(p)")))              
            else do
              (htm
               (:li (:a :href #u{uri}?max={max}&page={p} (str #?"$(p)"))))
            end)
         (when (< page pages)
           (let ((next (1+ page)))
             (htm
              (:li (:a :href #u{uri}?max={max}&page={next} (str "Siguiente >")))))))))

(defun render-clients (uri clients)
  (with-html-output-to-string (s)
    (:form :id "post-form" :class "box" :action #uclients :method "POST"
           (str "Agregar equipo para revisión:")
           (:input :type "text" :name "client")
           (:input :type "submit"))
    (:table :id "clients" :cellspacing "0"
            (:caption "Equipos pendientes de verificación")
            (:thead
             (:tr
              (:th "Equipo")
              (:th "Mensaje de estado")
              (:th "Última revisión")
              (:th "Accesible")
              (:th)))
            (:tfoot
             (:tr
              (:td :colspan "5" "Cantidad de clientes: "
                   (str (length clients)))))
            (:tbody
             (dolist (client clients)
               (with-slots (name status-description last-seen reachable) client
                 (htm
                  (:tr
                   (:td (:a :href #u/clients/{name} (str name)))
                   (:td (str status-description))
                   (:td
                    (multiple-value-bind
                          (snd min hr date month year day dl zone)
                        (decode-universal-time last-seen)
                      (fmt "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                           year month date hr min snd)))
                   (:td (str (if reachable "Sí" "No")))))))))))

(defun render-home ()
  (with-html-output-to-string (s)
    (:form :id "search-form" :class "box" :action #uclients
           (str "Buscar por nombre de equipo:")
           (:input :type "search" :name "client")
           (:input :type "submit"))))

(defun render-fixes (uri client fixes)
  (with-html-output-to-string (s)
    (:ul :id "client-menu"
     (:li (:a :href #u{client}/mark-obsolete (str "Marcar como obsoleto")))
     (:li (:a :href #u{client}/mark-fixed (str "Marcar como arreglado"))))
    (:table :id "fixes" :cellspacing "0"
            (:caption  (str #?"Reparaciones para el equipo $(client)"))
            (:tbody
             (dolist (fix fixes)
               (with-slots (date-fixed) fix
                 (htm
                  (:tr
                   (:td
                    (multiple-value-bind
                          (snd min hr date month year day dl zone)
                        (decode-universal-time date-fixed)
                      (fmt "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                           year month date hr min snd)))))))))))

(defun render-all-fixes (uri fixes max page pages)
  (with-html-output-to-string (s)
    (:table :id "fixes" :cellspacing "0"
            (:caption "Todas las reparaciones")
            (:thead
             (:tr
              (:th "Equipo")
              (:th "Fecha de reparación")))
            (:tbody
             (dolist (fix fixes)
               (with-slots (client date-fixed) fix
                 (htm (:tr
                       (:td  (:a :href #u/clients/{client} (str client)))
                       (:td (multiple-value-bind
                                  (snd min hr date month year day dl zone)
                                (decode-universal-time date-fixed)
                              (fmt "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month date hr min snd)))))))))
    (str (render-paginator uri max page pages))))

(defun render-stats ()
  (with-html-output-to-string (s)))

(defun render-queue (uri queue)
  (with-html-output-to-string (s)
    (:table :id "queue" :cellspacing "0"
            (:caption "Equipos en cola pendiente de procesamiento")
            (:thead
             (:tr
              (:th "Equipo")))
            (:tbody
             (dolist (client-name queue)
               (htm (:tr
                     (:td (str client-name)))))))))

(defun render-page (parts)
  (with-html-output-to-string (s nil :prologue t)
    (:html
     (:header
      (:title "Taλos")
      (:meta :http-equiv "content-type" :content "text/html;charset=UTF-8")
      (:meta :http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1")
      (:meta :http-equiv "refresh" :content "30")
      (:link :rel "stylesheet" :type "text/css" :href "/static/talos.css"))
     (:body
      (:ul :id "header"
           (:li (fmt "Taλos v~a"
                        (asdf:component-version (asdf:find-system 'talos))))
           (:li (:a :href "/" "Inicio"))
           (:li (:a :href "/clients" "Equipos pendientes de verificación"))
           (:li (:a :href "/fixes" "Listado de reparaciones"))
           (:li (:a :href "/queue" "Equipos en cola"))
           (:li (:a :href "/stats" "Estadísticas")))
      (:div :id "content"
            (fmt "~{~A~}" (mapcar #'funcall parts)))))))
