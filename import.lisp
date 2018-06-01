(ql:quickload :drakma :silent t)

 (with-open-file (s #P"C:/Temp/Clients.txt" :direction :input :external-format :ascii)
           (loop for line = (read-line s nil)
                while line
              doing
                (format t "~a~%" line)
                (drakma:http-request "http://127.0.0.1:9090/clients"
                                     :method :post
                                     :form-data t
                                     :parameters (acons "client" line nil))))
 