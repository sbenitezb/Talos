(in-package #:talos)

(defmacro list-of-things (l thing)
  "Verifica que todos los elementos de la lista L sean del tipo THING."
  `(null (member-if-not #'(lambda (type) (eql (type-of type) ,thing)) ,l)))
