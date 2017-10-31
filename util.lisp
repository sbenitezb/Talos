(in-package #:talos)

(defun partition-if (f seq)
    "Given a predicate F, partition SEQ into two sublists, the first
of which has elements that satisfy F, the second which do not."
    (let ((yes nil)
          (no nil))
      (map nil
           #'(lambda (x)
               (if (funcall f x)
                   (push x yes)
                   (push x no)))
           seq)
      (values yes no)))
  
  (defun partition-if-not (f seq)
    "Partition SEQ into two sublists, the first whose elements do not
satisfy the predicate F, and the second whose elements do."
    (multiple-value-bind (yes no)
        (partition-if f seq)
      (values no yes)))
