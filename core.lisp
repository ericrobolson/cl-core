;; A collection of useful methods for Common Lisp.


(define-condition missing-match (error)
  ((missing-match :initarg :missing-match :initform nil :reader missing-match))
  (:report
   (lambda (condition stream)
     (format stream
            "Unhandled match for input ~a"
             (missing-match condition))))
  (:documentation "A missing match clause was hit"))



(defmacro match (input &rest options)
  "Performs a match on the input. Example:
(defvar x 3)
(match x
  (3 (princ 'It works!'))
  (4 (princ 'Never matches')))"
  `(cond
     ,@(append
         (mapcar
          (lambda (x)
            (list
             (list 'equal input (car x))
             (cadr x)))
          options)
         (list
          (list 't `(error 'missing-match :missing-match ,input))))))
