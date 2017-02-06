(defpackage "AST"
  (:use "CL")
  (:shadow "VARIABLE")
  (:export
   #:make-variable
   #:variable
   #:make-if-block
   #:variable-name
   #:text-node
   #:make-text-node
   #:text-node-value))
(in-package "AST")

(defclass ast-node ()
  ())

(defclass text-node (ast-node)
  ((value :initarg :value :reader text-node-value)))

(defmethod print-object ((obj text-node) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (text-node-value obj))))

(defun make-text-node (text)
  (make-instance 'text-node :value text))

(defparameter +unbound-variable+ (gensym "UNBOUND-VARIABLE")
  "Designator for unbound variables")

(defclass variable (ast-node)
  ((name :initarg :name
         :reader variable-name
         :type symbol :documentation "A symbol denoting the variable name.")
   (value :initarg :value
          :initform +unbound-variable+ ; TODO better to leave the slot unbound?
          :reader value
          :documentation "The value bound to the variable.")
   (filters :initarg :filters
            :initform nil
            :documentation "A list of filters to apply.")))

(defmethod print-object ((obj variable) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (variable-name obj))))

(defun make-variable (name)
  (let ((name-keyword (alexandria:make-keyword (string-upcase (coerce name 'string)))))
    (make-instance 'variable :name name-keyword)))

(defclass filter (ast-node)
  ((filter-name :initarg :filter
                :reader filter-name
                :documentation "The filter name to be looked in the filter
                namespace to resolve to a function.")))

(defclass if-block (ast-node)
  ((test :initarg :test :reader test :documentation "The test case.")
   (then-block :initarg :then :reader then :documentation "The block to use when the test is true.")
   (else-block :initarg :else :reader else :documentation "The block to use when the test is false."))
  (:documentation "Represents an if block"))

(defun make-if-block (test-expression then-block &optional else-block)
  (declare (ignore else-block))
  (make-instance 'if-block :test test-expression
                           :then then-block))

(defclass cycle (ast-node)
  ((elements :initarg :elements
             :reader elements
             :documentation "The elements to cycle.")))

;; Maybe not for
(defclass for-loop (ast-node)
  ())

(defclass verbatim-string (ast-node)
  ((value :initarg :value :reader value)))
