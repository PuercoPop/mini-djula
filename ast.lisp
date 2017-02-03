(defpackage "AST"
  (:use "CL")
  (:shadow "VARIABLE")
  (:export
   #:make-variable
   #:variable
   #:make-if-block))
(in-package "AST")

(defclass ast-node ()
  ())

(defparameter +unbound-variable+ (gensym "UNBOUND-VARIABLE")
  "Designator for unbound variables")

(defclass variable (ast-node)
  ((name :initarg :name
         :reader name
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
    (format stream "~A" (name obj))))

(defun make-variable (name)
  (make-instance 'variable :name name))

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
