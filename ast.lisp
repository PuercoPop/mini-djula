(defpackage "AST"
  (:use "CL")
  (:shadow "VARIABLE")
  (:export
   #:make-variable
   #:variable
   #:variable-name

   #:text-node
   #:make-text-node
   #:text-node-value

   #:if-block
   #:make-if-block
   #:then-block
   #:test-block

   #:comparison
   #:make-comparison
   #:comparison-left
   #:comparison-right

   #:literal
   #:make-literal
   #:literal-value))
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

(defclass variable (ast-node)
  ((name :initarg :name
         :reader variable-name
         :type symbol :documentation "A symbol denoting the variable name.") 
   (filters :initarg :filters
            :initform nil
            :documentation "A list of filters to apply.")))

(defmethod print-object ((obj variable) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (variable-name obj))))

(defun make-variable (name)
  (let ((name-keyword (alexandria:make-keyword (string-upcase (coerce name 'string)))))
    (make-instance 'variable :name name-keyword)))

(defclass literal (ast-node)
  ((value :initarg :value :reader literal-value)))

(defmethod print-object ((obj literal) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (literal-value obj))))

(defun make-literal (value)
  (make-instance 'literal :value value))

(defclass filter (ast-node)
  ((filter-name :initarg :filter
                :reader filter-name
                :documentation "The filter name to be looked in the filter
                namespace to resolve to a function.")))

(defclass if-block (ast-node)
  ((test :initarg :test :reader test-block :documentation "The test case.")
   (then-block :initarg :then :reader then-block :documentation "The block to use when the test is true.")
   (else-block :initarg :else :reader else :documentation "The block to use when the test is false."))
  (:documentation "Represents an if block"))

(defun make-if-block (test-expression then-block &optional else-block)
  (declare (ignore else-block))
  (make-instance 'if-block :test test-expression
                           :then then-block))

(defclass comparison ()
  ((left :initarg :left :reader comparison-left)
   (right :initarg :right :reader comparison-right))
  (:documentation "Compare if VARIABLE is EQUALP to LITERAL."))

(defun make-comparison (variable literal)
  (make-instance 'comparison :left variable
                             :right literal))

(defclass cycle (ast-node)
  ((elements :initarg :elements
             :reader elements
             :documentation "The elements to cycle.")))

;; Maybe not for
(defclass for-loop (ast-node)
  ())

(defclass verbatim-string (ast-node)
  ((value :initarg :value :reader value)))
