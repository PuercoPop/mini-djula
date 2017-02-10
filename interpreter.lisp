(defpackage "INTERPRETER"
  (:use "CL")
  (:import-from #:parser
                #:parse)
  (:shadow #:eval)
  (:export "RENDER"))
(in-package "INTERPRETER")

(defun read-file (path)
  (with-open-file (in path)
    (loop :for char := (read-char in nil :end-of-file)
          :until (eq char :end-of-file)
          :collect char)))

(defgeneric %render (ast context stream)
  (:documentation ""))

(defun render (template-path context &optional (stream *standard-output*))
  (%render (caar (funcall (parse) (read-file template-path)))
           ; (parse (read-file template-path))
           context
           stream))

(defmethod %render ((nodes list) context stream)
  (dolist (node nodes)
    (%render node context stream)))

(defmethod %render ((text ast:text-node) context stream)
  (format stream "~A" (ast:text-node-value text)))

(defun lookup-variable (key ctx)
  (cdr (assoc key ctx)))

#+strict-version
(defun lookup-variable (key ctx)
  (alexandria:if-let ((value (cdr (assoc key ctx))))
    value
    (error "Key ~A not found in context, ~A." key ctx)))

(defmethod %render ((variable ast:variable) context stream)
  (format stream "~A" (lookup-variable (ast:variable-name variable) context)))

(defmethod %render ((if-block ast:if-block) context stream)
  (when (eval (ast:test-block if-block) context)
    (%render (ast:then-block if-block) context stream)))

(defgeneric eval (node context)
  (:documentation "Evaluate the NODE using CONTEXT to resolve any bindings."))

(defmethod eval ((node ast:variable) context)
  (lookup-variable (ast:variable-name node)
                   context))

(defmethod eval ((node ast:literal) context)
  (declare (ignore context))
  (ast:literal-value node))

(defmethod eval ((node ast:comparison) context)
  (equalp (eval (ast:comparison-left node))
          (eval (ast:comparison-right node))))
