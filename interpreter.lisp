(defpackage "INTERPRETER"
  (:use "CL")
  (:import-from #:parser
                #:parse)
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
  (alexandria:if-let ((value (cdr (assoc key ctx))))
    value
    (error "Key ~A not found in context, ~A." key ctx)))

(defmethod %render ((variable ast:variable) context stream)
  (format stream "~A" (lookup-variable (ast:variable-name variable) context)))

(defmethod %render ((if-block ast:if-block) context stream)
  (%render (ast:test-block if-block) context stream)
  (%render (ast:then-block if-block) context stream))
