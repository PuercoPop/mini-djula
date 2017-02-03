(defpackage "INTERPRETER"
  (:use "CL")
  (:export "RENDER"))
(in-package "INTERPRETER")

(defun %render (ast context))
(defun render (template-path context))
