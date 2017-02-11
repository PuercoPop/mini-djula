(defpackage "MINI-DJULA/SMOKE-TESTS"
  (:use #:cl
        #:prove))
(in-package "MINI-DJULA/SMOKE-TESTS")

(setf prove:*enable-colors* t)

(plan 0)
;; Assertion should include template name context and outputfile

(defmacro with-template-names (pairs &body body)
  (if pairs
      `(let ((,(caar pairs) (merge-pathnames ,(cadar pairs)
                                             (asdf:system-relative-pathname :mini-djula "tests/templates/"))))
         (with-template-names ,(cdr pairs) ,@body))
      `(progn ,@body)))

(defun verify (&key template with-context render-as)
  (with-template-names ((in-path template)
                        (expected-path render-as))
    (is (with-output-to-string (out)
          (interpreter:render in-path with-context out))
        (alexandria:read-file-into-string expected-path))))

(verify :template "1.djhtml"
        :with-context '((:bar . "baz"))
        :render-as "1.html")

(verify :template "2.djhtml"
        :with-context '((:foo . "baz")
                        (:obligatory-message . "I should be shown"))
        :render-as "2-a.html")

(verify :template "2.djhtml"
        :with-context '((:foo . nil)
                        (:obligatory-message . " I won't be displayed"))
        :render-as "2-b.html")

(verify :template "3.djhtml"
        :with-context '((:foo . "FU")
                        (:obligatory-message . "BAZ"))
        :render-as "3-a.html")

(verify :template "3.djhtml"
        :with-context '((:foo . nil)
                        :obligatory-message . " I won't be displayed")
        :render-as "3-b.html"))


(finalize)
