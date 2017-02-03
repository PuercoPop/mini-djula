(defpackage "TESTS/PARSER-COMBINATORS"
  (:use "CL"
        "PROVE"
        "PARSER-COMBINATORS"))
(in-package "TESTS/PARSER-COMBINATORS")

(setf prove:*enable-colors* t)

(let ((parser (singleton (constantly :ok) (lambda (x) (char= x #\{)))))
  (is-values (funcall parser (list #\{))
             '(t :ok nil))
  (is-values (funcall parser (list #\b #\{))
             '(nil nil (#\b #\{))))

