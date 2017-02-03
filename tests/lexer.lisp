(defpackage "LEXER/TESTS"
  (:use "CL"
        "LEXER"
        "PROVE"))
(in-package "LEXER/TESTS")

(setf *enable-colors* t)

(plan 3)

(ok (lex #P"examples/1.djhtml"))
(ok (with-input-from-string (in "hai {# foo #}")
      (lex in)))
(ok (lex "hai {# foo #}"))

(finalize)
