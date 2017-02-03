(defpackage "PARSER-COMBINATORS/EXAMPLES"
  (:use "CL"
        "PARSER-COMBINATORS"))
(in-package "PARSER-COMBINATORS/EXAMPLES")

#+(or)
(define parse-comment-start ()
  (cosecutive (singleton #\{)
              (singleton #\#)))

#+(or)
(parse-comment-start (lex "#{"))  ; => t 
#+(or)
(parse-comment-start (lex "foo"))

(defvar *comment-start*
  (singleton (constantly :ok) (lambda (x) (char= x #\{))))

(funcall *comment-start* (list #\{)) ; => T, :OK, NIL
(funcall *comment-start* (list #\b #\{)) ; => NIL, NIL, (#\b #\{)
