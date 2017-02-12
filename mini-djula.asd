(in-package #:asdf-user)

(defsystem "mini-djula"
  :name "Mini Djula"
  :author "Javier Olaechea"
  :license "GPLv3+"
  :depends-on (#:alexandria
               #:string-case)
  :components ((:static-file "lexer")
               (:file "ast")
               (:static-file "parser-combinators")
               (:file "mpc")
               (:file "parser" :depends-on ("mpc"
                                            "ast"))
               (:file "interpreter" :depends-on ("ast"
                                                 "parser")))
  :in-order-to ((asdf:test-op (asdf:load-op :mini-djula-tests)))
  :perform (asdf:test-op (o c)
                         (uiop/package:symbol-call :prove 'run (asdf:system-relative-pathname :mini-djula "tests/parser.lisp"))
                         (uiop/package:symbol-call :prove 'run (asdf:system-relative-pathname :mini-djula "tests/smoke.lisp"))))
