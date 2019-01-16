(defsystem "mini-djula"
  :name "Mini Djula"
  :author "Javier Olaechea <pirata@gmail.com>"
  :license "GPLv3+"
  :pathname "src/"
  :depends-on (#:alexandria
               #:string-case)
  :components ((:file "ast")
               (:file "mpc")
               (:file "parser" :depends-on ("mpc"
                                            "ast"))
               (:file "interpreter" :depends-on ("ast"
                                                 "parser")))
  :in-order-to ((test-op (test-op "mini-djula/tests"))))


(defsystem "mini-djula/tests"
  :name "mini-Djula Tests"
  :author "Javier Olaechea"
  :license "GPLv3+"
  :defsystem-depends-on (#:prove-asdf)
  :pathname "tests/"
  :depends-on (#:mini-djula
               #:prove)
  :components ((:test-file "parser")
               (:test-file "smoke"))
  :perform (test-op (o c)
                    (uiop:symbol-call :prove 'run (asdf:system-relative-pathname "mini-djula" "tests/parser.lisp"))
                    (uiop:symbol-call :prove 'run (asdf:system-relative-pathname "mini-djula" "tests/smoke.lisp"))))
