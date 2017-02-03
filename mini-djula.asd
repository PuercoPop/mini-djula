(in-package #:asdf-user)

(defsystem "mini-djula"
  :name "Mini Djula"
  :author "Javier Olaechea"
  :license "GPLv3+"
  :depends-on (#:alexandria
               #:string-case)
  :components ((:file "lexer")
               (:file "ast")
               (:file "parser-combinators")
               (:file "mpc")
               (:file "parser" :depends-on("parser-combinators"
                                           "ast"
                                           "lexer"))
               (:file "interpreter" :depends-on ("ast"))))
