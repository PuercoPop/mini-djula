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
               (:file "parser" :depends-on("parser-combinators"
                                           "ast"))
               (:file "interpreter" :depends-on ("ast"))))
