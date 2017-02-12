;; Correr parser tests y smoke tests
(in-package #:asdf-user)

(defsystem "mini-djula-tests"
  :name "mini-djula-tests"
  :author "Javier Olaechea"
  :license "GPLv3+"
  :pathname "tests/"
  :depends-on (#:mini-djula
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:test-file "parser")
               (:test-file "smoke")))
