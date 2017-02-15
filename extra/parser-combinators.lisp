(defpackage "PARSER-COMBINATORS"
  (:nicknames "PC")
  (:use "CL"
        "ALEXANDRIA")
  (:export "DEFINE-PARSER"
           "PARSE"
           "PARSE-1"
           "SINGLETON"
           "CONSECUTIVE"
           "ALTERNATIVE"
           "REPEAT*"
           "REPEAT+"
           "OPTIONAL")
  (:documentation "

Parser Combinators, Parsers that compose
==================

= What is Parser? (akka Parser interface) =

A parser takes a a sequence of input and returns a success flag, the remaining
input and a value to collect.

= Example =

(ql:quickload :sicl-loop-support)
(in-package #:sicl-loop)

(consecutive 'list
             (singleton 'identity (lambda (c) (char= #\A c)))
             (singleton 'identity (lambda (c) (char= #\B c))))
"))
(in-package "PARSER-COMBINATORS")

;; XXX: Should we name the token variable?
(defmacro define-parser (name (&rest args) &body body)
  ;; Parse-body
  `(defun ,name ,args
     (lambda (tokens) (progn ,@body))))

;; Invoca el PREDICATE con el primer token. Si retorna con exito entonces se
;; aplicar TRANSFORMER y consume un token.
(define-parser singleton (transformer predicate)
  (if (and (not (null tokens))
           (funcall predicate (car tokens)))
      (values t (funcall transformer (car tokens)) (cdr tokens))
      (values nil nil tokens)))

;; Ejecuta todos los PARSERS de uno tras otro. Si un parser falla todo el
;; parser definidio por CONSECUTIVE falla también. Si todos los PARSERS
;; retornan con exito entonces se invoca la función COMBINER con todos los
;; tokens comsumidos como argumentos.
(define-parser consecutive (combiner &rest parsers)
  (let ((result (loop :for remaining-tokens := tokens :then (cdr remaining-tokens)
                      :for parser :in parsers
                      :for result := (funcall parser (car tokens))
                      :if result
                        :collect result
                      :else
                        :do (return nil))))
    (if result
        (values t
                (apply combiner result)
                (cdr  tokens))
        (values nil nil tokens))))

;; Invoca los PARSERS hasta encontrar el primero que retorne con exito. Si
;; ninguno retorna con exito entonces el parser definidio por ALTERNATIVE
;; falla.
(define-parser alternative (&rest parsers)
  (loop :for parser :in parsers
        :for (successp result remaining-tokens) := (multiple-value-list (funcall parser tokens))
        :when successp
          :do (return (values t result remaining-tokens))
        :finally (return (values nil nil tokens))))

;; Invoca el PARSER repetidamente hasta que falle. Cada vez consumiendo un
;; nuevo token. Luego invoca COMBINER con los valores retornados por las
;; sucesivas invocaciones de PARSER. En el caso que la primera invocación falle
;; entonces se invoca COMBINER con NIL como argumento.
(define-parser repeat* (combiner parser)
  (let* ((remaning-tokens nil)
         (results
           (loop :for (successp result rest)
                   := (multiple-value-list (funcall parser tokens)) :then
                                                                    (multiple-value-list (funcall parser tokens))
                 :while successp
                 :do (setf remaning-tokens rest)
                 :collect result)))
    (values t (funcall combiner results) remaning-tokens)))

;; Invoca el PARSER repetidamente hasta que falle. Cada vez consumiendo un
;; nuevo token. Luego invoca COMBINER con los valores retornados por las
;; sucesivas invocaciones de PARSER. En el caso que la primera invocación falle
;; el parser definido por repeat+ falla tambien..
(define-parser repeat+ (combiner parser)
  (let* ((remaning-tokens nil)
         (at-least-once nil)
         (results
           (loop :for (successp result rest)
                   := (multiple-value-list (funcall parser tokens)) :then
                                                                    (multiple-value-list (funcall parser tokens))
                 :while successp
                 :do (setf remaning-tokens rest
                           at-least-once t)
                 :collect result)))
    (values t (funcall combiner results) remaning-tokens)))


;; Invoca el PARSER, si retorna con exito el parser definido por OPTIONAL
;; retorna esos valores. En caso que falle retorna los el valor por DEFAULT, no
;; consumiendo ningun token.
(define-parser optional (default parser)
  (multiple-value-bind (successp result remaning-tokens)
      (funcall parser tokens)
    (if successp
        (values successp result remaning-tokens)
        (values t default tokens))))

(define-parser and-pc (&rest parsers))

#+(or)
(define-token-parser (token?))
