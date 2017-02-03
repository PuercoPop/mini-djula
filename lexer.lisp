(defpackage "LEXER"
  (:use "CL"
        "STRING-CASE")
  (:export "LEX"
           "TOKENP"
           "TOKEN-TYPE"
           "TOKEN-VALUE")
  (:documentation "
Lexer Tokens
============

{
}
%
|
#
Characters <= The only token with value
"))
(in-package "LEXER")


(defclass token ()
  ((token-type :initarg :type
               :reader token-type
               :type (member :{ :} :% :\| :# :char)
               :documentation "")
   (value :initarg :value :reader token-value)))

(defgeneric tokenp (obj)
  (:documentation "Is OBJ a token?"))

(defmethod tokenp (obj)
  nil)
(defmethod tokenp ((obj token))
  t)

(defmethod print-object ((tok token) stream)
  (print-unreadable-object (tok stream :type t)
    (format stream "~A~@[ ~A~]" (token-type tok) (when (slot-boundp tok 'value)
                                                       (token-value tok)))))

(macrolet ((define-token-constructor (name)
             (alexandria:with-gensyms (arguments)
               `(defun ,(alexandria:symbolicate "MAKE-" name "-TOKEN") (&optional value)
                  (let ((,arguments (list :type ,name)))
                    (when value
                      (push value ,arguments)
                      (push :value ,arguments))
                    (apply 'make-instance 'token ,arguments))))))
  (define-token-constructor :{)
  (define-token-constructor :})
  (define-token-constructor :%)
  (define-token-constructor :\|)
  (define-token-constructor :#)
  (define-token-constructor :char))

(defun %lex-stream (in)
    (loop :for char := (read-char in nil :eof)
          :until (eq char :eof)
          :collect (case char 
                     (#\{ (make-{-token))
                     ( #\}(make-}-token))
                     (#\% (make-%-token))
                     (#\| (make-\|-token))
                     (#\# (make-#-token))
                     (t (make-char-token char)))))

(defun lex (in)
  ;; TODO Type case for pathnames. Strings as well? trouble when strings are
  ;; used as pathname designators
  (typecase in
    (stream (%lex-stream in))
    (pathname (with-open-file (in in)
                (%lex-stream in)))
    (string (with-input-from-string (in in)
              (%lex-stream in)))))

#+(or)
(with-open-file (in #P"examples/1.djhtml")
  (lex in))
