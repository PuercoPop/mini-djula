;; Clean Implementation
(defpackage #:mpc
  (:use #:cl
        #:alexandria)
  (:shadow)
  (:export
   ;; Primitive Parsers
   #:.return
   #:.fail
   #:.anything
   ;; Combinators
   #:parser-bind
   #:parser-let*
   ;; Parsers
   #:.char=
   #:.not
   #:.any
   #:.many
   #:.progn
   #:.prog1
   #:.prog2
   #:.and
   #:.whitespace
   #:.word
   #:.is
   #:.string-equal
   #:.plus
   #:.eof))
(in-package #:mpc)


;; Primitive Parsers

(defun .return (value)
  (lambda (tokens)
    (list (cons value tokens))))

(defun .fail ()
  (lambda (tokens)
    (declare (ignore tokens))
    nil))

(defun .anything ()
  (lambda (tokens)
    (if (null tokens)
        nil
        (list (cons (car tokens)
                    (cdr tokens))))))


;; Parser combinators

(defun parser-bind (parser function)
  (lambda (tokens)
    (loop :for (value . tokens) :in (funcall parser tokens)
          :append (funcall (funcall function value) tokens))))

(defmacro parser-let* (bindings &body body)
  (if bindings
      (let ((symbol (caar bindings))) ; caar => (first (first ...))
        `(parser-bind ,@(cdr (first bindings))
                (lambda (,symbol) 
                  (declare (ignorable ,symbol))
                  (parser-let* ,(cdr bindings)
                    ,@body))))
      `(progn ,@body)))


;; Parsers

(defun .satisfies (predicate)
  (parser-bind (.anything)
               (lambda (x)
                 (if (funcall predicate x)
                     (.return x)
                     (.fail)))))

(defun .char= (char)
  (.satisfies (lambda (c) (char= c char))))

(defun .first (parser)
  "Discard all but the first result of the parser"
  (lambda (tokens)
    (alexandria:when-let ((results (funcall parser tokens)))
      (list (first results)))))

(defun .plus (parser-1 parser-2)
  "Non-deterministic choice"
  (lambda (tokens)
    (append (funcall parser-1 tokens)
            (funcall parser-2 tokens))))

(defun .or (&rest parsers)
  (lambda (tokens)
    (loop :for parser :in parsers
          :for result := (funcall parser tokens)
          :when result
            :do (return result))))

(defun .and (parser-1 &rest parsers)
  (parser-let* ((result parser-1))
    (if parsers
        (apply #'.and parsers)
        (.return result))))

(defun .not (parser)
  (lambda (tokens)
    (let ((result (funcall parser tokens)))
      (if result
          nil
          (list (cons t tokens))))))


;; Recursion, not chevere
(defun .any (parser)
  "Match a parser any number of times. AKA ZERO-OR-MORE."
  (.plus (parser-let* ((x parser)
                       (xs (.any parser)))
           (.return (cons x xs)))
       (.return nil)))

(defmacro .progn (&rest parsers)
  (if (rest parsers)
      (let ((name (gensym)))
        `(parser-let* ((,name ,(first parsers)))
           (.progn ,@(rest parsers))))
      (first parsers)))

(defmacro .prog1 (parser &rest parsers)
  (with-unique-names (name ignore)
    `(parser-let* ((,name ,parser)
                   (,ignore (.progn ,@parsers)))
                  (.return ,name))))

(defmacro .prog2 (parser1 parser2 &rest parsers)
  (with-unique-names  (name ignore)
    `(parser-let* ((,ignore ,parser1)
             (,name ,parser2)
             (,ignore (.progn ,@parsers)))
       (.return ,name))))

;; (defun .any (parser)
;;   "Match a parser any number of times. AKA ZERO-OR-MORE."
;;   (lambda (tokens)
;;     (loop :with values := nil
;;           :for result := (funcall parser tokens)
;;           :if result
;;             :do (setf tokens (cdar result))
;;                 (push (caar result) values)
;;           :else
;;             :return (list (cons (reverse values)
;;                                 tokens)))))

(defun .many (parser)
  "AKA ONE-OR-MORE"
  (parser-let* ((x parser)
                (y (.any parser)))
    (.return (cons x y)))
  ;; No obiene el nuevo token
  ;; (lambda (tokens)
  ;;   (loop :for result := (funcall parser tokens)
  ;;         :until (null result)
  ;;         :append result))
  )

(defun .lower-case-char ()
  (.satisfies #'sb-unicode:lowercase-p))

(defun .upper-case-char ()
  (.satisfies #'sb-unicode:uppercase-p))

(defun .punctuation ()
  (.plus (.char= #\-)
         (.char= #\_)))

(defun .letter ()
  (.plus (.plus (.lower-case-char)
                (.upper-case-char))
         (.punctuation)))

(defun .word ()
  (.many (.letter)))

(defun .char-equal (char)
  (.satisfies (lambda (c)
                (char-equal char c))))

(defun .string-equal (string)
  (if (string= string "")
      (.return nil)
      (parser-let* ((_ (.char-equal (aref string 0)))
                    (_ (.string-equal (subseq string 1))))
        (.return string))))

(defun .whitespace ()
  (.satisfies #'sb-unicode:whitespace-p))

(defun .endp ()
  (.not (.anything)))

(defun .no-more-input ()
  (.not (.anything)))

(defun .eof ()
  (parser-let* ((_ (.no-more-input)))
    (.return nil)))
