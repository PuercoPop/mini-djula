(defpackage "PARSER"
  (:use "CL")
  (:export #:parse
           #:.comment
           #:.variable
           #:.if-block
           #:.text))
(in-package "PARSER")

 ;; First a { token Then a # then any number of characters then a # token followed by a } token. Returns nil
(defun .comment-start ()
  (mpc:parser-let*
      ((_ (mpc:.char= #\{))
       (_ (mpc:.char= #\#)))
    (mpc:.return t)))

(defun .comment-end ()
  (mpc:parser-let*
      ((_ (mpc:.char= #\#))
       (_ (mpc:.char= #\})))
    (mpc:.return t)))


(defun .comment ()
  (mpc:.prog2 (.comment-start)
              (mpc:.any (mpc:.anything))
              (.comment-end)))

#+Si-quisiera-retornar-un-nodo
(defun .parse-comment ()
  (mpc:parser-let* ((result (mpc:.prog2 (.comment-start)
                                    (mpc:.any (mpc:.anything))
                                    (.comment-end))))
    (mpc:.return (ast:make-comment (coerce result 'string)))))

(defun .variable-start ()
  (mpc:parser-let*
      ((_ (mpc:.char= #\{))
       (_ (mpc:.char= #\{)))
    (mpc:.return t)))

(defun .variable-end ()
  (mpc:parser-let*
      ((_ (mpc:.char= #\}))
       (_ (mpc:.char= #\})))
    (mpc:.return t)))

(defun .variable ()
  (mpc:parser-let*
      ((_ (.variable-start))
       (_ (mpc:.any (mpc:.whitespace)))
       (variable-name (mpc:.word))
       (_ (mpc:.any (mpc:.whitespace)))
       (_ (.variable-end)))
    (mpc:.return (ast:make-variable variable-name))))

(defun .block-start ()
  (mpc:parser-let*
      ((_ (mpc:.char= #\{))
       (_ (mpc:.char= #\%)))
    (mpc:.return t)))

(defun .block-end ()
  (mpc:parser-let*
      ((_ (mpc:.char= #\%))
       (_ (mpc:.char= #\})))
    (mpc:.return t)))

(defun .if-block-start ()
  (mpc:parser-let*
      ((_ (.block-start))
       (_ (mpc:.any (mpc:.whitespace)))
       (_ (mpc:.string-equal "if"))
       (_ (mpc:.any (mpc:.whitespace)))
       (variable (.predicate))
       (_ (mpc:.any (mpc:.whitespace)))
       (_ (.block-end)))
    (mpc:.return variable)))

(defun .if-block-end ()
  (mpc:parser-let*
      ((_ (.block-start))
       (_ (mpc:.any (mpc:.whitespace)))
       (_ (mpc:.string-equal "endif"))
       (_ (mpc:.any (mpc:.whitespace)))
       (_ (.block-end)))
    (mpc:.return t)))

(defun .if-block ()
  (mpc:parser-let*
   ((expression (.if-block-start))
    (consequent (mpc:.any (.top-level)))
    (_ (.if-block-end)))
   (mpc:.return (ast:make-if-block expression consequent))))

(defun .variable-word ()
  (mpc:parser-let* ((variable (mpc:.word)))
    (mpc:.return (ast:make-variable variable))))

(defun .string-literal ()
  (mpc:parser-let* ((_ (mpc:.char= #\"))
                    (string (mpc:.word))
                    (_ (mpc:.char= #\")))
    (mpc:.return (ast:make-literal (coerce string 'string)))))

(defun .var-or-val ()
  "Variable or value."
  ;; XXX: Order matters
  (mpc:.plus (.string-literal)
             (.variable-word)))

(defun .equal-sign ()
  (mpc:parser-let* ((_ (mpc:.char= #\=))
                    (_ (mpc:.char= #\=)))
    (mpc:.return :=)))

(defun .equality-comparison ()
  (mpc:parser-let* ((left (.var-or-val))
                    (_ (mpc:.any (mpc:.whitespace)))
                    (_ (.equal-sign))
                    (_ (mpc:.any (mpc:.whitespace)))
                    (right (.var-or-val)))
    (mpc:.return (ast:make-comparison left right))))

(defun .boolean-expression ()
  (.equality-comparison))

(defun .predicate ()
  (mpc:.plus (.variable-word)
             (.boolean-expression)))

(defun .text ()
  (mpc:parser-let*
      ((text (mpc:.anything)))
    (mpc:.return (ast:make-text-node (string text)))))

(defun .top-level ()
  (mpc::.or (.comment)
            (.variable)
            (.if-block)
            (.text)))

(defun parse ()
  (mpc:parser-let* ((ast (mpc:.many (.top-level)))
                    (eof (mpc:.eof)))
    (mpc:.return ast)))
