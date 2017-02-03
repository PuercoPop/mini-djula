(defpackage "PARSER"
  (:use "CL")
  (:export ;; "*GRAMMAR*"
           ;; "PARSE"
   
   #:.comment
   #:.variable
   #:.if-block))
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

;; TODO: Understand why this doesn't work
;; (defun .parse-comment ()
;;   (mpc:parser-let*
;;       ((_ (.comment-start)) 
;;        (_ (mpc:.any (mpc:.not (.comment-end))))
;;        (_ (.comment-end)))
;;     (mpc:.return :comment-ast)))


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
    (mpc:.return (ast:make-variable (coerce variable-name 'string)))))

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
       (variable (mpc:.word))
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
    (consequent (mpc:.any (mpc:.anything)))
    (_ (.if-block-end)))
   (mpc:.return (ast:make-if-block expression consequent))))

(defun parse (in))
