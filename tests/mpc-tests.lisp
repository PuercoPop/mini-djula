(defpackage #:mpc/tests
  (:use #:cl
        #:mpc
        #:prove))

(in-package #:mpc/tests)

(setf prove:*enable-colors* t)

(defun parse (parser input)
  (funcall parser (coerce input 'list)))

(plan nil)

(ok (parse (.char= #\a) "a"))
(ok (not (parse (.char= #\a) "b")))

(diag "Testing .NOT")
(is (parse (mpc:.not (mpc:.char= #\a)) "b")
    '((T #\b)))
(ok (not (parse (mpc:.not (mpc:.char= #\a)) "a")))

(diag ".ANY")

(ok (parse (.any (.char= #\a)) "a"))
(ok (parse (.any (.char= #\a))  "b"))
;; TODO check the input

(diag "Testing .MANY")

(ok (not (parse (.many (.char= #\a))
                "BBB")))
(ok (parse (.many (.char= #\a))
                "aaaB"))
;; (is )

(ok (parse (.parse-comment)
           "{# #}"))


(ok (parse (.string-equal "HaI") "hai"))


(finalize)
