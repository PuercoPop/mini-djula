(defpackage "PARSER/TESTS"
  (:use "CL"
        "PROVE"))
(in-package "PARSER/TESTS")

(defun parse (parser input)
  (funcall parser (coerce input 'list)))

(setf prove:*enable-colors* t)

(plan 21)

(ok (funcall (parser::.comment-start)
             (list #\{ #\#)))
(ok (funcall (parser::.comment-end)
             (list #\# #\})))

(ok (parse (parser:.comment) "{# #}"))
(ok (parse (parser:.comment) "{# ##}"))
(ok (parse (parser:.comment) "{## ##}"))
(ok (parse (parser:.comment) "{# foo # faf #}"))
(is (parse (parser:.comment) "{# foo")
    nil)

(ok (parse (parser:.variable) "{{ foo }}"))
(ok (parse (parser:.variable) "{{foo }}"))
(ok (parse (parser:.variable) "{{ foo}}"))
(ok (parse (parser:.variable) "{{foo}}"))
(is (parse (parser:.variable) "{{ foo } }")
    nil)


(ok (parse (parser::.if-block-start) "{% if foo %}"))


(ok (parse (parser::.if-block-end) "{% endif %}"))
(ok (parse (parser::.if-block-end) "{% endif%}"))
(ok (parse (parser::.if-block-end) "{%endif %}"))
(ok (parse (parser::.if-block-end) "{%endif%}"))

(ok (parse (parser::.string-literal) "\"HAI\""))
(ok (parse (parser::.equality-comparison) "foo == \"HAI\""))
(ok (parse (parser::.if-block-start) "{% if foo == HAI %}"))

(ok (parse (parser:.if-block) "{% if foo %} hai {% endif %}"))

(finalize)
