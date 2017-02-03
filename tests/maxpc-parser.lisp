;; Test parser made with maxpc
(in-package :maxpc-parser)

(parse "{# asda # #}" (?comment))
(parse "{{ foo }}" (?variable))
(parse "{{foo }}" (?variable))
(parse "{{ foo}}" (?variable))
(parse "{{foo}}" (?variable))
