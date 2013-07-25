;;;; lazy-bone.asd


(asdf:defsystem #:lazy-bone
  :serial t
  :depends-on (#:basicl
               #:hunchentoot
               #:html-template
               #:parenscript
               #:jsown
               #:alexandria)
  :components ((:file "lisp/package")
               (:file "lisp/template")
               (:file "lisp/sugar")
               (:file "lisp/core")
               (:file "lisp/graph")
               (:file "lisp/skeleton")))


