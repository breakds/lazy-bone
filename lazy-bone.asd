;;;; lazy-bone.asd


(asdf:defsystem #:lazy-bone
    :serial t
    :depends-on (#:basicl
                 #:hunchentoot
		 #:html-template
		 #:parenscript
                 #:alexandria)
    :components ((:file "lisp/package")
                 (:file "lisp/template")
		 (:file "lisp/core")
                 (:file "lisp/graph")
                 (:file "lisp/skeleton")))

    
