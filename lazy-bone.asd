;;;; lazy-bone.asd


(asdf:defsystem #:lazy-bone
    :serial t
    :depends-on (#:basicl
                 #:hunchentoot
		 #:html-template
		 #:parenscript)
    :components ((:file "lisp/package")
		 (:file "lisp/core")
                 (:file "lisp/graph")
                 (:file "lisp/skeleton")))

    
