;;;; package.lisp
;;;; Package definition for lazy-bone

(defpackage #:breakds.lazy-bone
  (:nicknames #:lazy-bone)
  (:use #:cl
	#:hunchentoot
        #:breakds.basicl.exmac)
  (:import-from #:parenscript #:ps* #:ps)
  (:export #:*acceptor*
           #:def-view
           #:make-view
           #:view-add-method
           #:*global-namespace*))
           





