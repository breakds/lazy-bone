;;;; package.lisp
;;;; Package definition for lazy-bone

(defpackage #:breakds.lazy-bone
  (:nicknames #:lazy-bone)
  (:use #:cl
	#:hunchentoot
        #:breakds.basicl.exmac)
  (:import-from #:parenscript #:ps* #:ps #:create
                #:chain)
  (:export *namespace*
           bone
           make-bone
           bone-name
           bone-base
           bone-members
           def-model
           def-view
           compile-obj
           compile-bone))

;; (defpackage #:breakds.lazy-bone-example
;;   (:nicknames #:lazy-bone-example)
;;   (:use #:cl
;;         #:hunchentoot
;;         #:breakds.lazy-bone)
;;   (:export #:boot
;;            #:shutdown))

           





