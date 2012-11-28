;;;; package.lisp
;;;; Package definition for lazy-bone

(defpackage #:breakds.lazy-bone
  (:nicknames #:lazy-bone)
  (:use #:cl
	#:hunchentoot
        #:breakds.basicl.exmac)
  (:import-from #:parenscript #:ps* #:ps #:create
                #:chain)
  (:export *models*
           model
           model-name
           def-model
           compile-model
           def-view
           compile-view))

;; (defpackage #:breakds.lazy-bone-example
;;   (:nicknames #:lazy-bone-example)
;;   (:use #:cl
;;         #:hunchentoot
;;         #:breakds.lazy-bone)
;;   (:export #:boot
;;            #:shutdown))

           





