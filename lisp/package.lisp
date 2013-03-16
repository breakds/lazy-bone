;;;; package.lisp
;;;; Package definition for lazy-bone

(defpackage #:breakds.lazy-bone
  (:nicknames #:lazy-bone)
  (:use #:cl
	#:hunchentoot
        #:alexandria)
  (:import-from #:parenscript #:ps* #:ps #:create
                #:chain #:defpsmacro #:new #:getprop #:@ #:for-in)
  (:export #:*global*
           #:read-tmpl
           #:clear-tmpl
           #:set-template-registry
           #:bone
	   #:bone-name
	   #:bone-base
	   #:bone-members
	   #:def-view
	   #:def-model
	   #:def-router
	   #:def-collection
	   #:def-collection-view
           #:access-bone
           #:clear-global
	   #:place-view
	   #:bone-definition
	   #:*lazy-view
	   #:*lazy-collection-view
           #:gen-topological
           #:compile-to-js
           #:define-simple-app
           #:start-server
           #:stop-server
           ;; parenscript macros
	   #:lazy-init
	   #:lazy-init-base
           #:wait-until
	   #:trace
           #:@.
	   #:acquire-args
           #:construct-chain
           #:render-from-model
           #:@get
           ;; for debugging
           #:*acceptor*))
           





