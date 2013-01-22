;;;; package.lisp
;;;; Package definition for lazy-bone

(defpackage #:breakds.lazy-bone
  (:nicknames #:lazy-bone)
  (:use #:cl
	#:hunchentoot
        #:breakds.basicl.exmac)
  (:import-from #:parenscript #:ps* #:ps #:create
                #:chain #:defpsmacro #:new #:getprop #:@ #:for-in)
  (:export #:*global*
           #:bone
	   #:bone-name
	   #:bone-base
	   #:bone-members
	   #:def-view
	   #:def-model
	   #:def-collection
	   #:def-collection-view
           #:access-bone
           #:clear-global
	   #:place-view
	   #:bone-definition
	   #:lazy-init
           #:wait-until
	   #:acquire-args
	   #:*lazy-view
	   #:*lazy-collection-view
           #:gen-topological
           #:compile-to-js
           #:define-simple-app
           #:start-server
           #:stop-server))
           





