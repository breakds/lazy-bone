;;;; skeleton.lisp
;;;; Description: Augmented Backbone.js classes that serve as the base
;;;;              class for lazy-bone objects (will be automatically
;;;;              added to the compiled JavaScript file).
;;;; Author: BreakDS
;;;; Date: Sun Jan 20 08:33:47 CST 2013


(in-package #:breakds.lazy-bone)

;;; base class for view
(def-view *lazy-view
    (('initialize '(lazy-init-base 
                    (setf (@ this bindings) (new (array)))))
     ('bind-to '(lambda (dispatcher event callback)
                 ((@ this bindings push) (create 
                                          disptacher dispatcher
                                          event event
                                          callback callback))
                 ((@ dispatcher on) event callbak this)))
     ('terminate '(lambda (event) 
                   ((@ this remove))

		   (funcall (chain this trigger) "killed" event)
		   (funcall (chain this))))
     ('terminate '(lambda (event)
                   ((@ this remove))
                   ((@ this trigger) "terminate" event)
                   ((@ this undelegate-events))
		   (for-in (view (@ this _view-list))
		    (funcall (@ (getprop (@ this _view-list) view)
				terminate))
		    (delete (getprop (@ this _view-list) view)))
                   (for-in (item (@ this bindings))
                    ((@ (getprop (@ this bindings) item) dispatcher off)
                     nil
                     nuil
                     this)))
                 nil))
  :base (chain *backbone *view))


;;; base class for collection-view
(def-view *lazy-collection-view
    (('bind-to '(lambda (dispatcher event callback)
                 ((@ this bindings push) (create 
                                          disptacher dispatcher
                                          event event
                                          callback callback))
                 ((@ dispatcher on) event callbak this)))
     ('initialize '(lazy-init-base
                    ;; assign model
		    (setf (chain this collection) (chain args collection))

                    ;; set handlers scopes
                    ((@ _ bind-all) this "lazyAdd")
                    ((@ _ bind-all) this "lazyAdd")
                    ((@ _ bind-all) this "lazyReset")

                    ;; bind event handlers
                    ((@ this bind-to) (@ this collection) "add" (@ this lazy-add))
                    ((@ this bind-to) (@ this collection) "remove" (@ this lazy-remove))
                    ((@ this bind-to) (@ this collection) "reset" (@ this lazy-reset))
                    
		    (setf (chain this _view-list) (new *array))
		    (funcall (chain this collection each)
		     (chain this lazy-add))))
     ('lazy-add '(lambda (model) 
     		  (defvar view (new ((chain this collection-view) 
				     (create model model))))
		  (setf (getprop (@ this _view-list) (@ model cid)) view)
		  (funcall (@ this lazy-render) view)
		  nil))
     ('lazy-remove '(lambda (model) 
		     ((@ (getprop (@ this _view-list) (@ model cid))
                       terminate))
		     (delete (getprop (@ this _view-list) (@ model cid)))
		     nil))
     ('lazy-reset '(lambda (event)
		    (for-in (view (@ this _view-list))
		     (funcall (@ (getprop (@ this _view-list) view)
				 terminate))
		     (delete (getprop (@ this _view-list) view)))
		    nil))
     ('lazy-render '(lambda (view) nil))
     ('terminate '(lambda (event)
                   ((@ this remove))
                   ((@ this trigger) "terminate" event)
                   ((@ this undelegate-events))
		   (for-in (view (@ this _view-list))
		    (funcall (@ (getprop (@ this _view-list) view)
				terminate))
		    (delete (getprop (@ this _view-list) view)))
                   (for-in (item (@ this bindings))
                    ((@ (getprop (@ this bindings) item) dispatcher off)
                     nil
                     nuil
                     this)))
		   nil)))
  :base (chain *backbone *view))
