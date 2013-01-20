;;;; skeleton.lisp
;;;; Description: Augmented Backbone.js classes that serve as the base
;;;;              class for lazy-bone objects (will be automatically
;;;;              added to the compiled JavaScript file).
;;;; Author: BreakDS
;;;; Date: Sun Jan 20 08:33:47 CST 2013


(in-package #:breakds.lazy-bone)

;;; base class for view
(def-view *lazy-view
    (('initialize '(lazy-init-base (funcall (chain _ bind-all) 
				   this "lazyKill")))
     ('lazy-kill '(lambda (event) 
		   (funcall (chain this trigger) "killed" event)
		   (funcall (chain this remove)))))
  :base '(chain *backbone *view))


;;; base class for collection-view
(def-view *lazy-collection-view
    (('collection-view '(create))
     ('_view-list '(create))
     ('initialize '(lazy-init-base
		    (setf (chain this collection) (chain args collection))
		    (funcall (chain _ bind-all) this "lazyAdd")
		    (funcall (chain this collection on) "add" 
		     (chain this lazy-add))
		    (funcall (chain _ bind-all) this "lazyRemove")
		    (funcall (chain this collection on) "remove" 
		     (chain this lazy-remove))
		    (funcall (chain _ bind-all) this "lazyReset")
		    (funcall (chain this collection on) "reset" 
		     (chain this lazy-reset))
		    (funcall (chain _ bind-all) this "lazyKill")
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
		     (funcall (@ (getprop (@ this _view-list) (@ model cid))
		     remove))
		     (delete (getprop (@ this _view-list) (@ model cid)))
		     nil))
     ('lazy-reset '(lambda (event)
		    (for-in (view (@ this _view-list))
		     (funcall (@ (getprop (@ this _view-list) view)
				 remove))
		     (delete (getprop (@ this _view-list) view)))
		    nil))
     ('lazy-render '(lambda (view) nil))
     ('lazy-kill '(lambda (event)
		   (for-in (view (@ this _view-list))
		    (funcall (@ (getprop (@ this _view-list) view)
				remove))
		    (delete (getprop (@ this _view-list) view)))
		   (funcall (@ this trigger) "killed" event)
		   nil)))
  :base '(chain *backbone *view))