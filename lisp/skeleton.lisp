;;;; skeleton.lisp
;;;; Description: Augmented Backbone.js classes that serve as the base
;;;;              class for lazy-bone objects (will be automatically
;;;;              added to the compiled JavaScript file).
;;;; Author: BreakDS
;;;; Date: Sun Jan 20 08:33:47 CST 2013


(in-package #:breakds.lazy-bone)

;;; base class for collection
(def-model *lazy-collection
    (('initialize '(lazy-init-base
                    (if (@ args model-list)
                        (setf (@ this list) 
                              (new ((@ *backbone *collection) 
                                    (@ args model-list)
                                    (create model (@ this model)))))
                        (setf (@ this list)
                              (new ((@ *backbone *collection) 
                                    (array)
                                    (create model (@ this model))))))))))

;;; base class for view
(def-view *lazy-view
    (('initialize '(lazy-init-base
		    (if (not (equal undefined (@ args parent-node)))
			(setf (@ this parent-node)
			      (@ args parent-node))
			(setf (@ this parent-node)
			      ($ "body")))
		    (setf (@ this view-list) (new (*array)))))
     ('terminate '(lambda (msg)
		   ((@ this undelegate-events))
		   ((@ this remove))
		   (for-in (cid (@ this view-list))
		    ((@ (getprop (@ this view-list) cid) terminate))
		    (delete (getprop (@ this view-list) cid)))
		   nil))
     ('add-sub-view '(lambda (view)
		      (setf (getprop (@ this view-list) (@ view cid)) view))))
  :base (chain *backbone *view))

(def-view *lazy-collection-view
    (('initialize '(lazy-init
		    (setf sub-view (create))
		    ((@ _ bind-all) this "lazyAdd")
		    ((@ _ bind-all) this "lazyRemove")
		    ((@ this listen-to) (@ this model list) 
		     "add" (@ this lazy-add))
		    ((@ this listen-to) (@ this model list) 
		     "remove" (@ this lazy-remove))))
     ('lazy-add '(lambda (model)
                  (defvar view nil)
                  (let ((parent-node (if (@ this entry-point)
                                         ((@ this $) (@ this entry-point))
                                         (@ this $el))))
                    (setf view (new ((@ this sub-view) 
                                     (create model model 
                                             parent-node parent-node)))))
		  (setf (getprop (@ this view-list) (@ model cid)) view)
		  view))
     ('lazy-remove '(lambda (model) 
		     ((@ (getprop (@ this view-list) (@ model cid)) terminate))
		     (delete (getprop (@ this view-list) (@ model cid))))))
  :base *lazy-view)

