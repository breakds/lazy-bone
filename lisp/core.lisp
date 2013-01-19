;;;; core.lisp
;;;; Description:


(in-package #:breakds.lazy-bone)

(defparameter *global* (make-hash-table :test #'equal))

(defclass bone ()
    ((name :accessor bone-name
	   :initarg :name
	   :initform 'no-name
	   :type symbol)
     (base :accessor bone-base
	   :initarg :base
	   :initform '(chain *bacbkbone *model)
	   :type (or symbol list))
     (members :accessor bone-members
	      :initarg :members
	      :initform nil
	      :type list)))


;;; ========== Definition Macros ==========
(defmacro def-view (name (&rest members) 
		    &key (base '*lazy-view))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ,base
			:members (list ,@(loop for pair in members
					    append pair)))))

(defmacro def-model (name (&rest members) 
		     &key (base '(list 'chain '*backbone '*model)))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ,base
			:members (list ,@(loop for pair in members
					    append pair)))))


(defmacro def-collection (name (&rest members) 
			  &key (base '(list 'chain 
				       '*backbone '*collection)))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ,base
			:members (list ,@(loop for pair in members
					    append pair)))))


(defmacro def-collection-view (name (&rest members) 
			       &key (base '*lazy-collection-view))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ,base
			:members (list ,@(loop for pair in members
					    append pair)))))


;;; ========== Compiler Macros==========
(defpsmacro wait-for (identifier expression (&body body) &key (event "killed"))
  `(progn (defvar ,identifier ,expression)
          (funcall (chain this undelegate-events))
          (funcall (chain ,identifier on)
                   ,event 
                   (lambda (return-form)
                     (progn ,@body)
                     (funcall (chain this delegate-events))
                     nil)
                   this)))

(defpsmacro lazy-init (&body body)
  `(lambda (args expand self)
     (funcall (lambda (cont)
		(if (ps::== undefined self)
		    (funcall 
		     (chain this constructor __super__ initialize call)
		     this args cont 
		     (chain this constructor __super__))
		    (funcall
		     (chain self constructor __super__ initialize call)
		     this args cont
		     (chain self constructor __super__)))
		nil)
	      this (lambda (args)
		     (progn ,@body)
		     nil))
     (when (ps:!= undefined expand)
       (funcall (chain expand call) this args))
     nil))

(defpsmacro lazy-init-base (&body body)
  `(lambda (args expand)
     (progn ,@body)
     (when (ps:!= undefined expand)
       (funcall (chain expand call) this args))
     nil))


(defpsmacro bone-definition (name)
  (let ((obj (gethash name *global*)))
    `(defvar ,(bone-name obj)
       (funcall (chain ,(bone-base obj) extend)
		(create ,@(bone-members obj))))))

(defpsmacro place-view (name)
  `(setf ,name (lambda () nil)))


;;; ========== pre-defined aux classes ==========
(def-view *lazy-view
    (('initialize '(lazy-init-base (funcall (chain _ bind-all) 
				   this "lazyKill")))
     ('lazy-kill '(lambda (event) 
		   (funcall (chain this trigger) "killed" event)
		   (funcall (chain this remove)))))
  :base '(chain *backbone *view))



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


      










		       
  

  