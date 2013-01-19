;;;; core.lisp
;;;; Description:


(in-package #:breakds.lazy-bone)

(defparameter *global* (make-hash-table))

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


;;; ========== Aux Functions ==========
(defmacro gen-init-fun (&key (body '(progn)) (call-parent nil))
  (with-gensyms (body-var call-parent-var)
    `(let ((,body-var ,body)
	   (,call-parent-var ,call-parent))
       (list 'lambda '(args expand)
	     (if ,call-parent-var
		 (list 'funcall 
		       '(chain this constructor __super__ initialize call)
		       'this 'args (list 'lambda '(args)
					 ,body-var
					 nil))
		 ,body-var)
	     '(when (ps:!= undefined expand) 
	       (funcall (chain expand call) this args))
	     nil))))

;;; ========== Definition Macros ==========
(defmacro def-view (name (&rest members) 
		    &key (base '(list 'chain '*backbone '*view)))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ,base
			:members (list ,@members))))

(defmacro def-model (name (&rest members) 
		     &key (base '(list 'chain '*backbone '*model)))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ,base
			:members (list ,@members))))

(defmacro def-collection (name (&rest members) 
			  &key (base '(list 'chain 
				       '*backbone '*collection)))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ,base
			:members (list ,@members))))

(defmacro def-collection-view (name (&rest members) 
			  &key (base '(list 'chain 
				       '*backbone '*lazy-collection-view)))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ,base
			:members (list ,@members))))


;;; ========== 1st level Compilers ==========

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
          

(defpsmacro place-view (name)
  `(setf ,name (lambda () nil)))







		       
  

  