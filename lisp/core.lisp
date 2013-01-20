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
			:base ',base
			:members (list ,@(loop for pair in members
					    append pair)))))

(defmacro def-model (name (&rest members) 
		     &key (base '(list 'chain '*backbone '*model)))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ',base
			:members (list ,@(loop for pair in members
					    append pair)))))


(defmacro def-collection (name (&rest members) 
			  &key (base '(list 'chain 
				       '*backbone '*collection)))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ',base
			:members (list ,@(loop for pair in members
					    append pair)))))


(defmacro def-collection-view (name (&rest members) 
			       &key (base '*lazy-collection-view))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ',base
			:members (list ,@(loop for pair in members
					    append pair)))))


(defmacro access-bone (name)
  `(gethash ',name *global*))

(defun clear-global ()
  (setf *global* (make-hash-table))
  (load (merge-pathnames "lisp/skeleton.lisp" (asdf:system-source-directory 'lazy-bone))))
        



;;; ========== Compiler Macros==========
(defpsmacro wait-until (identifier expression (&body body) &key (event "killed"))
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

(defmacro compile-to-js (&body body)
  (with-gensyms (name)
    `(ps* (list '$ (list 'lambda '() 
                         (cons 'progn
                               (loop for ,name in (gen-topological)
                                  collect (list 'bone-definition ,name)))
                         ,@body
                         nil)))))



        

      










		       
  

  