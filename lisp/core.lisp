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
		     &key (base '(chain *backbone *model)))
  `(setf (gethash ',name *global*)
	 (make-instance 'bone
			:name ',name
			:base ',base
			:members (list ,@(loop for pair in members
					    append pair)))))


(defmacro def-collection (name (&rest members) 
			  &key (base '(chain 
				       *backbone *collection)))
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
(defpsmacro wait-until (identifier expression (&body body) &key (event "terminate"))
  `(progn (defvar ,identifier ,expression)
          (funcall (chain this undelegate-events))
          ((@ this bind-to) ,identifier ,event
           (lambda (return-form)
             (progn ,@body)
             (funcall (chain this delegate-events))
             nil))))


(defpsmacro lazy-init (&body body)
  `(lambda (args expand self)
     ((@ (lambda (cont)
		(if (equal undefined self)
		    (funcall 
		     (chain this constructor __super__ initialize call)
		     this args cont 
		     (chain this constructor __super__))
		    (funcall
		     (chain self constructor __super__ initialize call)
		     this args cont
		     (chain self constructor __super__)))
		nil) call)
      this (lambda (args)
             (progn ,@body)
             nil))
     (when (not (equal undefined expand))
       (funcall (chain expand call) this args))
     nil))

(defpsmacro lazy-init-base (&body body)
  `(lambda (args expand self)
     (progn ,@body)
     (when (not (equal undefined expand))
       (funcall (chain expand call) this args))
     nil))


(defpsmacro bone-definition (name)
  (let ((obj (gethash name *global*)))
    `(defvar ,(bone-name obj)
       (funcall (chain ,(bone-base obj) extend)
		(create ,@(bone-members obj))))))

(defpsmacro place-view (name)
  `(setf ,name (lambda () nil)))

(defpsmacro trace (expression)
  `((@ console log) ,expression))


;;; ========== temperary parenscript macros ==========
(defpsmacro acquire-args ((&rest names) (&rest arg-names))
  `(setf ,@(mapcan (lambda (x y) (list (list 'chain 'this x) 
				       (list 'chain 'args y)))
		   names arg-names)))
  

  


(defmacro compile-to-js (&body body)
  (with-gensyms (name)
    `(ps* (list '$ (list 'lambda '() 
                         (cons 'progn
                               (loop for ,name in (gen-topological)
                                  collect (list 'bone-definition ,name)))
                         ,@body
                         nil)))))

;; ========== Hunchentoot ==========
(defparameter *acceptor* nil)


(defmacro define-simple-app (app-name (&key (title "Simple Application") 
                                            (uri "/app") 
                                            (template nil)
					    (css nil)
					    (libs nil)
                                            (port 8080)
                                            (document-base "")) &body body)
  `(progn
     (hunchentoot:define-easy-handler (,app-name :uri ,uri) ()
       (setf (hunchentoot:content-type*) "text/html")
       (let ((html-template:*string-modifier* #'identity))
         (with-output-to-string (html-template:*default-template-output*)
           (html-template:fill-and-print-template 
            ,(let ((tmpl template))
                  (if tmpl
                      tmpl
                      (merge-pathnames "template/simple-template.tmpl" (asdf:system-source-directory 'lazy-bone))))
            (list :title ,title
		  :css (list ,@(loop for url in css
				  collect (list 'list :url url)))
		  :libs (list ,@(loop for url in libs
				   collect (list 'list :url url)))
                  :javascript (compile-to-js ,@body))))))
     (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor 
                                     :port ,port
                                     :document-root ,document-base))))




(defun start-server ()
  (when *acceptor*
    (hunchentoot:start *acceptor*)
    (format t "server started.~%")))


(defun stop-server ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (format t "server stopped.~%")))


                                     


  
        
          
        
        
     
        












		       
  

  