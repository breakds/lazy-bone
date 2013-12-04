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


(defmacro def-class-create (def-name base)
  `(defmacro ,(swiss-knife:symb 'def- def-name) 
       (name (&rest members)
        &key (base ',base))
     `(setf (gethash ',name *global*)
            (make-instance 'bone
                           :name ',name
                           :base ',base
                           :members (list ,@(mapcan #`(',(car x1) ',(cadr x1))
                                                    members))))))

(def-class-create view *lazy-view)
(def-class-create model (chain *backbone *model))
(def-class-create router (chain *backbone *router))
(def-class-create collection *lazy-collection)
(def-class-create collection-view *lazy-collection-view)


(defmacro access-bone (name)
  `(gethash ',name *global*))

(defun clear-global ()
  (setf *global* (make-hash-table))
  (load (merge-pathnames "lisp/skeleton.lisp" (asdf:system-source-directory 'lazy-bone))))
        



(defmacro compile-to-js (&body body)
  (with-gensyms (name)
    `(ps* (list '$ (list 'lambda '() 
                         (cons 'progn
                               (loop for ,name in (gen-topological)
                                  collect (list 'bone-definition ,name)))
                         ,@(mapcar #`',x1 body)
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

