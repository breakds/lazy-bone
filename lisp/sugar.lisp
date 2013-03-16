;;;; sugar.lisp
;;;; Description: sugar for writing lazy-bone parenscript code for Backbone.js


(in-package #:breakds.lazy-bone)



(defpsmacro wait-until (identifier expression (&body body) &key (event "terminate"))
  "continuation semantics"
  `(progn (defvar ,identifier ,expression)
          (funcall (chain this undelegate-events))
          ((@ this bind-to) ,identifier ,event
           (lambda (return-form)
             (progn ,@body)
             (funcall (chain this delegate-events))
             nil))))


(defpsmacro lazy-init (&body body)
  "create initialize function that calls parent initialize before
  executing the body" 
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
  "create initialize function that do not call parent initialize"
  `(lambda (args expand self)
     (progn ,@body)
     (when (not (equal undefined expand))
       (funcall (chain expand call) this args))
     nil))


(defpsmacro bone-definition (name)
  "define a bone (view/collection/model)"
  (let ((obj (gethash name *global*)))
    `(defvar ,(bone-name obj)
       (funcall (chain ,(bone-base obj) extend)
		(create ,@(bone-members obj))))))

(defpsmacro place-view (name)
  `(setf ,name (lambda () nil)))


(defpsmacro trace (&rest expressions)
  "output to the console"
  `((@ console log) (+ ,@expressions)))


(defpsmacro construct-chain (accu &rest forms)
  (if (null forms)
      accu
      (let ((x (car forms)))
        (cond ((null accu) `(construct-chain ,x ,@(cdr forms)))
              ((consp x) `(construct-chain ,(cons (list '@ accu (car x)) 
                                                  (cdr x))
                                           ,@(cdr forms)))
              (t `(construct-chain ,(list '@ accu x) 
                                   ,@(cdr forms)))))))

(defpsmacro @. (&rest forms)
  "construct chain with chaining semantics"
  `(construct-chain nil ,@forms))



(defpsmacro render-from-model ()
  "convert the model to json format and use it in the template to
  render the view"
  `((@ this $el html)
    (((@ _ template)
      (@ this template))
     ((@ this model to-j-s-o-n)))))

(defpsmacro @get (str &optional (model '(@ this model)))
  "get the property of a model"
  `((@ ,model get) ,str))


(defpsmacro acquire-args ((&rest names) (&rest arg-names))
  "acquire the args to member variables"
  `(setf ,@(mapcan (lambda (x y) (list (list 'chain 'this x) 
				       (list 'chain 'args y)))
		   names arg-names)))