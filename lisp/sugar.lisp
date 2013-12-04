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

(defpsmacro duplicate (name &rest para-list)
  "instantiate a new model/collection/view"
  `(new (,name (create ,@(mapcan #`(,(swiss-knife:symb (car x1)) ,(cadr x1))
                                 (swiss-knife:group para-list 2))))))


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

(defpsmacro append-to-parent ()
  `(@. ($ (@ this parent-node)) 
       (append (@. this (render) el))))

(defpsmacro @get (str &optional (model '(@ this model)))
  "get the property of a model"
  `((@ ,model get) ,str))

(defpsmacro @set (str val &optional (model '(@ this model)))
  "get the property of a model"
  `((@ ,model set) ,str ,val))



(defpsmacro acquire-args ((&rest names) (&rest arg-names))
  "acquire the args to member variables"
  `(setf ,@(mapcan (lambda (x y) (list (list 'chain 'this x) 
				       (list 'chain 'args y)))
		   names arg-names)))

(defpsmacro eval-lisp (&body body)
  "eval lisp code which generates parenscript code"
  (eval `(progn ,@body)))

(defpsmacro create-event-manager (obj &rest events-handlers)
  `(progn (setf ,obj ((@ _ extend) (create) (@ *backbone *events)))
          ,@(mapcar #`((@ ,obj on) ,(car x1) ,(cadr x1) this)
                    (swiss-knife:group events-handlers 2))))

(defpsmacro stringified-obj (&rest para-list)
  `(@. *json* (stringify 
               (create ,@(mapcan #`(,(swiss-knife:symb (car x1)) ,(cadr x1))
                                 (swiss-knife:group para-list 2))))))
                       
                                     
(defpsmacro properties (&rest para-list)
  "defaults list for def-models"
  `(lambda () 
     (create ,@(mapcan #`(,(swiss-knife:symb (car x1)) ,(cadr x1))
                       (swiss-knife:group para-list 2)))))


(defmacro var-to-string (x)
  (with-gensyms (pre)
    `(let ((,pre (ps:ps ,x)))
       (subseq ,pre 0 (1- (length ,pre))))))



(defpsmacro @fetch (obj &rest para-list)
  (with-gensyms (json-obj session)
    (let ((url (getf para-list :url))
          (server-header (cadr (getf para-list :server)))
          (server-body (cddr (getf para-list :server)))
          (plist (copy-list para-list)))
      (remf plist :url)
      (remf plist :server)
      `(progn
         (setf (@ ,obj url) ,url)
         (@. ,obj (fetch (create ,@(mapcan #`(,(swiss-knife:symb (car x1)) ,(cadr x1))
                                           (swiss-knife:group plist 2))
                                 data (@. *json* (stringify (create ,@(mapcan #`,x1 server-header)))))))
         (eval-lisp (define-easy-handler (,(swiss-knife:symb url '-handler) :uri ,url) ()
                      (setf (content-type*) "application/json")
                      (let ((,json-obj (jsown:parse (raw-post-data :force-text t)))
                            (,session (start-session)))
                        (declare (ignorable ,session))
                        (let (,@(mapcar #`(,(car x1) (jsown:val ,json-obj (var-to-string ,(car x1))))
                                        server-header))
                          ,@server-body)))
                    nil)))))
                      
         
     