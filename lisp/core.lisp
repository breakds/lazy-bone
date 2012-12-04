;;;; core.lisp
;;;; Description: core functions/macros for lazy-bone framework
;;;; Author: BreakDS
;;;; Date: Sun Dec  2 13:53:42 CST 2012

(in-package #:breakds.lazy-bone)

;; + ---------- Global Variables ---------- +

(defparameter *namespace* (make-hash-table))


;;; +---------- Data Structures ----------+

;; Bone: A backbone class instance represent a backbone.js class that can be
;; compiled to javascript declaration and defintion
;; +----------
;; | name: symbol representing the identity of the class
;; | base: symbol representing the idenetty of the base class
;; | members: plist of (symbol, paren-script) pair

(defstruct bone
  (name 'noname)
  (base '*object)
  (members nil))


(defmethod add-member ((obj bone) member-name member-value)
  (setf (getf obj member-name) member-value))


(defmacro compile-bone (obj)
  (with-gensyms (obj-var)
    `(let ((,obj-var ,obj))
       (ps:ps* (list 'ps:defvar (bone-name ,obj-var)
                     (list (list 'ps:chain (bone-base ,obj-var) 'extend)
                           (cons 'ps:create (bone-members ,obj-var))))))))


(defmacro def-model (name &key (defaults nil) (methods nil))
  `(setf (gethash ',name *namespace* )
         (make-bone :name ',name
                    :base '(ps:chain *backbone *model)
                    :members (append (list 'defaults (list 'ps:lambda '() 
                                                           (cons 'ps:create ,defaults)))
                                     ,methods))))
                                     

(defmacro def-view (name &key (tag-name "") (template "") (events nil) (methods nil))
  `(setf (gethash ',name *namespace* )
         (make-bone :name ',name
                    :base '(ps:chain *backbone *view)
                    :members (append (list 'tag-name ,tag-name)
                                     (list 'template ,template)
                                     (list 'events (cons 'ps:create ,events))
                                     (list 'expand (list 'ps:lambda '(args) 'ps:nil))
                                     (list 'render (list 'ps:lambda '()
                                                         '((ps:chain ps:this $el html)
                                                           ((ps:chain ps:this template) ps:this))
                                                         'ps:this))
                                     ,methods))))

(defmacro compile-obj (name)
  (with-gensyms (obj)
    `(let ((,obj (gethash ',name *namespace* )))
       (when ,obj
         (compile-bone ,obj)))))



;;; +---------- Backbone Extensions for Parenscript ----------+

(defmacro with-view (base (&rest members) &key (instance nil))
  (with-gensyms (class-name instance-name)
    `(let ((,class-name (ps:gen-js-name))
           (,instance-name (aif ,instances it (ps:gen-js-name))))
       `(setf (gethash ,class-name *namespace*)
              (make-bone :name ,class-name
                         :base ,base))
       (let ((parent (gethash ,class-name *namespace*)))
         
       (list 'ps:defvar ,instance-name (list 'ps:new ,classname)))))














