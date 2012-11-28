;;;; core.lisp
;;;; Description: core functions/macros for lazy-bone framework
;;;; Author: BreakDS
;;;; Date: Sun Oct 21 15:39:51 CDT 2012


(in-package #:breakds.lazy-bone)

;;;; lazy-bone core code goes below



;; +------------------- Global Variables -------------------+

;; use string as key for handlers
(defparameter *handlers* (make-hash-table :test #'equal))

(defparameter *global-namespace* (make-hash-table))

(defparameter *models* (make-hash-table))

(defparameter *views* (make-hash-table))


;;; +-------------------- strcutures --------------------+

;;; Models
;;; structure 
;;;    +
;;;    | members: hashtable of (name, paren-value) pairs
;;;    | defaults: paren-function
;;;    | init: paren-function
;;; constructor 
;;;    +
;;;    | defaults: plist of (name, initial value)
;;;    | methods: plist of (name, paren-function)


(defstruct model name (members nil) (defaults nil) (init nil))

;;; Views
;;; structure 
;;;    +
;;;    | members: hashtable of (name, paren-value) pairs
;;;    | tagName: string
;;;    | tempalte: string
;;;    | events: plist of (name, member-name)
;;;    | expand: paren-function

(defstruct view name 
           (members nil) 
           (tag-name "") 
           (template "") 
           (render nil)
           (events nil)
           (expand '(lambda (args) args)))

           


(defmacro def-view (name &key (tag-name "") (template "") (events nil) (methods nil) (expand nil))
  (let ((name-var name))
    `(setf (gethash ',name-var *views*)
           (make-view :name ',name-var
                      :members ,methods
                      :tag-name ,tag-name
                      :template ,template
                      :events ,events
                      :expand ,expand
                      :render '(lambda () 
                                ((chain this $el html)
                                 (chain this template)
                                 this)
                                this)))))
                                
                                
                                

(defmacro compile-view (name)
  (let ((name-var name))
    `(let ((view-obj (gethash ',name-var *views*)))
       (ps* (list defvar ,name-var ((chain *backbone *view extend)
                                    (create 
                                     tag-name (view-tag-name view-obj)
                                     template (view-template view-obj)
                                     expand (view-expand view-obj)
                                     render (view-render view-obj))))))))
                                 
                                 
                               
                           




;;; TODO: Handle initialization
(defmacro def-model (name &key (defaults nil) (methods nil))
  "Define a model"
  (let ((name-var name))
    `(setf (gethash ',name-var *models*)
           (make-model :name ',name-var
                       :members ,methods
                       :defaults '(lambda () (create ,@defaults))))))


;;; TODO: Handle initialization
(defmacro compile-model (name)
  "Compile a model definition"
  (let ((name-var name))
    `(ps* '(defvar ,name-var ((chain *backbone *model extend)
                              (create defaults (model-defaults (gethash ',name-var *model))))))))


  
  




;;; Views

;; (defun make-view (&key (tag-name nil) (template "") (events nil))
;;   "make a skeleton view"
;;   (let ((new-view (list :tag-name tag-name
;;                         :template template
;;                         :events events
;;                         :expand '(lambda (args) args))))
;;     (mapcar (lambda (event)
;;               (setf (getf new-view (cdr event)) 
;;                     '(lambda (e) e)))
;;             events)
;;     new-view))
        


;; (defmacro view-add-property (view name js-thunk)
;;   "add a method to the view"
;;   ;; TODO: check whether body is a lambda
;;   (with-gensyms (view-var)
;;     `(let ((,view-var (gethash ,view *global-namespace*)))
;;        (when ,view-var
;;          (setf (getf ,view-var ,name) ,js-thunk)))))
         

;; (defmacro def-view (name &key (tag-name nil) (template "") (events nil))
;;   "define a view"
;;   (with-gensyms (view-name new-view events-var)
;;     `(let* ((,view-name ,name)
;;             (,events-var ,events)
;;             (,new-view (make-view :tag-name ,tag-name
;;                                   :template ,template
;;                                   :events ,events-var)))
;;        (setf (gethash ,view-name *global-namespace*) ,new-view))))
                       

       
         
    
    
    
  
                           
                                                           













  
  







