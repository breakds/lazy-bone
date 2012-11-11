;;;; core.lisp
;;;; Description: core functions/macros for lazy-bone framework
;;;; Author: BreakDS
;;;; Date: Sun Oct 21 15:39:51 CDT 2012


(in-package #:breakds.lazy-bone)

;;;; lazy-bone core code goes below



;;; +------------------- Global Variables -------------------+

;; use string as key for handlers
(defparameter *handlers* (make-hash-table :test #'equal))

;; acceptor for hunchentoot
(defparameter *acceptor* nil)


(defparameter *global-namespace* (make-hash-table))




;;; +-------------------- strcutures --------------------+


;; widgets will temparorily be like
;; (list :models (list) 
;;       :events (list)
;;       :init   (DSL) 
;;       :finish (DSL))

;; models will temparorily be like
;; (list :data (json-list) :listener (widget))

;;; Views

(defun make-view (&key (tag-name nil) (template "") (events nil))
  "make a skeleton view"
  (list :tag-name tag-name
        :template template
        :events events))

(defmacro view-add-property (view name js-thunk)
  "add a method to the view"
  ;; TODO: check whether body is a lambda
  (with-gensyms (view-var)
    `(let ((,view-var (gethash ,view *global-namespace*)))
       (when ,view-var
         (setf (getf ,view-var ,name) ,js-thunk)))))
         
  
  

(defmacro def-view (name &key (tag-name nil) (template "") (events nil))
  "define a view"
  (with-gensyms (view-name new-view)
    `(let ((,view-name ,name)
           (,new-view (make-view :tag-name ,tag-name
                                 :template ,template
                                 :events ,events)))
       (setf (gethash ,view-name *global-namespace*) ,new-view))))
                       

       
         
    
    
    
  
                           
                                                           













  
  







