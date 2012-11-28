;;;; core.lisp
;;;; Description: core functions/macros for lazy-bone framework
;;;; Author: BreakDS
;;;; Date: Sun Oct 21 15:39:51 CDT 2012


(in-package #:breakds.lazy-bone)

;;;; lazy-bone core code goes below



;;; +------------------- Global Variables -------------------+

;; use string as key for handlers
(defparameter *handlers* (make-hash-table :test #'equal))

(defparameter *glob
al-namespace* (make-hash-table))




;;; +-------------------- strcutures --------------------+

;;; Models
;;; structure 
;;;    +
;;;    | members: hashtable of (name, paren-value) pairs
;;; constructor 
;;;    +
;;;    | defaults: plist of (name, initial value)
;;;    | methods: plist of (name, paren-functions)



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
                       

       
         
    
    
    
  
                           
                                                           













  
  







