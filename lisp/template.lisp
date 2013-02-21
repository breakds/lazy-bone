;;;; template.lisp
;;;; Description: routines for loading templates
;;;; Author: BreakDS
;;;; Date: Wed Feb 20 20:00:48 CST 2013

(in-package lazy-bone)

(defparameter *template-store* (make-hash-table :test #'equal))
(defparameter *template-registry* #P"")

(defun set-template-registry (dir-path)
  "set the template registry to the directory"
  (setf *template-registry* (truename dir-path)))

(defun read-tmpl (tmpl-file)
  "read the template file"
  (let ((true-path (truename (merge-pathnames tmpl-file *template-registry*))))
    (multiple-value-bind (value exist) 
        (gethash true-path *template-store*)
      (if exist
          value
          (setf (gethash true-path *template-store*)
                (read-file-into-string true-path))))))
                     
  

