;;;; graph.lisp
;;;; Description: routines for building topolocial graph on *gloabl*
;;;;              namespace.
;;;; Author: BreakDS
;;;; Date: Sun Jan 20 09:05:36 CST 2013


(in-package lazy-bone)


(defun gen-topological ()
  "Find the topological order of *global* namespace based on
  inheritance relation."
  (let ((hash (make-hash-table))
        (accu nil))
    (labels ((visit (name)
               (let ((obj (gethash name *global*)))
                 (when (and obj (null (gethash name hash)))
                   (visit (bone-base obj))
                   (setf (gethash name hash) t)
                   (push name accu)))))
      (loop for name being the hash-key of *global*
         do (visit name)))
    (nreverse accu)))
           
         

