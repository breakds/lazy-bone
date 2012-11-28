;; acceptor for hunchentoot

(in-package #:breakds.lazy-bone-example)

(def-view 'my-button 
    :tag-name "button"
    :template "<%= caption %>"
    :events (list '("click" . :onClick)))


(compile-page (with-widget (btn0 "button") 
                (:on-click (with-widget-fire (btn1 "button")
                             (:on-click (fire) (remove))))))


(let ((local-namespace (make-hashtable ))
      (this nil))
  (let ((parent this 




  
          
          