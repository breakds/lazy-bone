

;; definition of my button
(lazy-bone:def-view *my-button 
    (('tag-name "button")
     ('template "<%= caption %")
     ('events '(ps:create "click" "onClick"))
     ('initialize '(lazy-bone:lazy-init
                    '(setf (ps:chain this caption) 
                      (ps:chain args caption))))
     ('render '(lambda () this))
     ('onClick '(lambda () nil))))




;; 
  

			  
	      
    




(with-compile-application test-application
  (with-view gen-button
    :base my-button
    :on-click (progn 
                (defvar state-yes (new (button-state (create msg "yes" caption "Yes"))))
                (defvar state-no (new (button-state (create msg "no" caption "No"))))
                (defvar panel (new (button-panel (create collection (new (state-set (array state-yes 
                                                                                           state-no)))))))
                (with-wait-for-event (panel "killed")
                  ...))))
                                
                
      
 
 
