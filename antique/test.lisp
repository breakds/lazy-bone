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
                                
                
      
 
 
