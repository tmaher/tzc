(defun face-at-point () 
  "What face is current point in?" 
  (interactive) 
  (message (let ((name (or  
                        (car (cdr  
                              (member 'face (extent-properties-at (point))))) 
                        "default")))
             (format "%s fg: %s bg: %s font: %s"
                     name
                     (face-foreground-name name)
                     (face-background-name name)
                     (face-font-name name)))))

(defun revert-force ()
  (interactive)
  (revert-buffer t t t))
