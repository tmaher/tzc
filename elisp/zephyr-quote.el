(defvar zephyr-quote-string "> "
  "String used by `zephyr-quote-message' as a quoting prefix.")

(defun zephyr-prev-divider (&optional location beg)
  (save-excursion
    (let ((p (or location (point))))
      (goto-char p)
      (forward-line)
      (let* ((either-div (concat "^.+\\(" zephyr-receive-divider-regexp 
				 "\\|" zephyr-send-divider-regexp "\\)")))
	(re-search-backward either-div (point-min) t) 
	(if beg
	    (match-beginning 0)
	  (match-end 0))))))

(defun zephyr-extent-at-location (&optional location dividers)
  "Returns a cons of the start and end of the zephyr at location.
If location is nil, the point is used. If dividers is non-nil, 
the extent with the divider is included."
  (save-excursion
    (let ((p (or location (point))))
      (goto-char p)
      (forward-line)
      (let* ((either-div (concat "^.+\\(" zephyr-receive-divider-regexp 
				 "\\|" zephyr-send-divider-regexp "\\)")) 
	     (end-msg (if (re-search-forward either-div (point-max) t) 
			  (match-beginning 0) 
			(re-search-backward either-div (point-min) t) 
			(match-beginning 0))) 
	     (-- (goto-char (match-beginning 0))) 
	     (begin-msg (if (re-search-backward either-div (point-min) t) 
			    (if dividers
				(match-beginning 0)
			      (match-end 0))
			  (point-min))))
	(cons begin-msg end-msg)))))
  
(defun zephyr-string-at-location (&optional location dividers)
  "returns a string at location, with or without dividers"
  (let* ((extent (zephyr-extent-at-location location dividers))
	  (beg (car extent))
	  (end (cdr extent)))
       (buffer-substring beg end)))

(defun zephyr-quote-message (&optional location string)
  "Quote the zephyr at LOCATION.  
If LOCATION is nil, then use message at point.  If the point is in the 
sending area, will use the previous message. String will be inserted right before, if it exists"
  (interactive)
  (save-excursion
    (let* ((either-div (concat "^.+\\(" zephyr-receive-divider-regexp
			       "\\|" zephyr-send-divider-regexp "\\)"))
	   (zephyr-cut (zephyr-string-at-location location)))
      (goto-char (point-max))
      (re-search-backward either-div (point-min) t)
      (goto-char (match-end 0))
      (if (not (bolp))
	  (if (eobp)
	      (insert "\n")
	    (progn (forward-line) (beginning-of-line))))
      (if string
	  (insert string))
      (let ((location (point)))
	(insert zephyr-cut)
	(setq p (copy-marker (point)))
	(if (bolp)
	    (forward-line -1)
	  (beginning-of-line))
	(string-rectangle location (point) zephyr-quote-string)))
    (goto-char p)))


(defun zephyr-quote-region (arg)
  "Force-fill and quote the active region."
  (interactive "*P")
  (if (region-active-p) 
      (progn (fill-region (region-beginning) (region-end))
       (prefix-region "> "))))

(defun zephyr-get-msg ()
  "Returns a cons of the start and end positions of the outgoing message."
  (save-excursion
    (let* ((either-div (concat "^.+\\(" zephyr-receive-divider-regexp
			       "\\|" zephyr-send-divider-regexp "\\)"))
	   (send-div   (concat "^.+\\("
			       zephyr-send-divider-regexp
			       "\\)"))
	   (end-msg    (if (re-search-forward either-div (point-max) t)
			   (match-beginning 0)
			 (point-max)))
	   (start-recip-ck  (progn 
			      (goto-char end-msg)
			      (if (re-search-backward either-div (point-min) t)
				  (match-beginning 0)
				(error "no message at point"))))
	   (start-recip     (progn (goto-char end-msg)
				   (re-search-backward send-div (point-min) t)
				   (match-beginning 0)))
	   (end-recip      (match-beginning 1))
	   (start-msg (match-end 0)))
      (cons start-msg end-msg))))

(defun zephyr-quote-outgoing (arg)
  "Force-fill and quote the outgoing message."
  (interactive "*P")
  (let* ((outgoing-msg (zephyr-get-msg))
	 (start-msg (car outgoing-msg))
	 (end-msg (cdr outgoing-msg)))
    (fill-region start-msg end-msg)
    (push-mark start-msg t t)
    (prefix-region "> ")
    (zmacs-deactivate-region)
    (pop-mark)))


(provide 'zephyr-quote)
