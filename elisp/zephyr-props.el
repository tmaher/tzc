; Add the ability to tag metadata onto each zephyr
; lets you hide entire groups 
; still in progress.

(require 'zephyr)

(defvar zephyr-reply-format-string "\n\n%s said:\n" "*format the reply string how?")
(defvar zephyr-quote-string "> "
  "String used by `zephyr-quote-message' as a quoting prefix.")
(defvar zephyr-buffer-read-only t "Incoming zephyrs are read-only")
(defvar zephyr-symbol-obarray (make-vector 101 0) "Object array for property symbols")
(defvar zephyr-send-regexp (concat "^\\(.*\\)" zephyr-send-divider-regexp) "Send string")

(defun zephyr-intern (str)
  "Intern a string in the obarray"
  (intern str zephyr-symbol-obarray))

(defun zephyr-intern-prop (list)
  "Intern an alist (for easy retrieval)"
  (zephyr-intern (downcase (format "zephyr-%S" list))))

(defun zephyr-property-assq (alist &rest proplist)
  "Grab stuff from an alist, intern that"
  (zephyr-intern-prop (mapcar (lambda (prop)
				(let ((a (assq prop alist)))
				  (setcdr a (format "%s" (cdr a)))
				  a)) proplist)))

(defun add-unique (var values)
  "Add uniqely to list variable VAR the list of VALUES."
  (mapcar '(lambda (x)
	     (if (not (member x (symbol-value var)))
		 (set var (cons x (symbol-value var)))))
	  values))

(defun zephyr-hide-sym (sym)
  (add-unique 'buffer-invisibility-spec `(,sym)))

(defun zephyr-hide-alist (alist)
  "hide some specified alist"
  (let ((sym (zephyr-intern-prop alist)))
    (zephyr-hide-sym sym)))

(defun zephyr-tag-outgoing-extents (-- --)
  "hook before zephyr goes out to tag things that are outgoing"
  (let* ((end (previous-single-property-change (point-max) 'zephyr-msg-end))
	 (msg-beg (make-marker))
	 (body-beg (make-marker))
	 (msg-end (make-marker)))

    (save-excursion
      (save-restriction
	(goto-char (or end (point-min)))
	(re-search-forward zephyr-send-regexp nil t)
	(let* ((to (match-string 1))
	       (fake `((class . " outgoing ")
		       (instance . " outgoing ")
		       (recipient . ,zephyr-id)
		       (sender . ,to)))
	       (cir    (zephyr-property-assq fake 'class 'instance 'recipient))
	       (cr     (zephyr-property-assq fake 'class 'recipient))
	       (sender (zephyr-property-assq fake 'sender)))
	  
	  (set-marker msg-beg (match-beginning 0))
	  (set-marker body-beg (match-end 0))
	  (set-marker msg-end (point-max-marker))
	  (add-nonduplicable-text-properties msg-beg msg-end
		`(invisible (,cir ,cr ,sender) zephyr-msg-begin ,msg-beg zephyr-msg-end ,msg-end zephyr-body-begin ,body-beg zephyr-instance "personal")))))
    (when zephyr-buffer-read-only 
      (add-nonduplicable-text-properties msg-beg msg-end '(read-only t)))))


(defun zephyr-tag-message-extents (msg)
  "Meant to go in the zephyr-insert-hook-list"
  (let* ((msg-begin (cdr (assq 'msg-begin msg)))
	 (body-begin (cdr (assq 'body-begin msg)))
	 (msg-end (cdr (assq 'msg-end msg)))
	 (instance (cdr (assq 'instance msg)))
	 (cir    (zephyr-property-assq msg 'class 'instance 'recipient))
	 (cr     (zephyr-property-assq msg 'class 'recipient))
	 (sender (zephyr-property-assq msg 'sender)))
    (add-nonduplicable-text-properties msg-begin msg-end
			 `(invisible (,cir ,cr ,sender) zephyr-msg-begin ,msg-begin zephyr-msg-end ,msg-end zephyr-body-begin ,body-begin zephyr-instance ,instance))
    (when zephyr-buffer-read-only 
      (add-nonduplicable-text-properties msg-begin msg-end '(read-only t))))
  msg)

(defun zephyr-get-field-sym (field &optional loc)
  (let ((pos (or loc (point)))
	(n (case field
	     ('class-instance 0)
	     ('class 1)
	     ('sender 2))))
    (nth n (get-text-property pos 'invisible))))

(defun zephyr-hide-class-at-point (&optional loc)
  (interactive)
  (zephyr-hide-sym (zephyr-get-field-sym 'class loc))
  (zephyr-goto-next))

(defun zephyr-hide-instance-at-point (&optional loc)
  (interactive)
  (zephyr-hide-sym (zephyr-get-field-sym 'class-instance loc))
  (zephyr-goto-next))

(defun zephyr-hide-sender-at-point (&optional loc)
  (interactive)
  (zephyr-hide-sym (zephyr-get-field-sym 'sender loc))
  (zephyr-goto-next))

(defun zephyr-get-field (field &optional loc)
  (if (eq field 'instance)
      (get-text-property (or loc (point)) 'zephyr-instance)
    (let* ((sym-str (symbol-name (zephyr-get-field-sym 
				  (if (eq field 'sender) 'sender 'class-instance) loc)))
	   (form (car (read-from-string sym-str 7))))
      (cdr (assq field form)))))

(defun zephyr-pop-last-hide ()
  (interactive)
  (let ((oldval (car buffer-invisibility-spec)))
    (setq buffer-invisibility-spec (cdr buffer-invisibility-spec))
    (message (format "Popped hiding %s" oldval))))

(defun zephyr-received-p ()
  (and
   (get-text-property (point) 'zephyr-msg-begin)
   (not (invisible-char-p))))

; the following may not work on all setups
; but attempts to fight the hidden nature with {previous,next}-line
; (assumes line movement ignores invisible)
(defun zephyr-goto-prev (&optional location)
  (interactive)
  (goto-char (or location (point)))

  (let (moved)
    (while 
	(and (not (bobp))
	     (not (zephyr-received-p)))
      (goto-char (or (previous-single-property-change (point) 'zephyr-msg-begin)
		     (point-min)))
      (setq moved t)
      (unless (bobp)
	(backward-char 1)))

    (unless (or moved (bobp))
      (goto-char (get-text-property (point) 'zephyr-msg-begin))
      (unless (bobp)
	(backward-char 1))
      
      (while 
	  (and (not (bobp))
	       (not (zephyr-received-p)))
	(goto-char (or (previous-single-property-change (point) 'zephyr-msg-begin)
		       (point-min)))
	(unless (bobp)
	  (backward-char 1)))))
  (get-text-property (point) 'zephyr-msg-begin))

(defun zephyr-goto-next (&optional location)
  (interactive)
  (goto-char (or location (point)))

  (let (moved)
    (while 
	(and (not (eobp))
	     (not (zephyr-received-p)))
	     
      (goto-char (or (next-single-property-change (point) 'zephyr-msg-end)
		     (point-max)))
      (setq moved t))

    (unless moved
      (goto-char (get-text-property (point) 'zephyr-msg-end))
      
      (while 
	  (and (not (eobp))
	       (not (zephyr-received-p)))
	(goto-char (or (next-single-property-change (point) 'zephyr-msg-end)
		       (point-max))))))
  (get-text-property (point) 'zephyr-msg-end))

(defun zephyr-goto-last-or-here (&optional location)
  (interactive)
  (goto-char (or location (point)))
  (let ((beg (get-text-property (point) 'zephyr-msg-begin)))
    (if beg
	(goto-char beg)
      (zephyr-goto-prev))))

(defun zephyr-quote-message (&optional location string)
  "Quote the zephyr at LOCATION.  
If LOCATION is nil, then use message at point.  If the point is in the 
sending area, will use the previous message. Inserts sender with
format specified in zephyr-reply-format-string"
  (interactive)
  (zephyr-goto-last-or-here location)
  (let* ((begin (get-text-property (point) 'zephyr-body-begin))
	 (end (get-text-property (point) 'zephyr-msg-end))
	 (zephyr-cut (buffer-substring begin end))
	 (sender (zephyr-alias-realm (zephyr-get-field 'sender)))
	 insert-point)
    (goto-char (point-max))
    (if (not (bolp))
	(if (eobp)
	    (insert "\n")
	  (progn (forward-line) (beginning-of-line))))
    (setq insert-point (point))
    (when zephyr-reply-format-string
      (insert (format zephyr-reply-format-string sender)))
    (let ((location (point)))
      (insert zephyr-cut)
      (if (bolp)
	  (forward-line -1)
	(beginning-of-line))
      (string-rectangle location (point) zephyr-quote-string))
    (goto-char insert-point)))

(provide 'zephyr-props)

