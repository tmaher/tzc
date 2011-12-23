(defun markov-intern (cur str)
  (let ((sym (intern str cur)))
    (unless (ignore-errors (progn (symbol-value sym) t))
      (set sym nil))
    sym))

(defun markov-analyze-list (list)
  (if (cdr list)
      (progn
	(set (car list) (cons (car (cdr list)) (symbol-value (car list))))
	(markov-analyze-list (cdr list)))))

(defun markov-intern-list (cur list)
  (mapcar (lambda (str) (markov-intern cur str)) list))

(defun markov-analyze-phrase (cur str)
  (let* ((spl (split-string str))
	 (strp (reduce (lambda (lst str)
			 (let ((st (replace-in-string str "^\\W+\\|\\W+$" "")))
			   (if (not (string= "" st))
			       (append lst (list st))
			     lst))) spl :initial-value nil)))
    (markov-analyze-list
     (markov-intern-list cur (cons "_begin" (nconc strp (list "_end")))))))
		  
(defun markov-analyze (cur str)
  (let* ((spl (split-string (downcase str) "[.;!?]+")))
    (mapc (lambda (str)
	    (if (string-match "\\w" str)
		(markov-analyze-phrase cur str))) spl)
    nil))

(defun markov-create (chain variance)
  (let* ((start (markov-intern chain "_begin"))
	 (state start)
	 (end (markov-intern chain "_end"))
	 (str "")
	 r lst lng)
    (while (not (eq state end))
      (if (not (eq state start))
	  (setq str (concat str (if (string= str "") "" " ") (symbol-name state))))
      (setq lst (symbol-value state))
      (setq lng (length lst))
      (setq r (random (+ lng variance)))
      (if (>= r lng)
	  (progn 
	    (setq lng 0)
	    (mapatoms (lambda (atm)
			(setq lng (1+ lng))
			(if (= 0 (random lng))
			    (setq state atm))) chain))
	(setq state (nth r lst))))
    str))

(defun suck-zephyrs ()
  (let (ext prev chain done)
    (setq prev 0)
    (setq zephyr-chain (make-vector 211 0))
    (setq done nil)
    (while (not done)
      (setq ext (zephyr-extent-at-location))
      (if (= (cdr ext) prev)
	  (setq done t)
	(markov-analyze zephyr-chain (buffer-substring (car ext) (cdr ext)))
	(goto-char (cdr ext))
	(setq prev (cdr ext))))))