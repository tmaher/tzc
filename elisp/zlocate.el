;;; relies on zlocate patch to tzc

(setq zephyr-tzcspew-dispatch-table (cons 
				      '(zlocation . zephyr-register-location)
				      zephyr-tzcspew-dispatch-table))

(defvar zephyr-zlocation-hook '(zephyr-zwatch-register)
  "*list of functions to handle incoming zlocation responses")

(defvar zephyr-zwatch-list nil
  "*sorted list of people you want watched.")

(defvar zephyr-zwatch-update-time 300
  "*how often you want to check the whole list
Note: It is my intention, tho not yet impemented, to hook this
in with LOGIN/LOGOUT messages so the whole list shouldn't need
to be checked very often.")

(defvar zephyr-zwatch-itimer nil
  "current timer")

(defvar zephyr-zwatch-painted nil
  "who is currently painted in the zwatch buffer?")

(defvar zephyr-zwatch-buffer nil
  "zwatch buffer")

(defvar zephyr-zwatch-format "%10s %-3s%s\n"
  "format for each user entry. The fields passed: user name, realm, 
then the host location list, format specified
by the zephyr-zwatch-location-format")

(defvar zephyr-zwatch-location-format " %2$s"
  "*format for each host. Fields passed: last host string, host (w/o domain), FQDN, tty, time
(the last host string is passed primarily so that if you only want to display one host per person, 
you can just throw it away)")

(defvar zephyr-zwatch-faces '((zephyr-zwatch-present-face "white")
			      (zephyr-zwatch-going-face "red")
			      (zephyr-zwatch-coming-face "brightyellow")))

(defvar zephyr-zwatch-notify-delay 60
  "*zwatch notifys the user of someone who just arrived and someone who just
left and changes it to 'normal' after some delay. This sets this delay.")

(defun zephyr-register-location (msg)
  (let* ((user (downcase (cdr (assq 'user msg))))
	 (locs (cdr (assq 'locations msg))))
    (zephyr-run-hook-with-args 'zephyr-zlocation-hook msg user locs))
  nil)

(defvar zephyr-zwatch-obarray (make-vector 101 0) "obarray for zwatch status")

(defvar zephyr-zwatch-default-compose-window nil "default window where 'compose' should go")

(defun zephyr-zwatch-set (str val)
  (set (intern str zephyr-zwatch-obarray) val))

(defun zephyr-zwatch-get (str)
  (let ((sym (intern-soft str zephyr-zwatch-obarray)))
    (if sym
	(symbol-value sym)
      nil)))

(defun zephyr-zwatch-register (msg user locs)
  (let* ((str (concat "status " user))
	 (sta (zephyr-zwatch-get str)))
    (zephyr-zwatch-set str locs)
    (if (not (equal locs sta))
	(zephyr-zwatch-paint msg user locs sta))))

(defun zephyr-zwatch-paint (msg user locs oldlocs)
  (save-excursion
    (if (<= (length oldlocs) (length locs))
	(progn
	  (zephyr-zwatch-paint-delete user) 
	  (zephyr-zwatch-paint-login user locs))
      (zephyr-zwatch-paint-logout user locs)))
  (let ((win (get-buffer-window zephyr-zwatch-buffer)))
    (if win
	(set-window-point win 1))))

(defun zephyr-zwatch-painted (user) 
  "return the extent where a user is painted"
  (zephyr-zwatch-get (concat "paint " user)))

(defun zephyr-zwatch-paint-login (user locs)
  (if zephyr-zwatch-buffer
      (let ((ext (zephyr-zwatch-painted user)))
	(or ext
	    (with-current-buffer zephyr-zwatch-buffer
	      (zephyr-zwatch-paint-entry user locs))))))

(defun zephyr-zwatch-goto-sorted (user)
  (let ((users-left (cdr zephyr-zwatch-list))
	cur-user)
    (goto-char 1)
    (setq cur-user (car zephyr-zwatch-list))
    (while (and cur-user (extent-at (point)))
      (if (string= (extent-property (extent-at (point)) 'zwatch-user)
		   cur-user)
	  (goto-char (next-single-property-change (point) 'zwatch-user nil (point-max))))
      (if (string= cur-user user)
	  (setq cur-user nil)
	(setq cur-user (car users-left))
	(setq users-left (cdr users-left))))))

(defun zephyr-zwatch-paint-login-after-delay (user)
  (let ((ext (zephyr-zwatch-painted user)))
    (if (< 0 (length (zephyr-zwatch-get (concat "status " user))))
	(set-extent-face ext 'zephyr-zwatch-present-face))))

(defun zephyr-zwatch-paint-entry (user locs)
  (let* ((str (concat "paint " user))
	 ext beg-num)
    (zephyr-zwatch-goto-sorted user)
    (setq beg-num (point))
    (insert-before-markers (zephyr-zwatch-pretty-login user locs))
    (map-extents (lambda (ext --)
		   (set-extent-endpoints ext (point) (extent-end-position ext)))
		 zephyr-zwatch-buffer beg-num (point) nil nil 'zwatch-user)
    (setq ext (make-extent beg-num (point)))
    (zephyr-zwatch-set str ext)
    (set-extent-face ext 'zephyr-zwatch-coming-face)
;    (set-extent-property ext 'start-open nil)
    (set-extent-property ext 'zwatch-user user)
    (start-itimer "login" `(lambda () (zephyr-zwatch-paint-login-after-delay ,user)) zephyr-zwatch-notify-delay)))

(defun zephyr-zwatch-pretty-login (user locs)
  (let* ((usr (zephyr-alias-realm user))
	 (--  (string-match "\\([^@]+\\)@\\(.*\\)" usr))
	 (uid (match-string 1 usr))
	 (rlm (downcase (match-string 2 usr))))
    (format zephyr-zwatch-format
	    uid rlm 
	    (reduce 'zephyr-zwatch-pretty-loc locs :initial-value ""))))

(defun zephyr-zwatch-pretty-loc (last loc)
  (let* ((fqdn (downcase (cdr (assq 'host loc))))
	 (tty  (cdr (assq 'tty  loc)))
	 (time (cdr (assq 'time loc)))
	 (host (replace-in-string fqdn "\\..*" "")))
    (format zephyr-zwatch-location-format
	    last host fqdn tty time)))

(defun zephyr-zwatch-paint-logout (user locs)
  (let ((ext (zephyr-zwatch-painted user)))
    (if ext 
	(progn
	  (set-extent-face ext 'zephyr-zwatch-going-face)
	  (start-itimer "logout" `(lambda () (zephyr-zwatch-paint-logout-after-delay ,user)) zephyr-zwatch-notify-delay)))))
  
(defun zephyr-zwatch-paint-logout-after-delay (user)
  (let ((sta (zephyr-zwatch-get (concat "status " user))))
    (if (= 0 (length sta))
	(zephyr-zwatch-paint-delete user)
      (let ((ext (zephyr-zwatch-painted user)))
	(set-extent-face ext 'zephyr-zwatch-present-face)))))

(defun zephyr-zwatch-paint-delete (user)
  (let ((ext (zephyr-zwatch-painted user)))
    (when ext 
      (let ((beg (extent-start-position ext))
	    (end (extent-end-position ext)))
	(delete-region beg end zephyr-zwatch-buffer)
	(zephyr-zwatch-set (concat "paint " user) nil)
	(delete-extent ext)))))

(defun zephyr-zwatch-start ()
  (setq zephyr-zwatch-itimer
	(start-itimer "zwatch" 'zephyr-zwatch-tick 1 zephyr-zwatch-update-time)))

(defun zephyr-zwatch-stop ()
  (delete-itimer zephyr-zwatch-itimer))

(defun zephyr-zlocate-request (list)
  (zephyr-send-to-client `((tzcfodder . zlocate) ,@list)))

(defun zephyr-zwatch-tick ()
  (zephyr-zlocate-request zephyr-zwatch-list))

(defun zephyr-zwatch-repaint ()
  (interactive)
  (erase-buffer zephyr-zwatch-buffer)
  (setq zephyr-zwatch-obarray (make-vector 101 0))
  (zephyr-zwatch-tick))

(defvar zephyr-zwatch-mode-map nil "mode map for zwatch")

(unless zephyr-zwatch-mode-map
  (setq zephyr-zwatch-mode-map (make-sparse-keymap))
  (suppress-keymap zephyr-zwatch-mode-map)

  (define-key zephyr-zwatch-mode-map "f" 'zephyr-zwatch-finger)
  (define-key zephyr-zwatch-mode-map "c" 'zephyr-zwatch-compose-to))

(defun zephyr-zwatch-new-buffer ()
  "Start the zwatch in the current buffer"
  (setq zephyr-zwatch-buffer (current-buffer))

  (toggle-truncate-lines 1)

  (maplist '(lambda (x) 
	      (let ((face (car (car x)))
		    (fgcolor (cdr (car x))))
		(make-face face)
		(set face face)
		(set-face-foreground face (car fgcolor))
		))
	   zephyr-zwatch-faces)

  (erase-buffer (current-buffer))
  (buffer-disable-undo)
  (use-local-map zephyr-zwatch-mode-map)
  (zephyr-zwatch-start))

; (add-hook 'zephyr-hook-list 'zwatch-tickle-hook t)

(defun zephyr-zwatch-paint-region (beg end face)
  (let ((ext (make-extent beg end zephyr-zwatch-buffer)))
    (set-extent-face ext face)))

(defun zephyr-zwatch-paint-face (user face)
  (let* ((pnt (zephyr-zwatch-get (concat "paint " user))))
    (if (consp pnt)
	(zephyr-zwatch-paint-region (car pnt) (1+ (marker-position (cdr pnt))) face))))

(defun zephyr-zwatch-extract-at-point ()
  (if (extent-at (point))
      (extent-property (extent-at (point)) 'zwatch-user)
    (error "No user selected.")))

(defun zephyr-zwatch-finger ()
  (interactive)
  (require 'zlocate-finger)
  (let* ((usr (zephyr-zwatch-extract-at-point))
	 (user (replace-in-string usr "@.*" ""))
	 (sta (zephyr-zwatch-get (concat "status " usr))))
    (if (car sta)
	(finger-idle user (cdr (assq 'host (car sta))))
      (message "Not logged in. (?)"))))

(defun zephyr-zwatch-compose-to ()
  (interactive)
  (let ((usr (zephyr-alias-realm (zephyr-zwatch-extract-at-point))))
    (if zephyr-zwatch-default-compose-window
	(select-window zephyr-zwatch-default-compose-window))
    (zephyr-compose usr)))

(defun zephyr-zwatch-tickle-hook (msg)
  (when zephyr-zwatch-buffer
    (let ((class (cdr (assq 'class msg))))
      (if (string= class "LOGIN")
	  (zephyr-zlocate-request (list (downcase (cdr (assq 'instance msg))))))))
  msg)

(provide 'zlocate)
