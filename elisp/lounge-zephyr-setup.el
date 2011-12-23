; cruft to prevent cycles from earlier setups that involved
; loading this file out of zephyr-options
(if (boundp 'lounge-zephyr-setup)
    (error "lounge-zephyr-setup loaded twice. Check your config."))
(setq lounge-zephyr-setup t)

(require 'zephyr)
(require 'zlocate-finger)
(require 'zephyr-logging)
(require 'zephyr-style-strip)
(require 'magus-macs)
(require 'zephyr-quote)
(require 'zephyr-zwgc)
(require 'zephyr-props)
(require 'zlocate)
(require 'zlocation)
(require 'zml-editing)

(defvar zephyr-anyone-file (if (file-exists-p "~/.anyone") "~/.anyone" nil) "Location of .anyone. nil prevents loading")
(defvar zephyr-signatures-file (if (file-exists-p "~/.zsigs") "~/.zsigs" nil) "Location of .zsigs. nil prevents loading")
(defvar zephyr-use-zephyr-subs (if (file-exists-p "~/.zephyr.subs.tzc") "~/.zephyr.subs.tzc" nil) "Should we load the subs from .zephyr.subs.tzc? nil prevents loading.")
(defvar zwatch-start t "*Maintain zwatch or not?")
(defvar zephyr-default-realm "@ANDREW.CMU.EDU" "*realm.. this is set later, but we need it NOW")
(defvar zephyr-sig-announce nil "*Announce signature after sending.")
(defvar zephyr-public-on-top nil "public buffer on top or not")
(defvar zephyr-away nil "*Are you away?")
(defvar zephyr-away-msg "Sorry, I can't answer your zephyr right now." "*message to send to aways")
; (defvar zephyr-auto-away-msg "Sorry, I seem to be inexplicably idle at the moment." "*message to send when idled out")
(defvar zephyr-auto-away-timeout 1200 "*timelimit before you are away. nil to disable auto-away")
(defvar zephyr-away-msg-stub "\n\nThis is a recording." "*The stub that is added to the away msg")
(defvar zwatch-buffer t "*Should I bother displaying zwatch foo?")
(defvar zephyr-style-emulation t "try emulating zwgc styles?")
(defvar zephyr-zap-in-public nil "*zap your own zephyrs in public window after sending? NOTE: zephyr-buffer-read-only MUST be off for this to work. It has been deprecated, recmmoned using zephyr-hide-in-public.")
(defvar zephyr-hide-self-in-public t "*hide your outgoing in public buffer")
(defvar zephyr-sender-instance-regexp (concat "^\\([^(]*\\)\\((\\([^)]*\\))\\)*"))
(defvar zephyr-received-match-regexp (concat "^\\([^(]*\\)\\(([^)]*)\\)*\\(!!!\\)?" (regexp-quote zephyr-receive-divider))
  "how do I split the info out of a received zephyr?")
(defvar zephyr-spell-check nil "*Call ispell for each zephyr you send out?")

(defvar zephyr-who () "*Who do I care about? (loaded automagically if you set a .anyone)")
(defvar zephyr-signatures () "*Signatures (loaded automatically if you set a zsigs file)")
(defvar zephyr-strip-excess-whitespace t "*Remove ending whitespace on outgoing messages")
(defvar zephyr-strip-excess-whitespace-received t "*Remove ending whitespace on incoming messages")
(defvar zephyr-before-send-and-compose-hook '(zephyr-strip-excess-whitespace-hook) "*Hook to call before I call send and compose")
(defvar zephyr-load-sane-faces t "Load the faces the maintainer has decided are sane.")
(defvar zephyr-use-ssh nil
  "*Set to t in order to ssh to another machine and run a tzc there.
You'll also want to set zephyr-ssh-host (the remote host), zephyr-ssh-userid
(username on that host), and  zephyr-ssh-tzc (path to the tzc execuable), 
each one a simple string.  Additionally, sshd on the remote host MUST grant
a kerb tgt on login, or tzc will fail miserably.")
(defvar zephyr-show-dates nil "Set to t to show dates in banner")
(defvar zephyr-default-hostname nil "*Where do we want to appear to be from? (hostname)")
(defvar zephyr-default-tty nil "*Where do we want to appear to be from? (tty)")
(defvar zephyr-heartbeat-period 0 "Heartbeat rate. 0 disables. tzc default is 45, not recommended much less.")
(defvar zephyr-one-liners t "Show zephyrs of one line on the same line (if possible)")
(defvar zephyr-log-path "~/zephyr-logs" "Store logs where?")
(defvar zephyr-log-private t "Log private zephyrs?")
(defvar zephyr-log-public nil "Log all public zephyrs?")
(defvar zephyr-hide-signatures t "Hide signatures? Only affects startup. You can hide and unhide signatures at any time using zephyr-hide-signatures and zephyr-show-signatures")
(defvar zephyr-private-ratio 0.3 "*How much should be devoted to private zephyrs? (0.0 to 1.0)")
;(defvar zephyr-class-history ())
;(defvar zephyr-instance-history ())
;(defvar zephyr-recipient-history ())

(if (fboundp 'fsf/x) nil
  (progn
    (if (boundp 'user-in-xemacs) nil
      (defvar user-in-xemacs (if (string-match "XEmacs" emacs-version) 
				 t nil)))
    (defmacro fsf/x (fsf x) `(if user-in-xemacs ,x ,fsf)))
  )

(defun zephyr-face-re (inside)
  (concat "^[-a-zA-Z0-9_.@]+" inside "!*\\(»\\|>>>\\) \\[.*\\]"))

(defun zephyr-setup-setqs ()
  (setq tzc-binary "tzc"
	zephyr-unauth-string "!!!"
	zephyr-buffer-limit nil
	zephyr-log-buffer-limit 50000
	zephyr-beep-personal-only t
	zephyr-deiconify-on-receive t
	zephyr-lazy-beep-time 1
	zephyr-send-function 'zephyr-send-show-multiple
	zephyr-opcodes-ignore-list t
	zephyr-warn-empty-message t
	zephyr-send-divider "<<< \n"
	zephyr-send-divider-regexp "<<< *\n?"
	zephyr-failed-string "[FORGED]"
	zephyr-exposure "NET-ANNOUNCED"
	
	zephyr-hook-list
	'(zephyr-parse
	  zephyr-dispatch
	  zephyr-setup-parse-fields
	  zephyr-setup-initial-add-banner
	  zephyr-style-strip-signature
	  zephyr-style-strip-opcode
	  zephyr-zwgc-signature
	  zephyr-setup-final-add-banner
	  zephyr-munge-cyrus
	  zephyr-away-hook
	  zephyr-insert)

	zephyr-insert-hook-list
	'(zephyr-ping-notify
	  zephyr-insert-message
	  zephyr-make-message-extents
	  zephyr-touch
	  zephyr-notify-with-message
	  zephyr-notify-with-deiconify
	  zephyr-notify-with-beep
	  ;zephyr-history-hook
	  zephyr-tag-message-extents
	  zephyr-tag-signature
	  zephyr-reset-history-pos-hook
	  zephyr-zwatch-tickle-hook
	  zephyr-log-incoming
	  )
	
	zephyr-receive-divider "» "
	zephyr-receive-divider-regexp "\\(»\\|>>>\\) \\[.*\\]\n?"
	
	zephyr-realm-aliases '(("CS" . "CS.CMU.EDU") 
			       ("ECE" . "ECE.CMU.EDU") 
			       ("AND" . "ANDREW.CMU.EDU") 
			       ("ANDREW" . "ANDREW.CMU.EDU")
			       ("CLB" . "CLUB.CC.CMU.EDU")
			       ("CLUB" . "CLUB.CC.CMU.EDU")
			       ("DEM" . "DEMENTIA.ORG") 
			       ("DEMENTIA" . "DEMENTIA.ORG") 
			       ("SNU" . "SNURGLE.ORG") 
			       ("SNURGLE" . "SNURGLE.ORG") 
			       ("KEP" . "THEKEEP.ORG") 
			       ("THEKEEP" . "THEKEEP.ORG") 
			       ("WAT" . "WATSON.ORG") 
			       ("WATSON" . "WATSON.ORG") 
			       ("ATH" . "ATHENA.MIT.EDU")
			       ("ATHENA" . "ATHENA.MIT.EDU")
			       ("ZON" . "ZONE.MIT.EDU")
			       ("SIM" . "ARTEMIS.SIMMONS.EDU")
			       ("FCK" . "FSCK.COM")
			       ("G17" . "GREY17.ORG")
			       ("HAR" . "EECS.HARVARD.EDU")
			       ("HARVARD" . "EECS.HARVARD.EDU")
			       ("IAS" . "IASTATE.EDU")
			       )

	zephyr-faces '(('personal       "red")
		       ('outgoing       "cyan"          "dodgerblue")
		       ("MAIL:.*"       "red")
		       ("LOGIN:.*"      "orange"        "orange")
;		       ("cslounge:.*"   "brightgreen"   "green")
;		       (".*@AND"        "yellow")
;		       (".*@CS"         "brightmagenta" "orange")
;		       (".*@ECE"        "green"         "cyan")
;		       (".*@[A-Z.]+"    "brightyellow"  "magenta")
;		       (".*"            "red"           "cyan")
		       )
	
	zephyr-additional-faces nil)

  (add-hook 'zephyr-before-send-hook 'zephyr-signature-hook t)
  (add-hook 'zephyr-after-send-hook 'zephyr-sig-announce-hook t)
  (add-hook 'zephyr-after-send-hook 'zephyr-tag-outgoing-extents t)
  (add-hook 'zephyr-after-send-hook 'zephyr-log-outgoing t)
  (add-hook 'zephyr-after-send-hook 'zephyr-reset-history-hook t))

(defun zephyr-define-face (match color &optional bcolor &optional xcolor &optional bxcolor)
  (let* ((mstr (case match
		('outgoing "^[^\n«»>]+<<<")
		('personal (zephyr-face-re ""))
		(t (zephyr-face-re (concat "(" match ")")))))
;	 (sym (make-symbol (concat "zephyr-face-" (replace-in-string mstr "[^A-Za-z]" "-"))))
	 (sym (intern (concat "zephyr-face-" (replace-in-string mstr "[^A-Za-z]" "-"))))
	 (xc  (or xcolor color))
         (bxc (or bxcolor bcolor)))
    (setq zephyr-font-lock-keywords 
	(nconc zephyr-font-lock-keywords
	       (list (cons mstr sym))))
    (eval `(defface ,sym '((((type tty))
			    (:foreground ,color :background ,bcolor))
			   (t (:foreground ,xc :background ,bxc))) (concat "zephyr matching " mstr)))))

(defun zephyr-setup-init-faces ()
  "Initializes the zephyr font faces."

  (require 'font-lock)

  (setq-default font-lock-maximum-decoration t)
  (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)

  (setq zephyr-font-lock-keywords nil)

  (mapc (lambda (x) 
	  (eval `(zephyr-define-face ,@x)))
	(nconc zephyr-additional-faces zephyr-faces)))

;;; give names to the various fields and add them to the alist.  also
;;; add the print-as tag, containing the printed rep.
;;; we convert the class to uppercase, since case is supposed to be
;;; insignificant in class names.
(defun zephyr-setup-parse-fields (msg)
  (let ((class (intern (upcase (zephyr-make-string (cdr (assq 'class msg))))))
	(urgent (string-equal (downcase (cdr (assq 'instance msg))) "urgent"))
	(sender (cdr (assq 'sender msg)))
	(opcode (intern (zephyr-make-string (cdr (assq 'opcode msg)))))
	(fields (cdr (assq 'message msg)))
	(time-secs  (cdr (assq 'time-secs msg))))
    ;;;; this is for a kludge to let emacs18 know (sort of) what time it is
    (if time-secs
	(setq zephyr-last-zgram-timestamp time-secs))
    (cond 
     ((eq class 'LOGIN)
      (let ((host (nth 0 fields))
	    (when (nth 1 fields))
	    (tty (nth 2 fields))
	    (type (cond ((eq opcode 'USER_LOGIN) "login")
			((eq opcode 'USER_LOGOUT) "logout")
			(t "<weird LOGIN opcode>"))))
	(append (list (cons 'print-as (concat ""))) msg)))
     (;; in messages, the first field is a signature, and the
      ;; second is the message body.
      ;; (We'll treat unknown classes like class 'MESSAGE)
      t 
      (let* ((len (length fields))
	     (sig (cond ((or (> len 1)
			     (and (> len 0) (not urgent)))
			 ;; strip trailing newline (multi-line sigs 
			 ;; will be truncated)
			 (string-match "\\(.*\\)\n*"
				       (car fields))
			 (zephyr-match (car fields) 1))
			(t sender)))
	     (body (cond ((eq opcode 'PING) "<PING>")
			 ;; urgent zgrams from server have no sig
			 ((and urgent (= 1 len))
			  (car fields))
			 ((= 1 len) "") ; usually just means empty body
;;; zwgc just ignores extra fields, so we will too
;;;			      ((not (= len 2)) 
;;;			       (format "malformed message: %s" fields))
			 (t (cadr fields))
			 )))
	(when zephyr-strip-excess-whitespace-received 
	  (let ((sbody (split-string body "\n")))
	    (while (and sbody (string-match "^\\\s-*$" (car sbody)))
	      (setq sbody (cdr sbody)))
	    (while (and sbody (string-match "^\\\s-*$" (car (last sbody))))
	      (setq sbody (butlast sbody)))
	    (setq body (reduce (lambda (last now)
				 (concat last now "\n")) sbody :initial-value ""))
	    (if (< 0 (length body))
		(setq body (substring body 0 (1- (length body)))))))
	(append (list (cons 'print-as body)
		      (cons 'signature sig)
		      (cons 'body body))
		msg)))
     )))

(defun zephyr-setup-fromline (msg)
  "return the from-line for MSG.  e.g. \"susan(graffiti)\""
  (let* ((inst  (zephyr-alias-realm (cdr (assq 'instance msg))))
	 (class (zephyr-make-string (cdr (assq 'class msg))))
	 (recip (zephyr-alias-realm (cdr (assq 'recipient msg))))
	 (sender (zephyr-alias-realm (cdr (assq 'sender msg))))
	 (inst-realm (if (and (> (length recip) 0)
			      (eq (aref recip 0) ?@))
			 (concat (zephyr-alias-realm inst) recip)
		       (zephyr-alias-realm inst))))
    (if (and (string-equal (upcase (zephyr-alias-realm inst)) "PERSONAL")
	     (string-equal (upcase class) "MESSAGE")
	     (zephyr-personal-msg-p msg))
	sender
      (concat 
       sender "("
       (if (string= class "LOGIN")
	   (zephyr-class-instance-string class "")
	 (zephyr-class-instance-string class inst-realm))
       ")"))))
;;;	       class (if (string= class "LOGIN") " " inst-realm) ")")))))
  
;;; add the string used to "introduce" a message.
(defun zephyr-setup-initial-add-banner (msg)
  (let ((auth (cdr (assq 'auth msg)))
	(class (cdr (assq 'auth msg))))
    (cons 
     (cons 
      'banner
      (concat (zephyr-setup-fromline msg)
	      (cond ((eq auth 'yes) "")
		    ((eq auth 'failed) zephyr-failed-string)
		    ((eq auth 'no) zephyr-unauth-string))
	      zephyr-receive-divider))
     msg)))

(defun zephyr-munge-cyrus (msg)
  "Munge zephyrgrams from Cyrus into a more pleasing and shorter form."
  (if (and (eq (cdr (assq 'class msg)) 'MAIL)
	   (string-match "^You have new mail" (cdr (assq 'print-as msg))))
      (let* ((text (cdr (assq 'print-as msg))))
	(string-match "^From: \\([^\n]*\\)" text)
	(setq sender (match-string 1 text))
	(string-match "^Subject: \\([^\n]*\\)" text)
	(setq subj (match-string 1 text))
	(setcdr 
	 (assq 'print-as msg)
	 (concat
	  (if (> (length sender) 38) (substring sender 0 38) sender) " : "
	  (if (> (length subj) 38) (substring subj 0 38) subj) ""))))
  msg)

(defun zephyr-setup-final-add-banner (msg)
  (let* ((opc (assq 'opcode msg))
	 (opcode (and (cdr opc) (symbol-name (cdr opc))))
	 (fromhost (cdr (assq 'fromhost msg)))
	 (class (cdr (assq 'class msg)))
	 (signature (cdr (assq 'signature msg)))
	 (body (cdr (assq 'body msg)))
	 (instance (zephyr-alias-realm (cdr (assq 'instance msg)))))    
    (let ((banner (assq 'banner msg))
	  (timestmp (cdr (assq 'time msg)))
	  (curl 0)
	  (sigbeg -1)
	  (sigend -1))
      (if banner
	  (setcdr 
	   banner
	   (concat 
	    (progn (setq curl (+ curl (length (cdr banner))))
		   (cdr banner))
;;;	    (if (> curl 68)
;;;		(progn (setq curl 0) "\n")
;;;	      "")
	    (progn (setq curl (+ curl (if zephyr-show-dates 16 9)))
		   (concat "[" (substring timestmp (if zephyr-show-dates 4 11) 19)))
;;;	    (if (> (+ curl (length signature) 1) 79)
;;;		(progn (setq curl 0) "\n")
;;;	      (progn (setq curl (1+ curl)) " "))
	    (if (string= class "LOGIN")
		(concat 
		 (progn (setq curl (1+ curl)) " ")
		 (if (> (+ curl (length opcode) 3) 79)
		     (progn (setq curl 0) "\n")		   
		   ;;		   (progn (setq curl (1+ curl)) " ")
		   )
		 (downcase fromhost) " "
		 (progn (setq curl (+ 2 (length opcode) curl))
			(if (string-match "LOGIN" opcode) "LOGIN" "LOGOUT"))
		 "]")
	      (concat 
	       (progn (setq sigbeg curl)
		      (setq curl (+ curl 1 (length signature)))
		      (concat " " signature))
	       (if (string-match "nil" opcode) ()
		 (concat
;;;		  (if (> (+ curl (length opcode) 3) 79)
;;;		      (progn (setq curl 0) "\n")
;;;		    (progn (setq curl (1+ curl)) " "))
		  "::"
		  (progn (setq curl (+ 2 (length opcode) curl)) opcode)))
	       (progn
		 (setq sigend curl)
		 "")

;;	       "]\n")

	       (let ((sbody (split-string body "\n")))
		 (if (and 
		      zephyr-one-liners
		      (= 1 (length sbody))
		      (< (+ curl (length (car sbody)) 3 (- sigbeg sigend)) (window-width)))
		     "] " "]\n")))))))
      (append (list (cons 'sig-begin sigbeg)
		    (cons 'sig-end sigend))
	      msg))))

(defun zephyr-log-incoming (msg)
  (if (zephyr-personal-msg-p msg)
      (when zephyr-log-private
	(make-directory (concat zephyr-log-path "/personal") t)
;	(message (format "%s:%s:%s" msg (assq 'msg-begin msg) (assq 'msg-end msg)))
	(append-to-file 
	 (cdr (assq 'msg-begin msg))
	 (cdr (assq 'msg-end msg))
	 (concat zephyr-log-path "/personal/" (zephyr-alias-realm (cdr (assq 'sender msg)))))))
  msg)

(defun zephyr-log-outgoing (msg recips)
  (mapc (lambda (recip)
	  (let* ((class (car recip))
		 (instance (car (cdr recip)))
		 (to (car (cdr (cdr recip)))))
	    (when (and (string= class "MESSAGE")
		       (string= instance "PERSONAL")
		       zephyr-log-private)
	      (make-directory (concat zephyr-log-path "/personal") t)
	      (with-temp-buffer 
		(insert (concat to "<<<" "\n" msg))
		(append-to-file
		 (point-min)
		 (point-max)
		 (concat zephyr-log-path "/personal/" (zephyr-alias-realm to)))))))
	recips)
  msg)

(defun zephyr-other-frame () (interactive)
  (select-frame (make-frame))
  (zephyr-new-buffer))

(defun zephyr-signature-hook (-- --)
  (when zephyr-signatures
    (setq zephyr-signature (nth (random (length zephyr-signatures)) zephyr-signatures))))

(defun zephyr-sig-announce-hook (-- --)
  (when zephyr-sig-announce
    (goto-char (point-max))
    (insert (concat "\n[" (substring (current-time-string) 11 19) "] SIG: [" zephyr-signature "]\n"))))

(defun zephyr-tag-signature (msg)
  (let ((mb    (cdr (assq 'msg-begin msg)))
	(start (cdr (assq 'sig-begin msg)))
	(end   (cdr (assq 'sig-end msg))))
    (when (<= 0 start)
      (let ((new (append '(signature)
			(get-text-property (+ mb start) 'invisible))))
	(add-nonduplicable-text-properties (+ mb start) (+ mb end)
					   `(invisible ,new)))))
  msg)

(defun zephyr-away-hook (msg)
  (when zephyr-away
    (zephyr-auto-reply msg))
  msg)

(defun ispell-zephyr-hook ()
  (let* ((end (previous-single-property-change (point-max) 'zephyr-msg-end))
	 (mrk (make-marker)))
    (save-excursion
      (save-restriction
	(goto-char (or end (point-min)))
	(re-search-forward zephyr-send-divider-regexp nil t)
	(set-marker mrk (point))
	(ispell-region mrk (point-max-marker))))))

(defun file-read-lines (file)
  (save-excursion
    (let ((tmpbuf (get-buffer-create "*temp*")))
      (set-buffer tmpbuf)
      (erase-buffer) 
      (insert-file-contents file)
      (prog1 
	  (split-string (replace-in-string (buffer-string) "\n\\'" "") "\n")
	(kill-buffer tmpbuf)))))

(defun zephyr-implicit-realm (user realm)
  (let* ((usr (zephyr-unalias-realm user))
	 (match (string-match "\@.*" usr)))
      (if match
	  (cons usr (match-string 0 usr))
	(cons (concat usr realm) realm))))

(defun zephyr-anyone ()
  (setq zephyr-who 
	(mapcar (lambda (user) 
		  (let* ((usrc (zephyr-implicit-realm user zephyr-default-realm))
			 (usr (car usrc))
			 (rlm (cdr usrc)))
		    (setq zephyr-extra-subscriptions (nconc zephyr-extra-subscriptions
							     (list (list "LOGIN" usr rlm))))
		    (downcase usr)))
		(file-read-lines zephyr-anyone-file))))

(defun zephyr-zsigs ()
  (save-excursion
    (let ((tmpbuf (get-buffer-create "*zsig-temp*")))
      (set-buffer tmpbuf)
      (erase-buffer) 
      (insert-file-contents zephyr-signatures-file)
      (goto-char (point-min))
      (setq beg (point-min))
      (while (< (forward-line 1) 1)
	(setq tmp (buffer-substring beg (- (point) 1)))
	(setq zephyr-signatures (append zephyr-signatures (list tmp)))
	(setq beg (point)))
      (kill-buffer tmpbuf))))

(defun zwatch-start ()
  (interactive)
  (or zephyr-zwatch-list 
      (setq zephyr-zwatch-list zephyr-who))
  (with-current-buffer (get-buffer-create "*zw*")
    (zephyr-zwatch-new-buffer)))

(defun zephyr-go-away ()
  (interactive)
  (setq zephyr-away t)
  (setq-default modeline-process " A W A Y")
  (message (concat "zephyr away set, message: " zephyr-away-msg)))  

(defun zephyr-come-back ()
  (interactive)
  (setq zephyr-away nil)
  (setq-default modeline-process nil)
  (message "zephyr away unset"))

(defun zephyr-toggle-away ()
  (interactive)
  (if zephyr-away 
      (zephyr-come-back)
    (zephyr-go-away)))

(defun zephyr-auto-away ()
  (interactive)
  (zephyr-go-away)
  (while (sit-for 600))
  (zephyr-come-back))

(defun zephyr-edit-away-msg ()
  (interactive)
  (setq zephyr-away-msg (read-string "New message: " zephyr-away-msg))
  (message (concat "Away message: " zephyr-away-msg)))

(defun zephyr-away-msg-fn (msg)
  "What reply do we want for this message?"
  (if zephyr-away
      zephyr-away-msg
    zephyr-auto-away-msg))

(defun zephyr-authenticated-p (msg)
  (let ((auth (cdr (assq 'auth msg))))
    (case auth
      ('yes t)
      (t nil))))

(defun zephyr-auto-reply (msg)
  "Send the message in zephyr-auto-reply-msg to anyone who sends 
you a personal zgram. Breaks loops by ignoring unauth and sending unauth, 
ala zaway 

You could put this function in zephyr-hook-list, just before zephyr-insert,
to auto-reply to zephyrs.  Modify to taste."
  (let* ((sender    (cdr (assq 'sender msg)))
	 (opcode    (zephyr-make-string (cdr (assq 'opcode msg)))))
    (if (and (zephyr-personal-msg-p msg)
	     (zephyr-authenticated-p msg)
	     (or (string-equal "" opcode)
		 (string-equal "nil" opcode)))
	(progn
	  (message "sending away message")
	  (let ((zephyr-kerberos-authenticate nil))
	    (funcall zephyr-send-function
		     (concat 
		      (zephyr-away-msg-fn msg)
		      zephyr-away-msg-stub)
		     (list (list "MESSAGE" "PERSONAL" sender)))))))
  msg)

(defun zephyr-auto-reply-old (msg)
  "Send the message in zephyr-auto-reply-msg to anyone who sends 
you a personal zgram.  Won't send to the same sender twice in 60 
seconds (zephyr-auto-reply-timeout), to prevent loops.

You could put this function in zephyr-hook-list, just before zephyr-insert,
to auto-reply to zephyrs.  Modify to taste.

Doesn't work in emacs18."
  (let* ((sender    (cdr (assq 'sender msg)))
	 (opcode    (zephyr-make-string (cdr (assq 'opcode msg))))
	 (last-send (assoc sender zephyr-auto-reply-users))
	 (now       (zephyr-current-time)))
    (if (and (or (null last-send)
		 (> (zephyr-time-diff (cdr last-send) now) 
		    zephyr-auto-reply-timeout))
	     (zephyr-personal-msg-p msg)
	     (or (string-equal "" opcode)
		 (string-equal "nil" opcode)))
	(progn
	  (message "sending away message")
	  (funcall zephyr-send-function
		   (concat 
		    (zephyr-away-msg-fn msg)
		    zephyr-away-msg-stub)
		   (list (list "MESSAGE" "PERSONAL" sender)))
	  (if last-send
	      (setcdr last-send now)
	    (setq zephyr-auto-reply-users (cons (cons sender now) 
						zephyr-auto-reply-users))))))
  msg)

(defun zephyr-delete-messages-from (inst)
  "delete all messages on a particular instance or sender that appear after
point.  takes a regexp.  (The function's name is somewhat deceptive.)"
  (interactive "sInstance/Sender name (regexp): ")
  (let* ((kill (concat "\\("
		       inst		; takes a regexp
		       "\\).*"
		       zephyr-receive-divider-regexp))
	 (any-divider-regexp (concat "\\("
				     zephyr-receive-divider-regexp
				     "\\|"
				     zephyr-send-divider-regexp
				     "\\)")))
    (while (and (not (eobp))
		(re-search-forward kill nil t)) 
      (goto-char (match-beginning 0))
      (let ((p (point))
	    (found (re-search-forward any-divider-regexp nil t 2)))
	(goto-char (match-beginning 0))
	(beginning-of-line 1)
	(if found 
	    (delete-region p (point))
	  (goto-char (match-end)))))))

(defun str-assq (prop alist)
  (format "%s" (cdr (assq prop alist))))

;(defun zephyr-history-hook (msg)
;  (setq zephyr-class-history (cons (format "%s" (cdr (assq 'class msg))) zephyr-class-history))
;  (setq zephyr-instance-history (cons (format "%s" (cdr (assq 'instance msg))) zephyr-instance-history))
;  (setq zephyr-recipient-history (cons (format "%s" (cdr (assq 'recipient msg))) zephyr-recipient-history))
;  msg)

(defun zephyr-zwgc-signature (msg)
  (if zephyr-style-emulation
      (let ((sig (assq 'signature msg)))
	(if (and sig (stringp (cdr sig)))
	    (let (before-change-functions after-change-functions)
	      (setcdr sig (zephyr-zwgc-fontify-string (cdr sig)))))))
  msg)

(defun zephyr-strip-excess-whitespace-hook ()
  (when zephyr-strip-excess-whitespace
    (save-excursion
      (goto-char (point-max))
      (let (len)
	(while (or (< 0 (setq len (length (thing-at-point 'whitespace)))) (bolp))
	  (delete-backward-char (if (bolp) 1 len) )))
      (insert "\n"))))
  
(defadvice zephyr-send-and-compose (around zml-hooked first activate)
  (run-hook-with-args 'zephyr-before-send-and-compose-hook)

  (if zephyr-spell-check
      (if (ispell-zephyr-hook)
	  (progn
	    ad-do-it))	    
    ad-do-it))

(defun zephyr-alias-class-realm (str)
  (if str
      (if (and (string-match zephyr-sender-instance-regexp str)
	       (> (length (match-data)) 4))
	  (format "(%s)" (zephyr-alias-realm (match-string 3 str)))
	(zephyr-alias-realm str))
    nil))

(defadvice zephyr-touch-name (before alias-realms first activate)
  (ad-set-arg 0 (zephyr-alias-class-realm (ad-get-arg 0))))

(defun zephyr-sub (class inst recip)
  (interactive "sClass: \nsInstance: \nsRecipient: ")

  (zephyr-subscribe (list (list class inst recip))))

(defun zml-format-regex (var &optional exact)
  (concat "^" (regexp-quote (format "%s" var))
	  (if exact "$" "")))

(defun zephyr-ignore-class-instance-recip-temporarily (class instance realm mins)
  (let* ((int-mins (cond ((stringp mins)
			  (if (string= mins "")
			      nil
			    (string-to-int mins)))
			 ((integerp mins) mins)
			 (t mins)))
	 (timeout (if int-mins
		      (list
		       (zephyr-add-secs (zephyr-current-time)
					(* 60 int-mins)))
		    nil)))
    (setq zephyr-filters
	  (append (list (append (list 
				 (cons `class (or class ".*"))
				 (cons `instance (or instance ".*"))
				 (cons `recipient (or realm ".*"))
				 nil) timeout))
		  zephyr-filters))))

(defun zephyr-ignore-class-instance-realm ()
  (interactive)
  (let* ( (class-str (completing-read "Class (defaults to message, * for all)? "))
	  (instance-str (completing-read "Instance (* for all)? "))
	  (realm-str (completing-read (concat "Realm (defaults to " zephyr-realm ")? ") zephyr-realm-aliases))
	  (mins (read-string "How long (minutes) [forever]? "))
	  (class (if (string="*" class-str) ".*" 
		   (zml-format-regex (if (string= "" class-str) "MESSAGE" class-str) t)))
	  (instance (if (string= "*" instance-str) ".*" 
		      (zml-format-regex instance-str)))
	  (recip-u (zephyr-unalias-realm 
		    (concat "@" (if (string= "" realm-str) zephyr-realm realm-str))))
	  (recip (zml-format-regex (if (string= recip-u (concat "@" zephyr-realm)) "" recip-u) t)))
    (zephyr-ignore-class-instance-recip-temporarily class instance recip mins)))

(defun zephyr-ignore-last ()
  (interactive)
  (save-excursion
    (zephyr-goto-last-or-here)
    (if (zephyr-received-p)
	(progn
	  (let* ((class (zephyr-get-field 'class))
		 (instance (zephyr-get-field 'instance))
		 (recip (zephyr-get-field 'recipient))
		 (class-str (zml-format-regex class t))
		 (instance-str (zml-format-regex instance))
		 (recip-str (zml-format-regex recip t))
		 (mins (read-string (format "Ignore (%s:%s%s) how long, in minutes [forever]? " class instance recip))))
	    (zephyr-ignore-class-instance-recip-temporarily class-str instance-str recip-str mins)
	    (message (format "(%s:%s%s) ignored." class instance recip))))
      (error "No zephyr found."))))
  
(defun zephyr-ignore-last-class ()
  (interactive)
  (interactive)
  (save-excursion
    (zephyr-goto-last-or-here)
    (if (zephyr-received-p)
	(progn
	  (let* ((class (zephyr-get-field 'class))
		 (recip (zephyr-get-field 'recipient))
		 (class-str (zml-format-regex class t))
		 (recip-str (zml-format-regex recip t))
		 (mins (read-string (format "Ignore class %s%s how long, in minutes [forever]? " class recip))))
	    (zephyr-ignore-class-instance-recip-temporarily class-str nil recip-str mins)
	    (message (format "class %s%s ignored." class recip))))
      (error "No zephyr found."))))      

(defun zephyr-pop-last-filter ()
  (interactive)
  (if (y-or-n-p (format "Pop %s? " (car zephyr-filters)))
      (let ((popped (pop zephyr-filters)))
	(message "Just popped %s" popped))))
  

(defun zephyr-zap-last (&optional dont care)
  "zap the last zephyr.. meant to go in after-send-hook..
badly written at the moment. :)"
  (interactive)
  (let* ((end (previous-single-property-change (point-max) 'zephyr-msg-end)))
    (delete-region (or end (point-min)) (point-max))))

(defun zephyr-goto-instance (&optional pos)
  (interactive)
  (let* ((p (or pos (point-max)))
	 (div (previous-single-property-change (point-max) 'zephyr-msg-end))
	 (--  (goto-char (or div (point-min))))
	 (ins (re-search-forward "^[^(]*([^:]+:" nil t)))
    (or ins
	(re-search-forward "^[^(]*(" nil t))))
	
(defun zephyr-goto-instance-and-zap (&optional pos)
  (interactive)
  (let ((start (zephyr-goto-instance pos)))
    (if start
	(if (re-search-forward "@\\|)" nil t)
	    (progn
	      (delete-region start (match-beginning 0))
	      (goto-char start))))))

(defun zephyr-parse-sender-recip (str)
  (if (string-match zephyr-received-match-regexp str)
      (list (match-string 1 str) (match-string 2 str))
    (list nil nil)))

(defun zephyr-infer-to (class instance recip sender)
  (if (string= " outgoing " class)
      (zephyr-alias-realm sender)
    (if (string= "login" class)
	(zephyr-alias-realm instance)
      (if (or (string= "" recip)
	      (string-match "^\\@.*$" recip))
	  (format "(%s%s)"
		  (if (string= class "message")
		      instance
		    (format "%s:%s" class instance))
		  (let ((def (concat "@" (downcase zephyr-realm))))
		    (if (string= recip def)
		      ""
		      (zephyr-alias-realm recip))))
	(zephyr-alias-realm sender)))))

(defun zephyr-infer-to-at (location)
  (let ((p (or location (point))))
    (zephyr-infer-to 
     (zephyr-get-field 'class (point))
     (zephyr-get-field 'instance (point))
     (zephyr-get-field 'recipient (point))
     (zephyr-get-field 'sender (point)))))

(defvar zephyr-history-pos nil)
(defvar zephyr-history-below nil)

(defun zephyr-history-goto-prev ()
  (interactive)
  (unless zephyr-history-pos
    (zephyr-goto-last-or-here (point-max)))
  (let (to)
    (while (and (not (bobp))
		(or (and 
		     (zephyr-received-p)
		     (member
		      (setq to (zephyr-infer-to-at (point)))
		      zephyr-history-below))
		    (string-match zephyr-history-ignore-regexp (or to ""))))
      (zephyr-goto-prev))
    (if (and to (member to zephyr-history-below))
	(setq to nil))
    (when to
      (setq zephyr-history-below (cons to zephyr-history-below)))
    to))

(defun zephyr-compose-to-prev ()
  (interactive)
  (when zephyr-history-pos (goto-char zephyr-history-pos))
  (let ((to (zephyr-history-goto-prev)))
    (when to
      (setq zephyr-history-pos (point))
      (zephyr-zap-last)
      (zephyr-compose to)))
  (goto-char (point-max)))
  
(defun zephyr-compose-to-next ()
  (interactive)
  (let ((next (car (cdr zephyr-history-below))))
    (when (cdr zephyr-history-below)
      (setq zephyr-history-below (cdr zephyr-history-below)))
    (setq zephyr-history-pos nil)
    (when next
      (zephyr-zap-last)
      (zephyr-compose next)))
  (goto-char (point-max)))

(defun zephyr-reset-history-hook (-- --)
  (setq zephyr-history-below (list zephyr-last-recipient))
  (setq zephyr-history-pos nil))

(defun zephyr-reset-history-pos-hook (msg)
  (setq zephyr-history-pos nil)
  msg)

(defun zephyr-reply-and-yank (&optional location)
  "grab the message at point, quote it, and compose it to the correct recipient"
  (interactive)
  (let ((p (or location (point))))
    (zephyr-goto-last-or-here p)
    (let ((to (zephyr-infer-to-at (point))))

      (zephyr-zap-last) ; kill the send point
      (zephyr-compose to)
      (zephyr-quote-message p))))

(defun zephyr-hide-signatures () 
  (interactive)
  (if (not (member 'signature buffer-invisibility-spec))
      (setq buffer-invisibility-spec (append buffer-invisibility-spec (list 'signature)))))

(defun zephyr-show-signatures ()
  (interactive)
  (setq buffer-invisibility-spec (remove 'signature buffer-invisibility-spec)))

(defun zephyr-hide-self ()
  (interactive)
  (let ((sym (zephyr-intern-prop `((class . " outgoing ") (instance . " outgoing ") (recipient . ,zephyr-id)))))
    (setq buffer-invisibility-spec (append buffer-invisibility-spec (list sym)))))

(defun zephyr-show-self ()
  (interactive)
  (let ((sym (zephyr-intern-prop `((class . " outgoing ") (instance . " outgoing ") (recipient . ,zephyr-id)))))
    (setq buffer-invisibility-spec (remove sym buffer-invisibility-spec))))  

(defun zephyr-toggle-self ()
  (interactive)
  (let ((sym (zephyr-intern-prop `((class . " outgoing ") (instance . " outgoing ") (recipient . ,zephyr-id)))))
    (if (member sym buffer-invisibility-spec)
	(setq buffer-invisibility-spec (remove sym buffer-invisibility-spec))
    (setq buffer-invisibility-spec (append buffer-invisibility-spec (list sym))))))
    
(defun zephyr-personal ()
  "start a buffer for personal zephyrs."
  (interactive)
  (setq zephyr-initial-history (append zephyr-who (list (or zephyr-id (user-login-name)))))
  (zephyr-new-buffer "private-zgrams")

  (setq zephyr-zwatch-default-compose-window (get-buffer-window (current-buffer)))

  (setq zephyr-filters
	(append zephyr-filters
		(list (list (cons 'recipient
				  (concat "^" (eval 'zephyr-id)))
			    t)
		      (list (cons 'class "^LOGIN$")
			    t)
		      '(nil))))

  (setq zephyr-history-ignore-regexp (concat zephyr-history-ignore-regexp 
					     "\\|^(LOGIN:.*)$")))

(defun zephyr-public ()
  "start a buffer for public zephyrs."
  (interactive)
  (setq zephyr-initial-history (list (concat "(cslounge:" (or zephyr-id (user-login-name)) ".status@AND)")))
  (zephyr-new-buffer "public-zgrams")
  (make-local-hook 'zephyr-after-send-hook)
  
  (when zephyr-zap-in-public
    (add-hook 'zephyr-after-send-hook 'zephyr-zap-last t t))

  (when zephyr-hide-self-in-public
    (zephyr-hide-self))

  (setq zephyr-filters
	(append (list (list (cons 'recipient 
				  (concat "^" (eval 'zephyr-id)))
			    nil)
		      (list (cons 'class "^LOGIN$")
			    nil)) 
		zephyr-filters))

  (setq zephyr-history-ignore-regexp (concat zephyr-history-ignore-regexp 
					     "\\|^[^(]*$")))

(defun zephyr-dual ()
  "starts two buffers, one public, one private."
  (interactive)
  (if zephyr-public-on-top 
      (zephyr-public)
    (zephyr-personal))

  (when zwatch-start (zwatch-start))

  (if zwatch-buffer
      (let* ((w (selected-window)))
	(let* ((w2 (split-window w (- (window-width) 25) t)))
	  (set-window-buffer w2 "*zw*"))))

  (split-window nil (round (* (window-height) 
			      (if zephyr-public-on-top 
				(- 1 zephyr-private-ratio)
				zephyr-private-ratio))))

  (other-window 1)
  (if zephyr-public-on-top 
      (zephyr-personal)
    (zephyr-public))
  (other-window (if zwatch-buffer 2 1))
  t)

(defun zephyr-reset-dual ()
  "resets buffer layout for M-x zephyr-dual
mostly thnkas to broz for rewriting above."
  (interactive)

  (delete-other-windows)

  (if zephyr-public-on-top
    (switch-to-buffer "*zephyr-public-zgrams*")
      (switch-to-buffer "*zephyr-private-zgrams*"))

  (if zwatch-buffer
      (let* ((w (selected-window)))
	(let* ((w2 (split-window w (- (window-width) 25) t)))
	  (set-window-buffer w2 "*zw*"))))

  (split-window nil (round (* (window-height) 
			      (if zephyr-public-on-top 
				(- 1 zephyr-private-ratio)
				zephyr-private-ratio))))

  (other-window 1)
  (if zephyr-public-on-top 
      (switch-to-buffer "*zephyr-private-zgrams*")
    (switch-to-buffer "*zephyr-public-zgrams*"))
  (other-window (if zwatch-buffer 2 1))
  (other-window 1)
  (set-frame-size (fw-frame (selected-window)) zephyr-frame-height zephyr-frame-width)
  t)

; (setq zephyr-initial-history (append zephyr-who zephyr-id))
; (setq zephyr-notify-with-message-p t)

(defun zephyr-mode-final ()
  (auto-fill-mode 1)
  (setq fill-column 78)
  (make-variable-buffer-local 'blink-matching-paren)
  (setq blink-matching-paren t)
  (if (or (string-match "XEmacs" emacs-version)
	  (eq window-system 'x))
      (fsf/x 
       (progn
	 (make-variable-buffer-local 'font-lock-keywords)
	 (font-lock-mode)
	 (setq font-lock-keywords zephyr-font-lock-keywords))
       (progn (setq font-lock-keywords zephyr-font-lock-keywords)
	      (font-lock-mode)
	      (setq font-lock-keywords-case-fold-search t))))	   

  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate 
	(concat "\\(" paragraph-separate "\\|"
		".*" zephyr-send-divider-regexp "\\|"
		".*" zephyr-receive-divider-regexp "\\)"))

  (make-local-variable 'zephyr-history-ignore-regexp)
					;      (make-variable-buffer-local 'zephyr-class-history)
					;      (make-variable-buffer-local 'zephyr-instance-history)
					;      (make-variable-buffer-local 'zephyr-recipient-history)
  (make-variable-buffer-local 'zephyr-history-pos)
  (make-variable-buffer-local 'zephyr-history-below)
      
  (setq buffer-invisibility-spec 
	(if zephyr-hide-signatures 
	    '(signature)
	  nil))

  (make-variable-buffer-local 'buffer-invisibility-spec)

  (setq line-move-ignore-invisible t)
      
  (make-local-variable 'font-lock-always-fontify-immediately)
  (setq font-lock-always-fontify-immediately t)
					;	    (make-local-variable 'font-lock-defaults)
					;	    (setq font-lock-defaults '(zephyr-font-lock-keywords 
					;				       t nil nil nil
					;				       (font-lock-unfontify-region-function . ignore)
					;				       (font-lock-inhibit-thing-lock 
					;					. (fast-lock-mode lazy-lock-mode))))
					;	    (setq font-lock-unfontify-region-function 'ignore)
					;	    (setq font-lock-inhibit-thing-lock '(fast-lock-mode lazy-lock-mode))

  (local-set-key "\C-c\C-f" 'zephyr-zlocate-finger) 	    
  (local-set-key "\C-c\C-z" `zephyr-zwatch-repaint)
  (local-set-key "\C-c\C-i" `zephyr-ignore-last)
  (local-set-key "\C-ci"    `zephyr-hide-instance-at-point)
  (local-set-key "\C-c\C-o" `zephyr-ignore-last-class)
  (local-set-key "\C-co"    `zephyr-hide-class-at-point)
  (local-set-key "\C-c\C-m" `zephyr-toggle-self)
  (local-set-key "\C-c\C-p" `zephyr-pop-last-filter)
  (local-set-key "\C-cp"    `zephyr-pop-last-hide)
  (local-set-key "\C-c\C-y" `zephyr-quote-message)
  (local-set-key "\C-c\C-a" `zephyr-toggle-away)			      
  (local-set-key "\C-c\C-e" `zephyr-edit-away-msg)
  (local-set-key "\C-cf"    `zephyr-reply-and-yank)
  (local-set-key "\C-k"     'copy-or-kill-line)
  (local-set-key "\C-c\C-k" `zephyr-goto-instance-and-zap)
  (local-set-key "\M-q" 'force-fill-paragraph-or-region) ; I like read only. But M-q is cool.

  (local-set-key "\M-p" 'zephyr-compose-to-prev)
  (local-set-key "\M-n" 'zephyr-compose-to-next))

(defun do-zephyr-setup ()

  (if (< max-lisp-eval-depth 500)
      (setq max-lisp-eval-depth 500))

  (zephyr-setup-setqs)

  (add-hook 'zephyr-mode-hook 'zephyr-mode-final)

  (load "~/.zephyr-options" t)
  ; SIDE EFFECTS FROM OPTIONS GO *HERE*

  (zephyr-setup-init-faces)
;  (zephyr-reset-dual)

  (if zephyr-use-ssh (load "zephyr-ssh"))
  (setq zephyr-receive-program (append 
				(list tzc-binary "-e" zephyr-exposure)
;				(if zephyr-default-hostname (list "-h" zephyr-default-hostname))
;				(if zephyr-default-tty (list "-l" zephyr-default-tty))
				(if zephyr-use-zephyr-subs (list "-s"))
				(list "-t" (format "%s" zephyr-heartbeat-period))))

  (when zephyr-style-emulation
    (setq zephyr-style-strip-on nil)
    (zephyr-add-hook 'zephyr-insert-hook-list 'zephyr-zwgc-format-message 
		     'zephyr-touch))

  (when zephyr-anyone-file
    (zephyr-anyone))
  
  (when zephyr-signatures-file
    (zephyr-zsigs))

  (when zephyr-auto-away-timeout
      (start-itimer "zephyr-auto-away" 'zephyr-auto-away zephyr-auto-away-timeout zephyr-auto-away-timeout t))

  (load "~/.zephyr-post-options.el" t))

(do-zephyr-setup)

(provide 'lounge-zephyr-setup)
