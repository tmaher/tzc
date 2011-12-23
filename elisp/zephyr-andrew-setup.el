;;; Emacs Zephyr Setup File - Andrew version
;;; Last modified:  18 Aug 1999

;;; This file serves as a wrapper for the elisp zephyr libraries provided by
;;; the CMU CS Dept.  It is chiefly derived from source provided by
;;; Nat Lanza <magus@cs.cmu.edu>, with modification done by
;;; Tom Maher <tardis@ece.cmu.edu>.

;;; This file is known to work with XEmacs 20, 21, and GNU Emacs 19.

;;; HISTORY

;;; 02 Sep 1999, Tom Maher <tardis@ece.cmu.edu>
;;;    Started keeping history.
;;;    Fixed typo that broke zephyr-mail-face.
;;;    Added a default setting for zephyr-exposure: "NET-ANNOUNCED"

;;; Add the zephyr setup files to our load path
(setq load-path (cons "/usr/lib/tzc" load-path))
   
;;; Zephyr requirements
(require 'zephyr)
(require 'zlocate-finger)
(require 'zephyr-logging)
(require 'zephyr-style-strip)

(require 'magus-macs)

(defun zephyr-setup-setqs ()
  "Set up various variables"
  (setq
   zephyr-unauth-string "!!!"
   zephyr-buffer-limit nil
   zephyr-log-buffer-limit 50000
   zephyr-beep-personal-only t
   zephyr-deiconify-on-receive t
   zephyr-lazy-beep-time 1
   zephyr-send-function 'zephyr-send-show-multiple
   zephyr-opcodes-ignore-list t
   zephyr-warn-empty-message t
   zephyr-send-divider "<<<\n"
   zephyr-send-divider-regexp "<<< *\n?"
   zephyr-failed-string "[FORGED]"
   zephyr-exposure "NET-ANNOUNCED"
   tzc-binary "tzc"

   zephyr-realm-aliases 
   '(("CS" . "CS.CMU.EDU")
     ("ECE" . "ECE.CMU.EDU")
     ("andrew" . "ANDREW.CMU.EDU")
     ("dementia.org" . "DEMENTIA.ORG")
     ("snurgle.org" . "SNURGLE.ORG")
     ("thekeep.org" . "THEKEEP.ORG")
     ("watson.org" . "WATSON.ORG")
     ("athena.mit.edu" . "ATHENA.MIT.EDU"))

   zephyr-font-lock-keywords
   '(("^.*<<<.*$" . zephyr-outgoing-face)
     ("^.*(MAIL.*$" . zephyr-mail-face)
     ("^.*(LOGIN.*$" . zephyr-znol-face)
     ("^[^()\n]+>>> \\[[^]]*\\].*$" . zephyr-personal-face)
     ("^.*(.*@.+)[^\n]*>>>.*$" . zephyr-foreign-face)
     ("^.*(.*)[^\n]*>>>.*$" . zephyr-native-face)
     )
   
   zephyr-hook-list
   '(zephyr-parse
     zephyr-dispatch
     zephyr-setup-parse-fields
     zephyr-setup-initial-add-banner
     zephyr-style-strip-all
     zephyr-setup-final-add-banner
     zephyr-insert)

   zephyr-insert-hook-list
   '(zephyr-ping-notify
     zephyr-insert-message
     zephyr-make-message-extents
     zephyr-touch
     zephyr-notify-with-message
     zephyr-notify-with-deiconify
     zephyr-notify-with-beep)

   zephyr-initial-history '("(help)")
   ))

(defun zephyr-setup-init-faces ()
  "Initializes the zephyr font faces."

  (require 'font-lock)

  (setq-default font-lock-maximum-decoration t)
  (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)

  ;; NOTE: Each call to magus-face must have NO right parens in it
  ;; before the one that ends the call, otherwise zconvert will
  ;; produce garbage.  Yes, this is stupid.  Yes, I should change it.
  (magus-face 'zephyr-foreign-face :source 'default 
	      :fg "purple" :tty "magenta")
  (magus-face 'zephyr-native-face :source 'default 
	      :fg "green" :tty "brightgreen")
  (magus-face 'zephyr-personal-face :source 'default 
	      :fg "red" :tty "red")
  (magus-face 'zephyr-mail-face :source 'default 
	      :fg "orange" :tty "yellow")
  (magus-face 'zephyr-outgoing-face :source 'default 
	      :fg "dodgerblue" :tty "cyan")
  (magus-face 'zephyr-znol-face :source 'default 
	      :fg "darkgrey" :tty "grey")

  (fsf/x (setq zephyr-foreign-face 'zephyr-foreign-face) ())
  (fsf/x (setq zephyr-native-face 'zephyr-native-face) ())
  (fsf/x (setq zephyr-personal-face 'zephyr-personal-face) ())
  (fsf/x (setq zephyr-mail-face 'zephyr-mail-face) ())
  (fsf/x (setq zephyr-outgoing-face 'zephyr-outgoing-face) ())
  (fsf/x (setq zephyr-znol-face 'zephyr-znol-face) ())
  )

(defun zephyr-personal ()
  "start a buffer for personal zephyrs."
  (interactive)
  (zephyr-new-buffer "personal-zgrams")
  (setq zephyr-filters
	(append zephyr-filters
		(list (list (cons 'recipient
				  (concat "^" (user-login-name) "$"))
			    t)
		      '(nil)))))

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

(defun zephyr-setup-final-add-banner (msg)
  (let* ((opc (assq 'opcode msg))
	 (opcode (and (cdr opc) (symbol-name (cdr opc))))
	 (fromhost (cdr (assq 'fromhost msg)))
	 (class (cdr (assq 'class msg)))
	 (signature (cdr (assq 'signature msg)))
	 (instance (zephyr-alias-realm (cdr (assq 'instance msg)))))
    (let ((banner (assq 'banner msg))
	  (timestmp (cdr (assq 'time msg)))
	  (curl 0))
      (if banner
	  (setcdr 
	   banner
	   (concat 
	    (progn (setq curl (+ curl (length (cdr banner))))
		   (cdr banner))
;;;	    (if (> curl 68)
;;;		(progn (setq curl 0) "\n")
;;;	      "")
	    (progn (setq curl (+ curl 10))
		   (concat "[" (substring timestmp 11 19) "]"))
;;;	    (if (> (+ curl (length signature) 1) 79)
;;;		(progn (setq curl 0) "\n")
;;;	      (progn (setq curl (1+ curl)) " "))
	    (progn (setq curl (1+ curl)) " ")
	    (if (string= class "LOGIN")
		(concat 
		 (if (> (+ curl (length opcode) 3) 79)
		     (progn (setq curl 0) "\n")
		   (progn (setq curl (1+ curl)) " "))
		 (downcase fromhost) " "
		 (progn (setq curl (+ 2 (length opcode) curl))
			(if (string-match "LOGIN" opcode) "LOGIN" "LOGOUT")))
	      (concat 
	       (progn (setq curl (+ curl (length signature)))
		      (concat "[" signature))
	       (if (string-match "nil" opcode) ()
		 (concat
;;;		  (if (> (+ curl (length opcode) 3) 79)
;;;		      (progn (setq curl 0) "\n")
;;;		    (progn (setq curl (1+ curl)) " "))
		  "/ "
		  (progn (setq curl (+ 2 (length opcode) curl)) opcode)))
	       "]\n")
;;;	       (if (> curl 76) (concat "]\n")
;;;		 (concat "]\n"))
	      ))))))
  msg)


(defun zephyr-other-frame () (interactive)
  (select-frame (make-frame))
  (zephyr-new-buffer))

(defun do-zephyr-setup ()

  (if (< max-lisp-eval-depth 500)
      (setq max-lisp-eval-depth 500))

  (zephyr-setup-init-faces)
  (zephyr-setup-setqs)

  (add-hook 
   'zephyr-mode-hook
   '(lambda ()
      (auto-fill-mode 1)
      (setq fill-column 78)
      (make-variable-buffer-local 'blink-matching-paren)
      (setq blink-matching-paren t)
      (if (or magus-in-xemacs
	      (eq window-system 'x))
	  (progn (setq font-lock-keywords zephyr-font-lock-keywords)
		 (font-lock-mode)))
      (local-set-key "\C-c\C-f" 'zephyr-zlocate-finger)
      ))

  (load "~/.zephyr-options")
  (setq zephyr-receive-program (list tzc-binary "-e" zephyr-exposure))
  )

(do-zephyr-setup)

(provide 'zephyr-setup)
