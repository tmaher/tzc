;;; netscape-fetch-url.el     tells Netscape to get a given URL
;;;
;;; Copyright (c)1995 Darrell Kindred <dkindred@cs.cmu.edu>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; This pkg provides the netscape-fetch-url function, which instructs a 
;;; running Netscape process to fetch the document corresponding to the
;;; given URL.
;;;

(defvar netscape-program 
  (if (memq 't 
	    (mapcar (function
		     (lambda (f)
		       (file-exists-p (expand-file-name "netscape-remote" f))))
		    exec-path))
      "netscape-remote"
    "netscape")
  "Program to run to send remote-control commands to netscape.  Must
accept the -remote argument.")

(defvar netscape-display nil
  "If non-nil, X display on which to find netscape.
If nil, DISPLAY environment variable is used.")

(defvar netscape-window-id nil
  "If non-nil, X window id of a netscape window to send commands to.")

(defvar netscape-quiet nil
  "*If non-nil, don't print the message saying that Netscape has been
asked to find a document.")

(defvar netscape-use-new-window t
  "*If non-nil, display documents fetched via netscape in a new window.")

(defvar netscape-process-url nil
  "don't use this")

(defun netscape-fetch-url (&optional buf b e pfx url)
  "Instruct a running Netscape process to fetch a document with 
the given URL.  If PFX is >1, prompt the user using the given
URL as the default."
  (interactive)
  (if (or (and pfx (> pfx 1))
	  (null url))
      (setq url (read-from-minibuffer "URL: " url)))
  (if (not (stringp url))
      (error "netscape-fetch-url: bad argument (expected a string)"))
  ;; we have to quote '(', ')', and ',' because they will confuse netscape.
  (save-excursion
    (let ((b (generate-new-buffer "*netscape-fetch-url*")))
      (set-buffer b)
      (insert url)
      (goto-char (point-min)) (replace-string "(" "%28")
      (goto-char (point-min)) (replace-string ")" "%29")
      (goto-char (point-min)) (replace-string "," "%2C")
      (setq url (buffer-substring (point-min) (point-max)))
      (kill-buffer b)))
  (let* ((args   (append  (if netscape-window-id
			      (list "-id" netscape-window-id))
			  (if netscape-use-new-window
			      '("-noraise") ; don't raise the old window
			    '("-raise"))   ; default
		          (list "-remote" (format 
					   (if netscape-use-new-window
					       "openURL(%s,new-tab)" 
					     "openURL(%s)")
					   url))
			  (if netscape-display
			      (list "-display" netscape-display))))
	 (proc (apply 'start-process
		      "netscape-remote"
		      (generate-new-buffer "*netscape-remote*")
		      netscape-program
		      args)))
    (set-buffer (process-buffer proc))
    (make-local-variable 'netscape-process-url)
    (setq netscape-process-url url)
    (set-process-sentinel proc 'netscape-sentinel)
    (if (not netscape-quiet)
	(message "Running %s in the background..." netscape-program ))
;		 (mapconcat '(lambda (a) a) args " ")))
    (if (not (eq (process-status proc) 'run))
	(netscape-sentinel proc nil))))

(defun netscape-sentinel (proc msg)
  "This is run when the netscape -remote process exits/dies"
  (let ((s  (process-status proc)))
    (if (not (eq s 'run))
	(progn
	  (set-buffer (process-buffer proc))
	  (if (= 0 (process-exit-status proc))
	      (progn
		(message "Asked netscape to fetch URL %s" 
			 netscape-process-url)
		(kill-buffer (current-buffer))
		(delete-process proc))
	    (pop-to-buffer (current-buffer))
	    (message "Error from %s: %s" netscape-program msg)
	    (beep))))))
	    

(provide 'netscape-fetch-url)
