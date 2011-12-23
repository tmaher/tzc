;;; mozilla-fetch-url.el     tells Mozilla to get a given URL
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


;;; This pkg provides the mozilla-fetch-url function, which instructs a 
;;; running Mozilla process to fetch the document corresponding to the
;;; given URL.
;;;

(defvar mozilla-program 
  (if (memq 't 
	    (mapcar (function
		     (lambda (f)
		       (file-exists-p (expand-file-name "mozilla-remote" f))))
		    exec-path))
      "mozilla-remote"
    "mozilla")
  "Program to run to send remote-control commands to mozilla.  Must
accept the -remote argument.")

(defvar mozilla-display nil
  "If non-nil, X display on which to find mozilla.
If nil, DISPLAY environment variable is used.")

(defvar mozilla-window-id nil
  "If non-nil, X window id of a mozilla window to send commands to.")

(defvar mozilla-quiet nil
  "*If non-nil, don't print the message saying that Mozilla has been
asked to find a document.")

(defvar mozilla-use-new-window t
  "*If non-nil, display documents fetched via mozilla in a new window.")

(defvar mozilla-process-url nil
  "don't use this")

(defun mozilla-fetch-url (&optional buf b e pfx url)
  "Instruct a running Mozilla process to fetch a document with 
the given URL.  If PFX is >1, prompt the user using the given
URL as the default."
  (interactive)
  (if (or (and pfx (> pfx 1))
	  (null url))
      (setq url (read-from-minibuffer "URL: " url)))
  (if (not (stringp url))
      (error "mozilla-fetch-url: bad argument (expected a string)"))
  ;; we have to quote '(', ')', and ',' because they will confuse mozilla.
  (save-excursion
    (let ((b (generate-new-buffer "*mozilla-fetch-url*")))
      (set-buffer b)
      (insert url)
      (goto-char (point-min)) (replace-string "(" "%28")
      (goto-char (point-min)) (replace-string ")" "%29")
      (goto-char (point-min)) (replace-string "," "%2C")
      (setq url (buffer-substring (point-min) (point-max)))
      (kill-buffer b)))
  (let* ((args   (append  (if mozilla-window-id
			      (list "-id" mozilla-window-id))
			  (if mozilla-use-new-window
			      '("-noraise") ; don't raise the old window
			    '("-raise"))   ; default
		          (list "-remote" (format 
					   (if mozilla-use-new-window
					       "openURL(%s,new-tab)" 
					     "openURL(%s)")
					   url))
			  (if mozilla-display
			      (list "-display" mozilla-display))))
	 (proc (apply 'start-process
		      "mozilla-remote"
		      (generate-new-buffer "*mozilla-remote*")
		      mozilla-program
		      args)))
    (set-buffer (process-buffer proc))
    (make-local-variable 'mozilla-process-url)
    (setq mozilla-process-url url)
    (set-process-sentinel proc 'mozilla-sentinel)
    (if (not mozilla-quiet)
	(message "Running %s in the background..." mozilla-program ))
;		 (mapconcat '(lambda (a) a) args " ")))
    (if (not (eq (process-status proc) 'run))
	(mozilla-sentinel proc nil))))

(defun mozilla-sentinel (proc msg)
  "This is run when the mozilla -remote process exits/dies"
  (let ((s  (process-status proc)))
    (if (not (eq s 'run))
	(progn
	  (set-buffer (process-buffer proc))
	  (if (= 0 (process-exit-status proc))
	      (progn
		(message "Asked mozilla to fetch URL %s" 
			 mozilla-process-url)
		(kill-buffer (current-buffer))
		(delete-process proc))
	    (pop-to-buffer (current-buffer))
	    (message "Error from %s: %s" mozilla-program msg)
	    (beep))))))
	    

(provide 'mozilla-fetch-url)
