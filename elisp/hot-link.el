;;; hot-link.el    support for creating & following hot links in a buffer
;;;
;;; Copyright (c)1993 Darrell Kindred <dkindred@cs.cmu.edu>
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


;;; General support for making hypertext-like links in a buffer,
;;; and for following those links.
;;;
;;; These functions were largely inspired by and/or stolen from
;;; w3.el, a cool emacs WWW browser by William Perry (wmperry@indiana.edu).
;;;
;;; To use hot-links, do something like this:
;;;   (require 'hot-link)
;;;   (if (string-match "XEmacs\\|Lucid" emacs-version)
;;;      (define-key some-mode-map 'button2 'hot-link-follow-mouse))
;;;   (define-key some-mode-map "\C-m" 'hot-link-follow-point)
;;; Then, to create a hot-link,  call the function hot-link-make:
;;;   (hot-link-make start end follow-function arg)
;;; where start and end delimit the hot link, follow-function is a
;;; function which should be called when the link is activated, and
;;; arg is an (optional) argument which will be passed to the follow-function.
;;;
;;;
;;;
;;; Notes:
;;;   - hot-link should work under XEmacs, Lucid GNU Emacs, Epoch,
;;;     FSF GNU Emacs v.18, and FSF GNU Emacs v.19, but it
;;;     only takes advantage of mouse support with XEmacs/Lucid.
;;;     If you want to write mouse support for Epoch or FSF19,
;;;     I'll be happy to put it in.
;;;   - Under XEmacs (Lucid Emacs), hot-link takes advantage of extents.
;;;   - Variables and functions prefixed with hot-link:: are
;;;     not intended for use outside this package.
;;;
;;; To do:
;;;   - under lemacs, if not (face-differs-from-default-p hot-link-face)
;;;     then do a copy-face from 'bold.  But what if the user wants to
;;;     have a non-bold face?  I guess they have to define it after hot-link
;;;     is loaded.

(defconst hot-link::running-xemacs
  (and (string-match "XEmacs\\|Lucid" emacs-version) t))

(defvar hot-link-face 'hot-link-face
  "The face in which hot links will be displayed.  (xemacs only)")

(defun hot-link-make (start end activate-fun &optional args 
			    overlap-ok keymap help-echo)
  "Make the text between START and END in the current buffer into a hot link.
START is included in the link, END is not.
ACTIVATE-FUN is a function which will be called when the link is activated.
The ACTIVATE-FUN will be called with these arguments:
    1: buffer in which the link appears
    2: start position of the link
    3: end position of the link
    4: the prefix argument used when the link was activated
and ARGS is an optional list of extra arguments which will be passed to
ACTIVATE-FUN.

If an overlapping hot-link already exists, do nothing and return nil;
otherwise return t.  If optional argument OVERLAP-OK is non-nil, then
allow overlaps and always return t, but if the overlapping region is 
clicked on, either link may be followed.

If KEYMAP is provided, it is a keymap to be used for keypresses or 
button clicks in the link (presumably containing bindings 
for hot-link-follow-mouse and hot-link-follow-point).  This feature
depends on extent keymaps, which are only available under lemacs 19.9 
and later.

If HELP-ECHO is provided, it is a string (or function returning a string)
which will be displayed when the mouse passes over the link (XEmacs
only)." 
  (if (or overlap-ok
	  (null (hot-link::get-link (current-buffer) start end)))
      (progn
	(if hot-link::running-xemacs
	    (hot-link::make-xemacs start end activate-fun 
				   args keymap help-echo)
	  (hot-link::make-emacs18 start end activate-fun args))
	t)
    nil))

(defun hot-link::make-xemacs (start end activate-fun args keymap help-echo)
  (let ((ext   (make-extent start end)))
    (or (find-face hot-link-face)
	(make-face hot-link-face))
    (if (fboundp 'set-extent-property)
	;; lemacs 19.9 and greater
	(progn
	  (set-extent-property ext 'face 'hot-link-face)
	  (set-extent-property ext 'hot-link-function activate-fun)
	  (set-extent-property ext 'hot-link-args args)
	  (set-extent-property ext 'highlight t)
	  (if keymap
	      (set-extent-property ext 'keymap keymap))
	  (if help-echo
	      (set-extent-property ext 'help-echo help-echo)))
      (set-extent-face ext hot-link-face)
      (set-extent-data ext (list 'hot-link activate-fun args))
      (set-extent-attribute ext 'highlight))))

(defvar hot-link::pseudo-extents nil
  "list of pseudo-extents which contain hot-links.  Each extent looks like
((begin . end) follow-fun args), where begin and end are markers, 
follow-fun is a function, and args is a list of arguments.
(non-xemacs only)")

(make-variable-buffer-local 'hot-link::pseudo-extents)

(defun hot-link::make-emacs18 (start end follow-fun args)
  ;; make the area between start & end hot
  ;; We don't have extents, so we have to do it the slow way,
  ;; with markers.
  ;; (could also add [[ ]] around the link here if we felt like it)
  (setq hot-link::pseudo-extents
	(cons
	 (list (cons (copy-marker start) (copy-marker end))
	       follow-fun
	       args)
	 hot-link::pseudo-extents)))

;;; (defun hot-link-maybe-follow-point ()
;;;   "Follow the hot-link at point.  If there is none there, then do
;;; whatever the global binding for the pressed key/button is.
;;; See hot-link-make."
;;;   (interactive)
;;;   (condition-case nil
;;;       (call-interactively 'hot-link-follow-point)
;;;     (hot-link-no-link  (call-interactively
;;; 			(hot-link::backup-key-binding
;;; 			 (hot-link::this-command-keys))))))

(put 'hot-link-no-link 'error-conditions '(error hot-link-no-link))
(put 'hot-link-no-link 'error-message  "hot-link")

(defun hot-link-follow-point (pt pfx)
  "Follow the hot-link at point.  See hot-link-make."
  (interactive "d
p")
  (hot-link::follow (current-buffer) pt pfx))

(defun hot-link-follow-mouse (e pfx)
  "Follow the hot link which was just clicked with the mouse.
See hot-link-make."
  (interactive "e
p")
  (if (not hot-link::running-xemacs)
      (error "hot-link-follow-mouse requires XEmacs."))
  (hot-link::follow (window-buffer (event-window e)) (event-point e) pfx))

;;; (defun hot-link-maybe-follow-mouse ()
;;;   "Follow the hot link which was just clicked with the mouse.
;;; If there is none there, then do whatever the global binding for the
;;; pressed button is.  See hot-link-make."
;;;   (interactive)
;;;   (condition-case nil
;;;       (call-interactively 'hot-link-follow-mouse)
;;;     (hot-link-no-link  (call-interactively
;;; 			(hot-link::backup-key-binding
;;; 			 (hot-link::this-command-keys))))))
  
(defun hot-link::follow (buf pt pfx)
  "internal function.
Use hot-link-follow-point or hot-link-follow-mouse instead."
  (let ((link  (if pt (hot-link::get-link buf pt pt) nil)))
    (if link
	(let ((func        (hot-link::link-function link))
	      (extra-args  (hot-link::link-args     link))
	      (begin       (hot-link::link-begin    link))
	      (end         (hot-link::link-end      link)))
	  (apply func buf begin end pfx extra-args))
      (signal 'hot-link-no-link '("No hot link there.")))))

(defun hot-link::maybe-follow-something (primary-cmd backup-cmd)
  (interactive)
  (condition-case nil
      (call-interactively primary-cmd)
    (hot-link-no-link  (call-interactively backup-cmd))))

(defun hot-link::link-function (link)
  (if hot-link::running-xemacs
      (if (fboundp 'extent-property)
	  (extent-property link 'hot-link-function)
	(car (cdr (extent-data link))))
    (car (cdr link))))

(defun hot-link::link-args (link)
  (if hot-link::running-xemacs
      (if (fboundp 'extent-property)
	  (extent-property link 'hot-link-args)
	(car (cdr (cdr (extent-data link)))))
    (car (cdr (cdr link)))))

(defun hot-link::link-begin (link)
  (if hot-link::running-xemacs
      (extent-start-position link)
    (marker-position (car (car link)))))

(defun hot-link::link-end (link)
  (if hot-link::running-xemacs
      (extent-end-position link)
    (marker-position (cdr (car link)))))

;;; return the global binding for the key/button sequence used to
;;; activate this function
; (defun hot-link::global-binding ()
;   (let ((keys  (if hot-link::running-xemacs
; 		   (if (fboundp 'events-to-keys)
; 		       (events-to-keys (this-command-keys))
; 		     (mapcar hot-link::event-to-key (this-command-keys)))
; 		 (this-command-keys))))
;     (global-key-binding keys)))
;     

;;; return *either* an extent or a pseudo-extent,
;;; which is a list ((begin . end) follow-fun args),
;;; which represents a hot-link overlapping the region [begin,end).
;;; Return nil if none matches.
(defun hot-link::get-link (buf begin end)
  (if hot-link::running-xemacs
      (hot-link::extent-at buf begin end)
    ;; non-xemacs: search for the right pseudo-extent
    (save-excursion
      (set-buffer buf)
      (let ((pexts  hot-link::pseudo-extents))
	(while (and pexts
		    (not (hot-link::regions-overlap
			  begin end
			  (hot-link::link-begin (car pexts))
			  (hot-link::link-end   (car pexts)))))
	  (setq pexts (cdr pexts)))
	(if pexts
	    (car pexts)
	  nil)))))

(defun hot-link::regions-overlap (begin1 end1 begin2 end2)
  "non-nil iff the region [BEGIN1,END1) overlaps [BEGIN2,END2)"
  (or (and (> end1 begin2)
	   (< begin1 end2))
      (= begin1 begin2)))


(defvar hot-link::ext nil
  "internal.  don't mess with it.")

(defun hot-link::extent-at (buf begin end)
  "Return a hot-link extent which overlaps the given region"
  (let ((hot-link::ext nil))  ;; dynamic scope, retch...
    (map-extents 'hot-link::extent-at-aux buf begin end)
    hot-link::ext))

(defun hot-link::extent-at-aux (extent tag)
  (if (fboundp 'extent-property)   ;; lemacs 19.9 & later
      (if (extent-property extent 'hot-link-function)
	  (setq hot-link::ext extent)
	nil)
    (let ((y (extent-data extent)))
      (if (and (listp y)
	       (eq (car y) 'hot-link))
	  (setq hot-link::ext extent)
	nil))))

;;; Mediocre substitute for events-to-keys, which doesn't appear 
;;; until lemacs19.8.
;;; (defun hot-link::event-to-key (e)
;;;   (cond
;;;    ((key-press-event-p e)  (event-key e))
;;;    ((button-event-p e)     (let* ((btn-num   (event-button e))
;;; 				  (name      (format "button%d" btn-num))
;;; 				  (up        (if (button-press-event-p e)
;;; 						 ""
;;; 					       "up")))
;;; 			     (make-symbol (concat name up))))
;;;    (t    (error "hot-link::event-to-key: bad argument"))))


(defun hot-link-bind-maybe-follow-point (keymap keys backup-fn)
  "In KEYMAP, bind the key-sequence KEYS to hot-link-follow-point, 
using BACKUP-FN as a backup (for when point is not on a link)."
  (let ((fn  (list 'lambda '() '(interactive)
		   (list 'hot-link::maybe-follow-something 
			 '(quote hot-link-follow-point)
			 (list 'quote backup-fn)))))
    (define-key keymap keys fn)))

(defun hot-link-bind-maybe-follow-mouse (keymap keys backup-fn)
  "In KEYMAP, bind the key-sequence KEYS to hot-link-follow-mouse, 
using BACKUP-FN as a backup (for when point is not on a link)."
  (let ((fn  (list 'lambda '() '(interactive)
		   (list 'hot-link::maybe-follow-something 
			 '(quote hot-link-follow-mouse)
			 (list 'quote backup-fn)))))
    (define-key keymap keys fn)))


;;; A hack to partially avert a bug in lemacs through 19.8 at least...
;;; (if you press an unbound key-seq and then the key-seq which activates
;;; the command, the unbound sequence stays on this-command-keys.  
;;; This is mainly a problem just for unbound buttonup events.  
;;; So we'll just strip any buttonup events from the vector
;;; (unless it's the last event on the vector).
;;; Calls to (hot-link::this-command-keys) should be replaced with
;;; (this-command-keys) once the bug is fixed.
;;; (defun hot-link::this-command-keys ()
;;;   (if (not hot-link::running-lemacs)
;;;       (this-command-keys)
;;;     (let* ((evs  (this-command-keys))
;;; 	   (n    (- (length evs) 1))
;;; 	   (l    (list (elt evs n))))
;;;       (setq foobar evs)
;;;       (while (> n 0)
;;; 	(setq n (- n 1))
;;; 	(let ((e (elt evs n)))
;;; 	  (if (not (button-release-event-p e))
;;; 	      (setq l (cons e l)))))
;;;       (apply 'vector l))))

(provide 'hot-link)
