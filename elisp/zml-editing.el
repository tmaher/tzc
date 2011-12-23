(defun copy-line (&optional arg)
  "Copy the rest of the current line; if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

If `kill-whole-line' is non-nil, then kill the whole line
when given no argument at the beginning of a line."
  (interactive "*P")
  (copy-region-as-kill (point)
               ;; Don't shift point before doing the delete; that way,
               ;; undo will record the right position of point.
;; FSF
;              ;; It is better to move point to the other end of the kill
;              ;; before killing.  That way, in a read-only buffer, point
;              ;; moves across the text that is copied to the kill ring.
;              ;; The choice has no effect on undo now that undo records
;              ;; the value of point from before the command was run.
		       (progn
			 (if arg
			     (forward-line (prefix-numeric-value arg))
			   (if (eobp)
			       (signal 'end-of-buffer nil))
			   (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
			       (forward-line 1)
			     (end-of-line)))
			 (point))))

(defun copy-or-kill-line (&optional arg)
  (interactive)
  "If the point is sitting in something read-only, we call copy-line.
Otherwsie, we call kill-line."
  
  (setq this-command 'kill-region)
  (if (get-text-property (point) 'read-only)
      (copy-line arg)
    (kill-line arg))
  (append-next-kill))

(defun force-fill-paragraph-or-region (arg)
  (interactive "*P")
  (let ((inhibit-read-only t))
    (fill-paragraph-or-region arg)))

(defun invisible-char-p (&optional loc) 
  (let ((p (or loc (point))))
    (let ((prop
	   (get-char-property p 'invisible)))
      (if (eq buffer-invisibility-spec t)
	  prop
	(or (memq prop buffer-invisibility-spec)
	    (assq prop buffer-invisibility-spec)
	    (and (listp prop)
		 (reduce (lambda (prev cur)
			   (or prev
			       (memq cur buffer-invisibility-spec)
			       (assq cur buffer-invisibility-spec))) prop :initial-value nil)))))))

;(define-function 'real-move-to-column (symbol-function 'move-to-column))

;(defun zml-mtc (col &optional force buffer)
;  (with-current-buffer buffer
;    (if force
;	; someone should figure this case out
;	(if (> col (zml-mtc col nil buffer))
;	    (real-move-to-column col t buffer))
;      (let ((curpoint (point))
;	    (inhibit-motion-hooks t)
;	    newpoint)

;	(beginning-of-line)
;	(while column 


;(defun line-move (arg)
;  ;; Don't run any point-motion hooks, and disregard intangibility,
;  ;; for intermediate positions.
;  (let ((inhibit-point-motion-hooks t)
;        (opoint (point))
;        new line-end line-beg)
;    (unwind-protect
;        (progn
;          (if (not (or (eq last-command 'next-line)
;                       (eq last-command 'previous-line)))
;              (setq temporary-goal-column
;                    (if (and track-eol (eolp)
;                             ;; Don't count beg of empty line as end of line
;                             ;; unless we just did explicit end-of-line.
;                             (or (not (bolp)) (eq last-command 'end-of-line)))
;                        9999
;                      (current-column))))
;          (if (and (not (integerp selective-display))
;                   (not line-move-ignore-invisible))
;              ;; Use just newline characters.
;              (or (if (> arg 0)
;                      (progn (if (> arg 1) (forward-line (1- arg)))
;                             ;; This way of moving forward ARG lines
;                             ;; verifies that we have a newline after the last one.
;                             ;; It doesn't get confused by intangible text.
;                             (end-of-line)
;                             (zerop (forward-line 1)))
;                    (and (zerop (forward-line arg))
;                         (bolp)))
;                  (signal (if (< arg 0)
;                              'beginning-of-buffer
;                            'end-of-buffer)
;                          nil))
;            ;; Move by arg lines, but ignore invisible ones.
;            (while (> arg 0)
;              (end-of-line)
;              (and (zerop (vertical-motion 1))
;                   (signal 'end-of-buffer nil))
;              ;; If the following character is currently invisible,
;              ;; skip all characters with that same `invisible' property value.
;              (while (and (not (eobp))
;			  (invisible-char-p (point)))
;                (if (invisible-char-p)
;                    (goto-char (next-single-property-change (point) 'invisible))))
;              (setq arg (1- arg)))
;            (while (< arg 0)
;              (beginning-of-line)
;              (and (zerop (vertical-motion -1))
;                   (signal 'beginning-of-buffer nil))
;              (while (and (not (bobp))
;			  (invisible-char-p (point)))
;                (if (invisible-char-p)
;                    (goto-char (previous-single-property-change (point) 'invisible))))
;              (setq arg (1+ arg))))
;	    (let ((buffer-invisibility-spec nil))
;	      (move-to-column (or goal-column temporary-goal-column))))
;      (setq new (point))
;;      ;; If we are moving into some intangible text,
;;      ;; look for following text on the same line which isn't intangible
;;      ;; and move there.
;;      (setq line-end (save-excursion (end-of-line) (point)))
;;      (setq line-beg (save-excursion (beginning-of-line) (point)))
;;      (let ((after (and (< new (point-max))
;;                        (get-char-property new 'intangible)))
;;            (before (and (> new (point-min))
;;                         (get-char-property (1- new) 'intangible))))
;;        (when (and before (eq before after)
;;                   (not (bolp)))
;;          (goto-char (point-min))
;;          (let ((inhibit-point-motion-hooks nil))
;;            (goto-char new))
;;          (if (<= new line-end)
;;              (setq new (point)))))
;      ;; NEW is where we want to move to.
;      ;; LINE-BEG and LINE-END are the beginning and end of the line.
;      ;; Move there in just one step, from our starting position,
;      ;; with intangibility and point-motion hooks enabled this time.
;      (goto-char opoint)
;      (setq inhibit-point-motion-hooks nil)
;      (goto-char new)
;      ;; If intangibility processing moved us to a different line,
;      ;; readjust the horizontal position within the line we ended up at.
;;      (when (or (< (point) line-beg) (> (point) line-end))
;;        (setq new (point))
;;        (setq inhibit-point-motion-hooks t)
;;        (setq line-end (save-excursion (end-of-line) (point)))
;;        (beginning-of-line)
;;        (setq line-beg (point))
;;        (let ((buffer-invisibility-spec nil))
;;          (move-to-column (or goal-column temporary-goal-column)))
;;        (if (<= (point) line-end)
;;            (setq new (point)))
;;        (goto-char (point-min))
;;        (setq inhibit-point-motion-hooks nil)
;;        (goto-char new)
;;        )))
;      ))
;  nil)

(provide 'zml-editing)

