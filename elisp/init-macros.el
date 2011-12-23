(load "magus-macs")

(defvar tardis-in-xemacs
  (string-match "XEmacs" emacs-version)
  "Returns true if XEmacs is running this.")

(defmacro fsf/x (fsf x)
  `(if tardis-in-xemacs
	   ,x ,fsf))

(defun side-effect (&rest body)
  (if (boundp 'zml-with-side-effects)
      (progn body)))

(defun zml-load-directory ()
  (let ((dir (directory-files zml-load-directory-path nil "^.*\\.el$")))
    (mapcar 'load dir)))

(defun zml-load-with-side-effects ()
  (let ((zml-with-side-effects))
    (zml-load-directory)))
