;;; hot-url.el     makes urls & filenames into hot-links
;;;
;;; Copyright (c)1993,1998 Darrell Kindred <dkindred@cs.cmu.edu>
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


;;; This package provides support for making URLs and/or 
;;; absolute filenames in a region into hot-links (see hot-link.el).  
;;;
;;; To use it in lemacs 19.9 & later, do something like this:
;;;   (setq my-mode-hot-link-map (make-sparse-keymap))
;;;   (define-key my-mode-hot-link-map 'button2 'hot-link-follow-mouse)
;;;   (define-key my-mode-hot-link-map "\C-m" 'hot-link-follow-point)
;;;   (require 'hot-url)
;;; Then, call
;;;   (hot-url-make-urls-hot start end my-mode-hot-link-map)
;;; to make the urls in a given region hot (or replace 'urls' with
;;; 'msgids' or 'filenames').
;;;
;;; To use it in a more primitive emacs, do something like this:
;;;   (require 'hot-url)
;;;   (if (string-match "XEmacs\\|Lucid" emacs-version)
;;;      (define-key some-mode-map 'button2 'hot-link-maybe-follow-mouse))
;;;   (define-key some-mode-map "\C-m" 'hot-link-maybe-follow-point)
;;; Then, call
;;;   (hot-url-make-urls-hot start end)
;;; to make the urls in a given region hot (or replace 'urls' with
;;; 'msgids' or 'filenames').
;;;

(require 'hot-link)      ; support for creating and following the hot links

;;; See http://info.cern.ch/hypertext/WWW/Addressing/URL/5_BNF.html
;;; for a BNF grammar of URLs.  

;;; a couple of these (tn3270, file) aren't mentioned in the spec above
(defvar hot-url-url-schemes
  '("mailto" "news" "ftp" "http" "https" "file" "telnet" "gopher" "wais"
     ; "tn3270" "prospero" (nobody uses these...)
    )
  "list of strings which are valid url schemes")

;;; these chars are illegal in URLs:   {  }  | [  ]  \  ^  ~  <  > 
;;; Apparently a URL may contain whitespace as well as single and double
;;; quotes.  We'll pretend that whitespace is illegal.
(defvar hot-url-url-char
  (concat "[^"
          "}{<>"               ; illegal chars
          ;; "\\\\^~|\[\]"       ; illegal chars that we'll allow anyway:
	                         ;   \^~|[]
          " \t\n"                ; legal chars which we hope not to see.
          ").,\""                  ; these are legal, but we'll only allow
                                 ; them if they're followed by a url-char
          "]")
  "regexp matching a legal URL char.")

(defvar hot-url-url-regexp (concat
			    "\\("
			      "\\("
                                "\\("
			        (mapconcat 'regexp-quote
				  	   hot-url-url-schemes
					   "\\|")
			        "\\)"
			        ":"
				;; try allowing www.* and ftp.* too
				"\\|" "www\\."
				"\\|" "ftp\\."
                              "\\)"
			      hot-url-url-char
			      hot-url-url-char "+"
			      "\\("                              
			        "[).,\"]+" hot-url-url-char "+"
			      "\\)*"
			    "\\)")
    "Regular expression matching a url.
First parenthesized expression should correspond to the url itself.

If you modify this regexp, make sure that the setting of `starts' in
hot-url-make-urls-hot still covers all the possible initial substrings.
")

;;; unfortunately, whitespace is legal, but we'll pretend it's not,
;;; except in urls which are bracketed by < >.

(defvar hot-url-msgid-regexp "\\(<[^ \t\n@<>]+@[^ \t\n@<>]+>\\)"
    "Regular expression matching a message-id.
First parenthesized expression should correspond to the actual message-id.")

(defvar hot-url-include-regexp "^#include[ \t]*\\([\"<][^>\"\n]*[>\"]\\)"
  "Regular expression matching a #include'd file.
First parenthesized expression should correspond to the filename, including
angle-brackets or quotes.")

(defvar hot-url-filename-char "[^])[( \t\n,.;!*?<>\"']"
  "regexp matching a character which may appear in a filename
(except period, comma, semicolon)")

(defvar hot-url-filename-first-components 
  '("afs" "alex" "bin" "dev" "etc" "lib" "sup" "tkt" "tmp" "usr" 
    "sbin" "home" "..")
  "list of strings used in the default value for hot-url-filename-regexp.
An absolute filename must begin with /foo where foo is one of these strings.
Note that these are just prefixes, so if \"usr\" is one of these strings
then \"/usr0/foo\" is a valid absolute filename.  You can set this
list to (\"\") to allow any pathname starting with \"/\"")

(defvar hot-url-filename-regexp 
  (concat 
   "\\(/"				; starts with a `/'
     "\\("                              ; then some reasonable first component
       (mapconcat 'regexp-quote
		  hot-url-filename-first-components
		  "\\|")
     "\\)"
     hot-url-filename-char "*"		; then a bunch of filename chars
     "\\("                              
       "[.,;]+" hot-url-filename-char "+"   ; allow .,; only if followed by
     "\\)*"				    ; more filename chars
   "\\)")
  "Regular expression matching a filename.
First parenthesized expression should correspond to the actual filename.")

;;;(defvar hot-url-filename-regexp "[]([ \t\n]\\(/[^]()[ \t\n]*\\)"
;;;    "Regular expression matching a filename.
;;;First parenthesized expression should correspond to the actual filename.")

(defvar hot-url-fetch-url-function 'mozilla-fetch-url
  "*This variable contains a function which will be called from hot-url
to fetch the document with the given URL.  
The function should take six arguments:
    1- the buffer in which the url appears
    2- beginning position of the url
    3- end position of the url
    4- numeric prefix arg
    5- the url itself
    6- an alist containing more info (e.g. 'timesecs for the containing zgram)
Likely values: mozilla-fetch-url, netscape-fetch-url, fetch-url-using-w3")


(defvar hot-url-convert-url-functions '(hot-url-cvt-bare-www-ftp)
  "*If non-nil, this function is called with a URL as its first argument,
and an 'info' alist as its second, and the result is passed to the 
hot-url-fetch-url-function.  Useful if people are in the habit of 
sending you URLs that you have to swizzle before you can use them locally.

One possible value for this variable is 'hot-url-cvt-file-url-to-http.

If the value of this variable is a list, each function on the list is
applied in turn to perform successive conversions on the url.")

(if (fboundp 'define-obsolete-variable-alias)
    (define-obsolete-variable-alias
      'hot-url-convert-url-function
      'hot-url-convert-url-functions))

(defvar hot-url-file-url-prefix "http://www.cs.cmu.edu/")

(defun hot-url-cvt-bare-www-ftp (url info)
  "If the argument is a partial url starting with, e.g., \"www.\" or \"ftp.\",
convert to a full url with http:// or ftp://."
  (if (string-match "^[^/]*:" url)
      url
    (if (string-match "^ftp" url)
	(concat "ftp://" url)
      (concat "http://" url))))

(defun hot-url-cvt-file-url-to-http (url)
  "If the argument is a local file URL, make it into an http URL
using hot-url-file-url-prefix.  Otherwise, leave it alone."
  (if (or (string-match "\\(file://localhost/\\)" url)
	  (string-match "\\(file:///\\)" url)
	  (string-match "\\(file:/\\)[^/]" url))
      (concat hot-url-file-url-prefix (substring url (match-end 1)))
    url))

(defvar hot-url-html-files-are-urls t
  "*If non-nil, treat filenames ending in \".html\" like file://localhost/
URLs.")

(defvar hot-url-fetch-file-function 'hot-url-fetch-file-default
  "*This variable contains a function which will be called from hot-url
to fetch a file.  The function should take five arguments:
    1- the buffer in which the filename appears
    2- beginning position of the filename
    3- end position of the filename
    4- numeric prefix arg
    5- the filename itself (will be absolute)")

(defvar hot-url-fetch-msgid-function 'hot-url-fetch-msgid-default
  "*This variable contains a function which will be called from hot-url
to fetch a message by its message-id.  The function should take 
five arguments:
    1- the buffer in which the msgid appears
    2- beginning position of the msgid
    3- end position of the msgid
    4- numeric prefix arg
    5- the message-id itself (including angle-brackets)")

(defvar hot-url-fetch-include-function 'hot-url-fetch-include-default
  "*This variable contains a function which will be called from hot-url
to fetch a file in a #include line.  The function should take 
five arguments:
    1- the buffer in which the #include line appears
    2- beginning position of the <foo.h> or \"foo.h\" part
    3- end position of the the <foo.h> or \"foo.h\" part (just past the end)
    4- numeric prefix arg
    5- the <foo.h> or \"foo.h\" text itself (including angle-brackets or quotes)")

;; We don't want to use help-echo before XEmacs 19.13 because the
;; temporary-message facility doesn't exist yet.
(defvar hot-url-use-help-echo (fboundp 'display-message)
  "If non-nil, show a message when the mouse passes over a 
hot url/filename/whatever.")

(autoload 'mosaic-fetch-url "mosaic-fetch-url"
  "Tell a running Mosaic process to fetch a document with the given URL." t)

(autoload 'netscape-fetch-url "netscape-fetch-url"
  "Tell a running Mosaic process to fetch a document with the given URL." t)

(autoload 'mozilla-fetch-url "mozilla-fetch-url"
  "Tell a running Mosaic process to fetch a document with the given URL." t)

(autoload 'raw-fetch-url "raw-fetch-url"
  "Use raw-fetch-url-program (e.g. lynx -dump) to fetch a URL into
a buffer.")


(defun hot-url-use-mosaic ()
  "Tell hot-url package to display retrieved URLs using the currently
running Mosaic process."
  (interactive)
  (setq hot-url-fetch-url-function 'mosaic-fetch-url)
  (message "hot-url-fetch-url-function is now 'mosaic-fetch-url"))

(defun hot-url-use-netscape ()
  "Tell hot-url package to display retrieved URLs using the currently
running Netscape process."
  (interactive)
  (setq hot-url-fetch-url-function 'netscape-fetch-url)
  (message "hot-url-fetch-url-function is now 'netscape-fetch-url"))

(defun hot-url-use-mozilla ()
  "Tell hot-url package to display retrieved URLs using the currently
running Mozilla process."
  (interactive)
  (setq hot-url-fetch-url-function 'mozilla-fetch-url)
  (message "hot-url-fetch-url-function is now 'mozilla-fetch-url"))

(defun hot-url-use-raw-fetch ()
  "Tell hot-url package to display retrieved URLs in an emacs buffer
using 'lynx -dump' (see raw-fetch-url-program)."
  (interactive)
  (setq hot-url-fetch-url-function 'raw-fetch-url)
  (message "hot-url-fetch-url-function is now 'raw-fetch-url"))

(autoload 'fetch-url-using-w3 "w3-fetch-url"
  "Tell w3 to display a document with the given URL." t)

(defun hot-url-use-w3 ()
  "Tell hot-url package to display retrieved URLs using w3-mode in emacs."
  (interactive)
  (setq hot-url-fetch-url-function 'fetch-url-using-w3)
  (message "hot-url-fetch-url-function is now 'fetch-url-using-w3"))

(defvar hot-url-type-registry 
  '((url      . ((make-hot-fun  . hot-url-make-urls-hot)
		 (action-var    . hot-url-fetch-url-function)))
    (filename . ((regexp-var    . hot-url-filename-regexp)
		 (help-echo     . "View this file")
		 (action-var    . hot-url-fetch-file-function)))
    (msgid    . ((regexp-var    . hot-url-msgid-regexp)
		 (help-echo-fun . hot-url-msgid-help-echo)
		 (action-var    . hot-url-fetch-msgid-function)))
    (include  . ((regexp-var    . hot-url-include-regexp)
		 (help-echo     . "Fetch this include file")
		 (action-var    . hot-url-fetch-include-function))))
  "This is an alist of the known types of things which can be made 
into hot-links by this package.  Each item in the list is a pair
   (TYPE . PROPERTY-ALIST)
where property-alist is an alist of properties of that type.
The available properties are
    regexp	    a regular expression matching the text to be made hot
    regexp-var	    a variable containing such a regexp
    help-echo       a string containing a message to appear in the echo-area
                    when the mouse passes over the hot-link
    help-echo-var   a variable containing such a message
    help-echo-fun   a function which takes a hot-link extent and returns
                    the message
    make-hot-fun    a function which should be called to make the hot links;
                    it should accept start, end, and keymap arguments
                    (If this is set, all regexp and help-echo properties
                    are ignored.)
    action          a function which will be called when the link is 
                    clicked (or whatever).  It will get these arguments:
                    buffer, begin-pos, end-pos, prefix, contents.
                    `prefix' is the prefix arg the user gave, and `contents'
                    is the text of the item being selected. 
    action-var      a variable containing an action function

One of regexp, regexp-var, and make-hot-fun must be defined,
and one of action and action-var must be defined.")

(defvar hot-url-default-hot-types '(url msgid filename)
  "This variable should contain a list of symbols;
the hot-url-make-all-hot function will turn things of the
indicated types into hot-links.  Supported types are 'url,
'filename, and 'msgid, but you can add your own by adding 
entries to hot-url-type-registry.

You can make this variable buffer-local if you want different 
default hot types for different buffers.")

(defun hot-url-make-all-hot (&optional start end keymap info)
  "Make urls, files, etc. in the given region into hot-links.
If called interactively, use the current region.
Default for START and END are beginning and end of buffer.

See the hot-url-default-hot-types variable to customize exactly
what gets made hot."
  (let ((types hot-url-default-hot-types))
    (while types
      (let* ((ty  (car types))
	     (props (cdr (assq ty hot-url-type-registry))))
	(if (null props)
	    (error "Type %s in hot-url-default-hot-types not in hot-url-type-registry"

		   (symbol-name ty)))
	(let ((m-h-f (cdr (assq 'make-hot-fun props))))
	  (if m-h-f
	      (funcall m-h-f start end keymap info)
	    (let ((rexp (or (cdr (assq 'regexp props))
			    (symbol-value (cdr (assq 'regexp-var props)))))
		  (msg  (or (cdr (assq 'help-echo props))
			    (symbol-value (cdr (assq 'help-echo-var props)))
			    (cdr (assq 'help-echo-fun props)))))
	      (if (null rexp)
		  (error "No regexp or make-hot-fun for %s in registry"
			 (symbol-name ty)))
	      (hot-url::make-things-hot start end rexp ty keymap msg info)))))
    (setq types (cdr types)))))

;; this used to be done with one big regexp, but this is faster:
(defun hot-url-make-urls-hot (&optional start end keymap info)
  "Make regular URLs in the given region into hot-links.
If called interactively, use the current region.
Default for START and END are beginning and end of buffer."
  (interactive "r")
  (if (null start)
      (setq start (point-min)))
  (if (null end)
      (setq end (point-max)))
  (save-excursion
    (let ((starts   (reverse (cons "www." hot-url-url-schemes))))
      (while starts
	(goto-char start)
	(while (search-forward (car starts) end t)
	  (if (and (progn (goto-char (match-beginning 0))
			  (looking-at hot-url-url-regexp))
		   (< (match-end 1) end))
	      (hot-link-make (match-beginning 1) (match-end 1)
			     'hot-url::fetch-it (list 'url nil info)
			     nil keymap 'hot-url-url-help-echo))
	  (condition-case nil (forward-char) (error nil)))
	(setq starts (cdr starts))))))

(defun hot-url-make-includes-hot (&optional start end keymap info)
  "Make files in \"#include <foo.h>\" lines between START and END 
into hot-links.  If called interactively, use the current region.
Default for START and END are beginning and end of buffer.
The file will be fetched using the function stored in
hot-url-fetch-include-function"
  (interactive "r")
  (let ((hot-url-default-hot-types '(include)))
    (hot-url-make-all-hot start end keymap info)))

(defun hot-url-make-filenames-hot (&optional start end keymap info)
  "Make regular URLs in the given region into hot-links.
If called interactively, use the current region.
Default for START and END are beginning and end of buffer."
  (interactive "r")
  (let ((hot-url-default-hot-types '(filename)))
    (hot-url-make-all-hot start end keymap info)))

(defun hot-url-make-msgids-hot (&optional start end keymap info)
  "Make message-ids of the form <foo@bar> in the given region into hot-links.
If called interactively, use the current region.
Default for START and END are beginning and end of buffer."
  (interactive "r")
  (let ((hot-url-default-hot-types '(msgid)))
    (hot-url-make-all-hot start end keymap info)))

(defun hot-url-url-help-echo (extent)
  (format "Open this URL%s"
	  (cond
	   ((eq hot-url-fetch-url-function 'fetch-url-using-w3)
	    " using w3")
	   ((eq hot-url-fetch-url-function 'netscape-fetch-url)
	    " using Netscape")
	   ((eq hot-url-fetch-url-function 'mozilla-fetch-url)
	    " using Mozilla")
	   ((eq hot-url-fetch-url-function 'mosaic-fetch-url)
	    " using Mosaic")
	   (t ""))))

(defun hot-url-msgid-help-echo (extent)
  (format "Fetch this news article%s"
	  (cond
	   ((eq hot-url-fetch-url-function 'fetch-url-using-w3)
	    " using w3")
	   ((eq hot-url-fetch-url-function 'netscape-fetch-url)
	    " using Netscape")
	   ((eq hot-url-fetch-url-function 'mozilla-fetch-url)
	    " using Mozilla")
	   ((eq hot-url-fetch-url-function 'mosaic-fetch-url)
	    " using Mosaic")
	   (t ""))))

(defun hot-url::make-things-hot (start end regexp type keymap help-echo 
				 &optional info)
  (if (null start)
      (setq start (point-min)))
  (if (null end)
      (setq end (point-max)))
  (or hot-url-use-help-echo
      (setq help-echo nil))
  (save-excursion
    (goto-char start)
    (while (re-search-forward regexp end t)
      (let ((b  (match-beginning 1))
	    (e  (match-end 1)))
	(hot-link-make b e
		       'hot-url::fetch-it (list type nil info) 
		       nil keymap help-echo)))))

;; older fsfmacs don't have functionp
(defun hot-url::functionp (f)
  (if (fboundp 'functionp)
      (functionp f)
    (and (symbolp f) (fboundp f))))

(defun hot-url::convert-url (url info)
  "Don't redefine this function; see the hot-url-convert-url-functions var."
  (let ((fns hot-url-convert-url-functions)
	fn args)
    (while fns
      (if (hot-url::functionp fns)
	  (progn 
	    (setq fn  fns)
	    (setq fns nil))
	(setq fn (car fns))
	(setq fns (cdr fns)))
      (setq url (funcall fn url info)))
    url))

(defun hot-url::fetch-it (buf b e pfx type &optional thing info)
  (let ((contents (if thing
		      thing
		    (save-excursion
		      (set-buffer buf)
		      (buffer-substring b e)))))
    ;; gross hack--should stick this in the fetch function
    (if (eq type 'url)
	(setq contents (hot-url::convert-url contents info)))
    ;; look up the action function and invoke it
    (let* ((props  (cdr (assq type hot-url-type-registry)))
	   (f      (or (cdr (assq 'action props))
		       (symbol-value (cdr (assq 'action-var props))))))
      (cond 
       ((null props)  (error "hot-url type %s not in type registry"
			     (symbol-name type)))
       ((null f)      (error "hot-url type %s has no action or action-var"
			     (symbol-name type)))
       (t             (funcall f buf b e pfx contents))))))

(defvar hot-url-use-view-mode t
  "if non-nil, then hot-url-fetch-file-default and 
hot-url-fetch-include-default will use view-mode to examine files
which are not writable")

(defun hot-url-fetch-file-default (buf b e pfx fname)
  "Default function used by hot-url to retrieve files.
It uses auto-view-mode to turn on view-mode if the file is not writable."
  (if (or (string-equal fname "") (> pfx 1))
      (let ((f  (if (or (and (boundp 'epoch::version) epoch::version)
			(string-lessp emacs-version "19"))
		    ;; v18 read-file-name doesn't know about
		    ;; initial-contents argument
		    (read-file-name "View file: " fname fname t)
		  (read-file-name "View file: "
				  (file-name-directory fname)
				  fname
				  t
				  (file-name-nondirectory fname)))))
	(setq fname (expand-file-name f))))
  (if (and hot-url-html-files-are-urls
	   (string-match "\\.html" fname))
      (let ((url (concat "file://localhost" fname)))
	(funcall hot-url-fetch-url-function buf b e pfx
		 (hot-url::convert-url url) ))
    (hot-url::get-regular-file fname)))

(defun hot-url-fetch-include-default (buf b e pfx text)
  "Default function used by hot-url to retrieve #included files.
If TEXT is in angle-brackets, it uses the CPATH environment variable
to search for the file.  If TEXT is in quotes, it searches BUF's
default-directory.
It uses auto-view-mode to turn on view-mode if the file is not writable."
  (let ((fname  (progn (string-match "[\"<]\\([^>]*\\)[\">]" text)
		       (substring (match-beginning 1) (match-end 1)))))
    (error "hot-url-fetch-include-default not yet implemented")))
    

(defun hot-url::get-regular-file (fname)
  (let ((find-file-fn  
	 (cond
	  ((and (fboundp 'device-type) 
		(not (memq 'x (mapcar 'device-type (device-list)))))
	   'find-file)
	  ((fboundp 'find-file-other-screen) 'find-file-other-screen)
	  ((fboundp 'find-file-new-screen) 'find-file-new-screen)
	  (t 'find-file))))
    (if hot-url-use-view-mode
	(let ((find-file-hooks (if (memq 'auto-view-mode find-file-hooks)
				   find-file-hooks
				 (append find-file-hooks
					 '(hot-url::auto-view-mode)))))
	  (funcall find-file-fn fname))
      (funcall find-file-fn fname))))

(defun hot-url::auto-view-mode ()
  "If the file of the current buffer is not writable, call view-mode.
  This is meant to be added to find-file-hooks.
  (Taken from view-less.el.)"
  (if (and buffer-file-name
	   (not (file-writable-p buffer-file-name))) (view-mode)))

(defun hot-url-fetch-msgid-default (buf b e pfx msgid)
  "Default function used by hot-url to fetch messages by message-id.
Just takes the given msgid (of the form <foo@bar>), converts it
to a URL (news:foo@bar), and hands it to hot-url-fetch-url-function."
  (if (or (string-equal msgid "") (> pfx 1))
      (setq msgid (read-from-minibuffer "Fetch message-id: " msgid)))
  (if (and (= 0 (string-match "<\\([^@<>]*@[^@<>]*\\)>" msgid))
	   (= (match-end 0) (length msgid)))
      (let ((url  (format "news:%s" (substring msgid
					       (match-beginning 1)
					       (match-end 1)))))
	(funcall hot-url-fetch-url-function buf b e pfx url))
    (error "hot-url-fetch-msgid-default: bad message-id (%s)" msgid)))

(provide 'hot-url)
