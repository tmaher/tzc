;;; default exposure
(setq zephyr-exposure "NET-ANNOUNCED")

;;; default signature (assuming no random sigs found)
(setq zephyr-signature "My Sig")

;;; By default, if you have a .anyone, it's loaded.
;;; If you don't want it loaded, set this to nil
;;; or set it to a filename
(setq zephyr-anyone-file "~/.anyone")

;;; By default, if you have a .zsigs, it's loaded.
;;; This file is a list of random signatures
;;; Set to nil to suppress loading.
;(setq zephyr-signatures-file "~/.zsigs")

;;; By default, if you have a .zephyr.subs.tzc, it's loaded
;;; (this file should have the same format a .zephyr.subs file)
;;; Set to nil to suppress
;(setq zephyr-use-zephyr-subs t)

;;; If your .anyone file has names without
;;; realm names, this should be the default thing to
;;; tack on. For instance, "zml" instead of "zml@DEMENTIA.ORG"
;;; Has no effect anywhere else
(setq zephyr-default-realm "@CLUB.CC.CMU.EDU")

;;; Your Username
(setq zephyr-id "Barney")

;;; If you want the signatures announced before you send, this this to t
;(setq zephyr-sig-announce nil)

;;; Should the public zephyr buffer be on top?
; (setq public-on-top t)

;(setq zwatch-start nil)

;;; Should I *display* the zwatch buffer?
;;; If this is nil, but zwatch-start is still true,
;;; a *zw* buffer will exist with data but won't be displayed
;;; on the side.
;;; (requires modified tzc)
;(setq zwatch-buffer nil)


;;; spell check outgoing zephyrs
;(setq zephyr-spell-check t)

;;; default filters
;;; (this format is weird, but these are basically..
;;;   ( (field . "regex") (field2 . "regex2") what-to-do)
;;; where what-to-do is t or nil. t to accept, nil to punt)
(setq zephyr-filters '(
;   ((class . "^message$")  (instance . "^weather$")           (sender . "") nil)
;   ((class . "^message$")  (instance . "^cows$")              (sender . "") (recipient . "ATHENA") nil)
;   ((class . "^message$")  (instance . "^ahooy")              (sender . "") (recipient . "ATHENA") nil)
   nil))
  
  

;;; You will automatically be marked away after this many seconds.
;;; nil to disable.
(setq zephyr-auto-away-timeout nil)

;;; Show zephyrs of one line on the same line (if possible)
(setq zephyr-one-liners nil)

;;; Hide signatures? Only affects startup. 
;;; You can hide and unhide signatures at any time using 
;;; zephyr-hide-signatures and zephyr-show-signatures
(setq zephyr-hide-signatures nil)

;;; How much should be devoted to private zephyrs? (0.0 to 1.0)
(setq zephyr-private-ratio 0.4)

(setq zephyr-additional-faces '(('personal      "red"          "red")
                                ('outgoing      "cyan"         "dodgerblue")
                                ("MAIL:.*"      "magenta"      "magenta")
                                ("LOGIN:.*"     "yellow"       "orange")
                                ("PRINTING:.*"  "green"        "green")
                                (".*@AND"       "magenta"      "pink")
                                (".*@CS"        "magenta"      "pink")
                                (".*@ECE"       "magenta"      "pink")
				(".*@ATH"       "cyan"         "cyan")
                                (".*@CLB"       "brightyellow" "yellow")
                                (".*@[A-Z.]+"   "blue"         "blue")
                                (".*"           "blue"         "blue")))

