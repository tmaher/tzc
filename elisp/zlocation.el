;; requires get/set-location patch to tzc

(setq zephyr-tzcspew-dispatch-table (cons 
				      '(location . zephyr-location-info)
				      zephyr-tzcspew-dispatch-table))

; for now, just ignore the reports
(defun zephyr-location-info (msg) 
  nil)

(defun zephyr-change-hostname (host)
  (interactive "sNew hostname: ")
  (zephyr-send-to-client `((tzcfodder . set-location) (hostname . ,host))))

(defun zephyr-change-tty (tty)
  (interactive "sNew tty: ")
  (zephyr-send-to-client `((tzcfodder . set-location) (location . ,tty))))

(defun zephyr-change-exposure (exp)
  (interactive "sNew exposure: ")
  (zephyr-send-to-client `((tzcfodder . set-location) (exposure . ,exp))))

(provide 'zlocation)
