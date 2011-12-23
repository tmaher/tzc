(defvar zephyr-buffer-create-list nil  "list of buffers to start up")
(defvar zephyr-buffer-hot-list nil "*Which buffers should I cycle through?")

(defun zephyr-start-buffer (name public filters initial-history hook)
  "start a generic buffer"
  (setq zephyr-initial-history initial-history)
  (zephyr-new-buffer "name")
  (make-local-hook 'zephyr-after-send-hook)
  
  (if (and public 
	   zephyr-zap-in-public)
      (add-hook 'zephyr-after-send-hook 'zephyr-zap-last t t))

  (setq zephyr-filters filters)

  (if public
      (setq zephyr-history-ignore-regexp (concat zephyr-history-ignore-regexp "\\|^[^(]*$"))))

(defun zephyr-create-buffers .... )