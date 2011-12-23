(setq load-path (append (list (concat (getenv "ZEPHYR_CAVE") "/code")) load-path))

(setq zephyr-default-realm (concat "@" (getenv "ZEPHYR_DEFAULT_REALM")))
(setq zwatch-buffer (not (getenv "ZEPHYR_NO_ZW")))
(load "lounge-zephyr-setup")

(zephyr-dual)
