(setq site-run-file nil
      gc-cons-threshold (* 100 1024 1024))

(tool-bar-mode -1)
(menu-bar-mode -1)

(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))
