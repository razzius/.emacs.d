(setq site-run-file nil)

(tool-bar-mode -1)
(menu-bar-mode -1)

(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))

