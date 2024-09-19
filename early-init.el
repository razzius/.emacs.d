(setq site-run-file nil
      gc-cons-threshold (* 500 1024 1024))

(menu-bar-mode -1)

(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
