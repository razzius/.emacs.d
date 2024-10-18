(setq site-run-file nil
      gc-cons-threshold (* 500 1024 1024))

(menu-bar-mode -1)

(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))

(setq default-frame-alist
      '((top . 0) (left . 0) (width . 80) (height . 45)))
