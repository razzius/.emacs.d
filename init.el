(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	user-emacs-directory)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-dark 'noconfirm))

(setq
 inhibit-startup-screen t
 visible-bell t)

(use-package general)

(use-package vc-hooks
  :straight nil
  :custom
  (vc-follow-symlinks t))

(use-package help
  :straight nil
  :custom
  (help-window-select t)
  :general (:keymaps 'help-mode-map
		     "<tab>" 'forward-button))

(use-package help-fns
  :straight nil
  :general (:states 'normal
		    :prefix "SPC"
		    "hdf" 'describe-function
		    "hdv" 'describe-variable))

(use-package window
  :straight nil
  :general (:states 'normal
		    :prefix "SPC"
		    "wd" 'delete-window
		    "wm" 'delete-other-windows
		    "wo" 'other-window))

(use-package files
  :straight nil
  :custom
  (make-backup-files nil)
  :general (:states 'normal
		    :prefix "SPC"
		    "ff" 'find-file
		    "fs" 'save-buffer))

(use-package elisp-mode
  :straight nil
  :general ("M-RET" 'eval-defun))


(use-package menu-bar
  :straight nil
  :general (:states 'normal
		    :prefix "SPC"
		    "ESC" 'kill-this-buffer))

(use-package evil
  :config (evil-mode))

(use-package restart-emacs
  :general (:states 'normal
		    :prefix "SPC"
		    "qr" 'restart-emacs))

(use-package vterm
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  :general
  (:states 'normal
	   :prefix "SPC"
	   "'" 'vterm))

(use-package crux
  :general (:states 'normal
		    :prefix "SPC"
		    "TAB" 'crux-switch-to-previous-buffer
		    "fi" 'crux-find-user-init-file
		    "fd" 'crux-delete-file-and-buffer
		    "fR" 'crux-rename-file-and-buffer))

(use-package eval-sexp-fu
  :custom
  (eval-sexp-fu-flash-duration .3))

(use-package ivy
  :config
  (ivy-mode))

(use-package magit
  :general (:states 'normal
		    "gs" 'magit-status))

(use-package projectile)
