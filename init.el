(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	user-emacs-directory)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq
 make-backup-files nil
 visible-bell t)

(use-package evil
  :config (evil-mode))

(use-package restart-emacs)

(use-package vterm
  :config
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package general)

(use-package crux
  :general (:states 'normal
		    :prefix "SPC"
		    "TAB" 'crux-switch-to-previous-buffer
		    "fi" 'crux-find-user-init-file
		    "fd" 'crux-delete-file-and-buffer
		    "fR" 'crux-rename-file-and-buffer))

(use-package eval-sexp-fu
  :custom
  (eval-sexp-fu-flash-duration .2))

(use-package ivy
  :config
  (ivy-mode))

(general-define-key "M-RET" 'eval-defun)

(general-define-key :states 'normal
                    :prefix "SPC"
		    "hdv" 'describe-variable
		    "ff" 'find-file
		    "fs" 'save-buffer
		    "wm" 'delete-other-windows
		    "wo" 'other-window)

(use-package magit
  :general (:states 'normal
		    "gs" 'magit-status))
