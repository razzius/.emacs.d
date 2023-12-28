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
 scroll-conservatively 1
 scroll-margin 2
 visible-bell t)

(use-package general)

(use-package blackout)

(use-package simple
  :straight nil
  :general (:states 'normal
		    :prefix "SPC"
		    "bd" 'kill-current-buffer
		    "bs" 'scratch-buffer))

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

(use-package recentf
  :straight nil
  :general (:states 'normal
		    :prefix "SPC"
		    "fr" 'recentf))

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
		    "bb" 'switch-to-buffer
		    "w=" 'balance-windows
		    "w2" 'split-window-horizontally
		    "w-" 'split-window-vertically
		    "wd" 'delete-window
		    "wh" 'windmove-left
		    "wj" 'windmove-down
		    "wk" 'windmove-up
		    "wl" 'windmove-right
		    "wm" 'delete-other-windows
		    "wo" 'other-window))

(use-package files
  :straight nil
  :custom
  (make-backup-files nil)
  :general (:states 'normal
		    :prefix "SPC"
		    "SPC" 'save-buffer
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
  :init
  (setq evil-want-keybinding nil)

  :custom
  (evil-symbol-word-search t)

  :config
  (evil-mode)

  (use-package evil-commentary
    :config (evil-commentary-mode)
    :blackout)

  (use-package evil-collection
    :config (evil-collection-init)))

(use-package restart-emacs
  :general (:states 'normal
		    :prefix "SPC"
		    "qr" 'restart-emacs))

(defun razzi-vterm-cleanup-on-exit (buf event)
  (when buf (kill-buffer buf))

  ;; Close the split of a closed vterm buffer.
  (when (> (count-windows) 1)
    (delete-window)))

(defun razzi-vterm-split-vertically ()
  (interactive)
  (split-window-vertically)
  (windmove-down)
  (vterm 'new))

(defun razzi-vterm-split-horizontally ()
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (vterm 'new))

(use-package vterm
  :custom
  (vterm-exit-functions '(razzi-vterm-cleanup-on-exit))

  :config
  (evil-set-initial-state 'vterm-mode 'emacs)

  :general
  (:states 'normal
	   :prefix "SPC"
	   "'" 'vterm)

  (:keymaps 'vterm-mode-map
	    :prefix "C-SPC"
	    "" nil
	    "'" 'razzi-vterm-split-vertically
	    "\"" 'razzi-vterm-split-vertically
	    "%" 'razzi-vterm-split-horizontally))

(use-package crux
  :general (:states 'normal
		    :prefix "SPC"
		    "TAB" 'crux-switch-to-previous-buffer
		    "fd" 'crux-delete-file-and-buffer
		    "fR" 'crux-rename-file-and-buffer))

(use-package eval-sexp-fu
  :custom
  (eval-sexp-fu-flash-duration .3))

(use-package ivy
  :config (ivy-mode)
  :blackout)

(use-package magit
  :custom
  (magit-status-sections-hook
   '(magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes
     magit-insert-status-headers
     magit-insert-merge-log
     magit-insert-rebase-sequence
     magit-insert-am-sequence
     magit-insert-sequencer-sequence
     magit-insert-bisect-output
     magit-insert-bisect-rest
     magit-insert-bisect-log
     magit-insert-stashes
     magit-insert-unpushed-to-pushremote
     magit-insert-unpushed-to-upstream-or-recent
     magit-insert-unpulled-from-pushremote
     magit-insert-unpulled-from-upstream))

  :config
  (add-hook 'git-commit-mode-hook 'evil-insert-state)

  :general (:states 'normal
		    "gs" 'magit-status))

(use-package projectile)

(use-package vimrc-mode)

(general-define-key "C-`" 'describe-key)

(defun razzi-edit-init ()
  (interactive)
  (find-file user-init-file))

(general-define-key :states 'normal
		    :prefix "SPC"
		    "fi" 'razzi-edit-init
		    "qq" 'kill-emacs)

(use-package flycheck
  :config
  (global-flycheck-mode))
