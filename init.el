(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package '(flow-js2-mode :type git :host github :repo "Fuco1/flow-js2-mode"))
(straight-use-package 'crux)
(straight-use-package 'dumb-jump)
(straight-use-package 'eval-sexp-fu)
(straight-use-package 'evil)
(straight-use-package 'evil-commentary)
(straight-use-package 'evil-magit)
(straight-use-package 'evil-surround)
(straight-use-package 'flow-minor-mode)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-package)
(straight-use-package 'general)
(straight-use-package 'ivy)
(straight-use-package 'js2-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'magit)
(straight-use-package 'restart-emacs)
(straight-use-package 'rjsx-mode)
(straight-use-package 'smartparens)
(straight-use-package 'use-package)

(straight-use-package
 '(zerodark-theme :host github :repo "NicolasPetton/zerodark-theme"))

(straight-use-package
 '(razzi :host github :repo "razzius/razzi.el"))

(use-package zerodark-theme)

(use-package eval-sexp-fu)

(use-package smartparens)
;; (use-package counsel
;;   :config
;;   (counsel-mode +1))

(ivy-mode)
(eldoc-mode)
(evil-mode 1)
(evil-magit-init)
(evil-commentary-mode)
(set-face-attribute 'default nil :height 180)
(dumb-jump-mode)
(recentf-mode)
(tool-bar-mode -1)

(use-package evil-surround)
(global-evil-surround-mode)

(use-package magit
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)

  (add-hook 'git-commit-mode-hook 'evil-insert-state)

  (setq
    magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
    magit-push-current-set-remote-if-missing nil
    magit-commit-show-diff nil
    magit-status-sections-hook
    '(magit-insert-merge-log
      magit-insert-rebase-sequence
      magit-insert-am-sequence
      magit-insert-sequencer-sequence
      magit-insert-bisect-output
      magit-insert-bisect-rest
      magit-insert-bisect-log
      magit-insert-unstaged-changes
      magit-insert-untracked-files
      magit-insert-staged-changes
      magit-insert-status-headers
      magit-insert-stashes
      magit-insert-unpulled-from-upstream
      magit-insert-unpulled-from-pushremote
      magit-insert-unpushed-to-upstream
      magit-insert-unpushed-to-pushremote)))

; this should load automatically
(use-package flow-js2-mode)

(add-hook 'js2-mode-hook 'flycheck-mode)
(razzi-associate-extension-mode "js" 'rjsx-mode)

(setq
 frame-title-format "%f"
 inhibit-startup-screen t
 ns-pop-up-frames nil)

(setq-default
 evil-symbol-word-search t)

(general-define-key "C-`" 'describe-key)
(general-auto-unbind-keys)
(general-define-key :states 'normal
		    :prefix "SPC"
		    "bb" 'ivy-switch-buffer
		    "bd" 'kill-buffer
		    "en" 'flycheck-next-error
		    "ep" 'flycheck-previous-error
		    "hdf" 'describe-function
		    "wd" 'delete-window
		    "wo" 'other-window
		    "qr" 'restart-emacs
		    "qq" 'save-buffers-kill-terminal
		    "wk" 'evil-window-up
		    "wj" 'evil-window-down
		    "wm" 'delete-other-windows
		    "fi" 'crux-find-user-init-file
		    "ff" 'find-file
		    "fr" 'crux-recentf-find-file
		    "f SPC" 'razzi-copy-file-name-to-clipboard
		    "o" 'razzi-put-after
		    "O" 'razzi-put-before
		    "ESC" 'kill-this-buffer
		    "SPC" 'execute-extended-command
		    "TAB" 'razzi-previous-useful-buffer)

(general-define-key :states 'insert
		    "C-t" 'razzi-transpose-previous-chars
		    "C-c a" 'razzi-abbrev-or-add-global-abbrev
		    "M-s" 'razzi-exit-insert-and-save
		    "M-RET" 'eval-defun)

(general-define-key :states 'normal
		    "[ SPC" 'razzi-insert-newline-before
		    "] SPC" 'razzi-insert-newline-after
		    "D" 'razzi-kill-line-and-whitespace
		    "g]" 'dumb-jump-go
		    "gs" 'magit-status
		    "M-s" 'save-buffer
		    "M-r" 'raise-sexp
		    "M-RET" 'eval-defun
		    "<backtab>" 'crux-switch-to-previous-buffer)

(general-define-key :states 'operator
  "E" 'forward-symbol
  "ae" 'whole-buffer
  "SPC" 'evil-inner-symbol)

(general-define-key :states 'visual
		    "s" 'evil-surround-region)

(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-strict-missing-semi-warning nil)

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))
