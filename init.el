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
(evil-mode 1)
(evil-commentary-mode)
(set-face-attribute 'default nil :height 180)
(dumb-jump-mode)
(recentf-mode)

(use-package evil-surround)
;; (global-evil-surround-mode)

; this should load automatically
(use-package flow-js2-mode)

(add-hook 'js2-mode-hook 'flycheck-mode)
(razzi-associate-extension-mode "js" rjsx-mode)

(tool-bar-mode -1)

(defun edit-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

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
		    "hdf" 'describe-funciton
		    "wd" 'delete-window
		    "qr" 'restart-emacs
		    "qq" 'save-buffers-kill-terminal
		    "wk" 'evil-window-up
		    "wj" 'evil-window-down
		    "wm" 'delete-other-windows
		    "fi" 'edit-init
		    "ff" 'find-file
		    "fr" 'crux-recentf-find-file
		    "f SPC" 'copy-file-name-to-clipboard
		    "ESC" 'kill-this-buffer
		    "SPC" 'execute-extended-command
		    "TAB" 'previous-buffer)

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(general-define-key :states 'insert "C-t" 'razzi-transpose-previous-chars)

; (general-define-key :modes
(general-define-key :states 'normal
		    "[ SPC" 'razzi-insert-newline-before
		    "] SPC" 'razzi-insert-newline-after
		    "g]" 'dumb-jump-go
		    "gs" 'magit-status
		    "M-RET" 'eval-defun
		    "<backtab>" 'crux-switch-to-previous-buffer)

(general-define-key :states 'operator
  "E" 'forward-symbol
  "ae" 'whole-buffer
  "SPC" 'evil-inner-symbol)

(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-strict-missing-semi-warning nil)

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))
