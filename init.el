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

(setq
 fill-column 100
 ring-bell-function 'ignore
 shell-file-name "/bin/bash"
 evil-cross-lines t
 evil-ex-substitute-global t
 evil-regexp-search nil
 save-abbrevs 'silently
 ivy-initial-inputs-alist nil
 evil-shift-width 2
 straight-use-package-by-default t
 vc-follow-symlinks t
 frame-title-format "%f"
 inhibit-startup-screen t
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 recentf-max-saved-items 100
 ns-pop-up-frames nil)

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
(straight-use-package 'flycheck-package)
(straight-use-package 'general)
(straight-use-package 'golden-ratio)
(straight-use-package 'ivy)
(straight-use-package 'js2-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'magit)

(straight-use-package 'restart-emacs)
(straight-use-package 'ripgrep)
(straight-use-package 'rjsx-mode)
(straight-use-package 'smartparens)
(straight-use-package 'swiper)
(straight-use-package 'string-inflection)
(straight-use-package 'use-package)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package projectile
  :config (projectile-mode 1))

(use-package flycheck-flow
  :config
  (add-hook 'javascript-mode-hook 'flycheck-mode))

(straight-use-package
 '(zerodark-theme :host github :repo "NicolasPetton/zerodark-theme"))

(straight-use-package
 '(razzi :host github :repo "razzius/razzi.el"))

(use-package js-comint
  :config
  (setq js-comint-program-command "/Users/razzi/apps/pick-api/node_modules/.bin/babel-node"))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package flycheck-flow)

(use-package tern
  :init (add-hook 'js2-mode-hook 'tern-mode))

(use-package golden-ratio
  :config
  (golden-ratio-mode))

(use-package eval-sexp-fu)

;; (use-package evil-mc
;;   :config
;;   (global-evil-mc-mode 1))

(use-package iedit)

(use-package counsel
  :straight t
  :config
  (counsel-mode +1))

(use-package smartparens
  :config
  (smartparens-global-mode)
  (sp-with-modes sp--lisp-modes
    ;; Disable ' as it's the quote character.
    (sp-local-pair "'" nil :actions nil)))

(ivy-mode)
(eldoc-mode -1)
(global-linum-mode)
(evil-mode 1)
(evil-magit-init)
(evil-commentary-mode)
(set-face-attribute 'default nil :height 180)
(dumb-jump-mode)
(recentf-mode)
(tool-bar-mode -1)
(column-number-mode)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(global-subword-mode)

(use-package evil-surround :config
  (global-evil-surround-mode))

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

(use-package flow-js2-mode)

(use-package super-save
  :config
  (super-save-mode))

(use-package yasnippet
  :config
  (yas-minor-mode))

(add-hook 'js2-mode-hook 'flycheck-mode)
(razzi-associate-extension-mode "js" 'rjsx-mode)

(setq-default
 evil-symbol-word-search t)

(general-define-key "C-`" 'describe-key)

(general-auto-unbind-keys)

(defun razzi-evil-mc-quit-and-quit ()
  (interactive)
  (when (and (boundp 'iedit-mode) iedit-mode) (iedit-mode))
  (keyboard-quit))

(general-define-key :states 'normal
		    :prefix "SPC"
		    "bb" 'ivy-switch-buffer
		    "bs" 'razzi-switch-to-scratch-buffer
		    "bn" 'next-buffer
		    "bp" 'razzi-previous-useful-buffer
		    "bd" 'kill-buffer
		    "el" 'flycheck-list-errors
		    "en" 'flycheck-next-error
		    "ep" 'flycheck-previous-error
		    "hdf" 'describe-function
		    "hdv" 'describe-variable
		    "wd" 'delete-window
		    "wo" 'other-window
		    "/" 'counsel-rg
		    "," 'razzi-append-comma
		    "qr" 'restart-emacs
		    "qq" 'save-buffers-kill-terminal
		    "sl" 'ivy-resume
		    "pf" 'projectile-find-file
		    "ss" 'swiper
		    "td" 'toggle-debug-on-error
		    "wk" 'evil-window-up
		    "w2" 'evil-window-vsplit
		    "w-" 'evil-window-split
		    "wl" 'evil-window-right
		    "wj" 'evil-window-down
		    "wk" 'evil-window-up
		    "wm" 'delete-other-windows
		    "fi" 'crux-find-user-init-file
		    "ff" 'razzi-find-relative-to-current
		    "f/" 'find-file ; / because this works from room. eh might not need it
		    "ft" (lambda () (interactive) (find-file (replace-regexp-in-string "index.js" "test.js" (buffer-file-name))))
		    "fe" (lambda () (interactive) (find-file (replace-regexp-in-string "test.js" "index.js" (buffer-file-name))))
		    "fp" 'razzi-copy-project-file-path
		    "fr" 'counsel-recentf
		    "f SPC" 'razzi-copy-file-name-to-clipboard
		    "o" 'razzi-put-after
		    "tg" 'golden-ratio-mode
		    "O" 'razzi-put-before
		    "ESC" 'kill-this-buffer
		    "SPC" 'execute-extended-command
		    "TAB" 'crux-switch-to-previous-buffer)

(general-define-key :states 'normal
		    "<up>" 'evil-scroll-line-up
		    "<down>" 'evil-scroll-line-down
		    "-" 'razzi-transpose-next-line
		    "0" 'evil-first-non-blank
		    "C-c r" 'rjsx-rename-tag-at-point
		    "C-g" 'razzi-evil-mc-quit-and-quit
		    "[ SPC" 'razzi-insert-newline-before
		    "] SPC" 'razzi-insert-newline-after
		    "_" 'razzi-transpose-previous-line
		    "c" (general-key-dispatch 'evil-change
			  "ru" 'string-inflection-upcase
			  "rs" 'string-inflection-underscore
			  "rt" 'string-inflection-camelcase
			  "rc" 'string-inflection-lower-camelcase
			  "rd" 'string-inflection-kebab-case
			  "c" 'magit-commit)
		    "C" 'razzi-change-line
		    "D" 'razzi-kill-line-and-whitespace
		    "M-/" 'evil-commentary-line
		    "M-RET" 'eval-defun
		    "SPC RET" 'eval-defun ; experiment
		    "M-[" 'evil-backward-paragraph
		    "M-]" 'evil-forward-paragraph
		    "M-d" 'iedit-mode
		    "M-f" 'evil-ex-search-forward
		    "M-l" 'evil-visual-line
		    "M-n" 'flycheck-next-error
		    "M-p" 'flycheck-previous-error
		    "M-r" 'raise-sexp
		    "M-s" 'razzi-flycheck-and-save-buffer
		    "M-u" 'razzi-update-current-package
		    "M-w" 'kill-current-buffer
		    "Q" 'razzi-replay-q-macro
		    "g/" 'razzi-ivy-search-at-point
		    "g]" 'dumb-jump-go
		    "gb" 'magit-blame-addition
		    "gs" 'magit-status
		    "o" 'razzi-open-with-comma
		    ;; "/" 'evil-ex-search-forward
		    "<backtab>" 'razzi-previous-useful-buffer)

(general-define-key :states 'insert
		    "<tab>" 'yas-expand
		    "<C-i>" 'hippie-expand
		    "C-l" 'sp-forward-slurp-sexp
		    "C-t" 'razzi-transpose-previous-chars
		    "C-c a" 'razzi-abbrev-or-add-global-abbrev
		    "s-<backspace>" 'evil-delete-backward-word
		    "M-/" 'evil-commentary-line
		    "M-l" 'evil-visual-line
		    "M-s" 'razzi-exit-insert-and-save
		    "M-v" 'razzi-paste
		    "M-t" 'transpose-words
		    "M-RET" 'eval-defun)

(define-key input-decode-map "\C-i" [C-i]) ; does this work?

(evil-define-text-object whole-buffer (count &optional beginning end type)
  (evil-range 0 (point-max)))

(general-define-key :states 'operator
		    "E" 'forward-symbol
		    "ae" 'whole-buffer
		    "SPC" 'evil-inner-symbol)

(general-define-key :states 'visual
		    "c" 'evil-change
		    "s" 'evil-surround-region
		    "il" 'razzi-mark-line-text
		    "SPC SPC" 'execute-extended-command
		    "'" 'razzi-surround-with-single-quotes
		    ")" 'razzi-surround-with-parens
		    "]" 'razzi-surround-with-brackets
		    "M-l" 'evil-next-line
		    "M-RET" 'eval-region
		    "SPC RET" 'eval-region
		    "\"" 'razzi-surround-with-double-quotes)


(general-define-key :modes ivy-mode
		    "C-h" 'ivy-backward-delete-char)

(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-strict-missing-semi-warning nil)
(setq js-indent-level 2)

(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'focus-out-hook 'garbage-collect)

(use-package hippie-exp
  :config
  (setq hippie-expand-try-functions-list
	'(try-expand-line try-expand-line-all-buffers))

  (defun hippie-expand-substitute-string (arg)
    "Remove extra paren when expanding line in smartparens"
    (if (and smartparens-mode
  	     (memq (razzi-char-at-point) '(?} ?\))))
  	(delete-char 1)))

  (advice-add 'hippie-expand :after 'hippie-expand-substitute-string))

(use-package prettier-js :config
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(add-hook 'focus-out-hook 'garbage-collect)
(mapc 'evil-declare-not-repeat '(flycheck-next-error flycheck-previous-error razzi-flycheck-and-save-buffer))
(add-hook 'python-mode-hook (lambda ()
			      (setq evil-shift-width 4)))


(defun razzi-make-parent-directories (filename)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
	(make-directory dir)))))

(defun cd-project-root ()
  (cd (projectile-project-root)))

(add-hook 'find-file-hook 'cd-project-root)

(advice-add 'find-file :before 'razzi-make-parent-directories)

;; feature: paste into search minibuffer
;; currently does not work
(define-key minibuffer-local-map (kbd "M-v") 'yank)
;; (define-key evil-ex-completion-map "\C-r" #'evil-paste-from-register)
(define-key evil-ex-completion-map (kbd "M-v") 'yank)
; does this need to refer to evil-vars?
(eval-after-load 'evil-vars '(define-key evil-ex-completion-map (kbd "M-v") 'isearch-yank-kill))


(defun razzi/magit-push ()
  (interactive)
  (magit-run-git "push"))

(magit-define-popup-action 'magit-push-popup ?p "Push current :D" 'razzi/magit-push)

(use-package zerodark-theme
  :demand t)

(use-package dired+
  :config
  (setq
   dired-recursive-copies 'always
   dired-recursive-deletes 'always

   diredp-hide-details-initially-flag t)
  (define-key dired-mode-map (kbd "c") 'find-file)
  (define-key dired-mode-map (kbd ".") 'dired-up-directory)
  (define-key dired-mode-map (kbd "C-h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "gs") 'magit-status)

  (add-hook 'dired-mode-hook 'dired-hide-details-mode))
; isearch c-t not work
; visual u makes lowercase - not good
; ui for number of errors
; visual V to select the current paragraph or eol other brace: $%
; goto-last-change: Invalid function: undo-tree-node-p
; spc-based keybindings in * buffers: spc esc does not close
; search then n does not work...? seems to use a different buffer
; normal c-a increase number
; ripgrep file names take up whole screen
; visual eval flash region
; git push current with progress or async - editor hangs at the moment
; gc uncomment removes end-of-line //
; partial keybinding show options
; show app folder in title bar
; emacs without basic title bar
; D moves ; comments to the right (try it here)
; spc i c insert copy
; spc ret eval
