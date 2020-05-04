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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq
 auto-save-default nil
 create-lockfiles nil
 default-directory (expand-file-name "~")
 fill-column 100
 frame-title-format "%f"
 inhibit-startup-screen t
 kill-buffer-query-functions nil
 make-backup-files nil
 recentf-max-saved-items 100
 ring-bell-function 'ignore
 save-abbrevs 'silently
 shell-file-name "fish"
 vc-follow-symlinks t
 ns-pop-up-frames nil)

(global-eldoc-mode -1)
(global-linum-mode)
(set-face-attribute 'default nil :height 180)
(recentf-mode)
(tool-bar-mode -1)
(column-number-mode)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(server-start)

(use-package blackout)

(use-package subword
  :config
  (global-subword-mode)
  :blackout)

(define-key input-decode-map "\C-i" [C-i])

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super
	mac-command-modifier 'meta))

(defun razzi-add-directory-to-path (directory)
  (setq exec-path (append exec-path (list directory)))
  (setenv "PATH" (concat (getenv "PATH") ":" directory)))

(razzi-add-directory-to-path "/usr/local/bin")
(razzi-add-directory-to-path (expand-file-name "~/.local/bin"))

(use-package zerodark-theme
  :straight (:host github :repo "NicolasPetton/zerodark-theme")
  :config (load-theme 'zerodark 'noconfirm))

(use-package general)

(use-package flow-js2-mode)

(general-define-key :keymaps 'help-mode-map "<tab>" 'forward-button)

(use-package evil
  :config
  (setq evil-cross-lines t
	evil-ex-substitute-global t
	evil-regexp-search nil
	evil-shift-width 2)

  (setq-default evil-symbol-word-search t)

  (evil-mode 1)

  (mapc 'evil-declare-not-repeat
	'(flycheck-next-error
	  flycheck-previous-error
	  razzi-flycheck-and-save-buffer))

  (evil-define-text-object whole-buffer (count &optional beginning end type)
    (evil-range 0 (point-max)))

  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  (use-package evil-matchit
    :config
    (global-evil-matchit-mode 1))

  (use-package evil-magit
    :config (evil-magit-init))

  (use-package evil-commentary
    :config (evil-commentary-mode)
    :blackout)

  (use-package evil-numbers
    :general (:states 'normal "C-a" 'evil-numbers/inc-at-pt)))

(use-package eshell
  :config
  (evil-set-initial-state 'eshell-mode 'emacs))

(use-package vterm
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)

  (defun razzi-vterm-send-c-w ()
    (interactive)
    (vterm-send-key "w" nil nil t))

  (defun razzi-vterm-send-m-b ()
    (interactive)
    (vterm-send-key "b" nil t nil))

  (defun razzi-vterm-send-m-f ()
    (interactive)
    (vterm-send-key "f" nil t nil))

  ;; These next 2 require fish integration
  (defun razzi-vterm-send-s-up ()
    (interactive)
    (vterm-send-key "<up>" t nil nil))

  (defun razzi-vterm-send-s-down ()
    (interactive)
    (vterm-send-key "<down>" t nil nil))

  (general-define-key :keymaps 'vterm-mode-map
		      "<tab>" #'vterm--self-insert
		      "C-a" #'vterm--self-insert
		      "C-c" #'vterm--self-insert
		      "C-e" #'vterm--self-insert
		      "C-h" #'vterm--self-insert
		      "C-n" #'vterm--self-insert
		      "C-p" #'vterm--self-insert
		      "C-u" #'vterm--self-insert

		      "M-v" #'vterm-yank
		      "M-w" #'kill-this-buffer

		      "<s-backspace>" #'razzi-vterm-send-c-w

		      ;; todo bind other than arrow keys to stay on home row
		      "<s-up>" #'razzi-vterm-send-s-up
		      "<s-down>" #'razzi-vterm-send-s-down

		      ;; These are remapped to c-q and c-v system-wide
		      "<s-left>" #'razzi-vterm-send-m-b
		      "<s-right>" #'razzi-vterm-send-m-f)

  (general-define-key :keymaps 'vterm-mode-map
		      :prefix "C-SPC"
		      "" nil
		      "c" 'vterm))

(use-package crux
  :general (:states 'normal
		    :prefix "SPC"
		    "TAB" 'crux-switch-to-previous-buffer))

(use-package dumb-jump
  :config (dumb-jump-mode))

(use-package flow-minor-mode)

(use-package vterm-toggle
  :general ("M-`" 'vterm-toggle))

(use-package eval-sexp-fu
  :config
  ;; The default duration disappears for some forms that take a while
  ;; to evaluate, like use-package
  (setq eval-sexp-fu-flash-duration .3)

  (defun razzi-flash-eval-defun ()
    "Hack to make the thing flash even when on an opening parenthesis."
    (interactive)
    (save-excursion
      (when (string= (thing-at-point 'char) "(")
	(forward-char))
      (call-interactively 'eval-defun)))

  (general-define-key "M-RET" 'razzi-flash-eval-defun))

(use-package flycheck
  :config
  (setq flycheck-flake8rc "~/.config/flake8"
	flycheck-python-flake8-executable "flake8")
  (setq-default flycheck-disabled-checkers '(python-pycompile python-pylint)))

(use-package flycheck-mypy
  :config
  (setq flycheck-python-mypy-args '("--ignore-missing-imports" "--follow-imports=silent")))

(use-package pipenv)

(use-package flycheck-package)

(straight-use-package 'js2-mode)
(straight-use-package 'restart-emacs)
(straight-use-package 'ripgrep)
(straight-use-package 'rjsx-mode)
(straight-use-package 'string-inflection)

(use-package markdown-mode)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :blackout)

(use-package projectile
  :config (projectile-mode 1))

(use-package flycheck-flow
  :config
  (add-hook 'javascript-mode-hook 'flycheck-mode))

(use-package razzi
  :straight (:host github :repo "razzius/razzi.el")
  :general
  ("M-s" 'razzi-flycheck-and-save-buffer)
  (:states 'normal
	    "C-c r" 'web-mode-element-rename
	    "<backtab>" 'razzi-previous-useful-buffer
	    "[ SPC" 'razzi-insert-newline-before
	    "] SPC" 'razzi-insert-newline-after
	    "-" 'razzi-transpose-next-line
	    "_" 'razzi-transpose-previous-line
	    "g s" 'razzi-save-and-magit-status
	    "C" 'razzi-change-line
	    "D" 'razzi-kill-line-and-whitespace
	    "G" 'razzi-almost-end-of-buffer
	    "Q" 'razzi-replay-q-macro)
  (:states 'normal
	    :prefix "SPC"
	    "," 'razzi-append-comma
	    "o" 'razzi-put-after
	    "i d" 'razzi-put-debugger
	    "f r" 'razzi-recentf
	    "q r" 'razzi-restart-emacs)
  (:states 'visual
	    "$" 'razzi-almost-end-of-line
	    "il" 'razzi-mark-line-text)
  (:states 'insert
	    "C-t" 'razzi-transpose-previous-chars
	    "C-c a" 'razzi-abbrev-or-add-global-abbrev
	    "M-v" 'razzi-paste))

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :config
  (selectrum-mode +1)

  (defun razzi-delete-backward-to-slash ()
    (interactive)
    (zap-up-to-char -1 ?/))

  (defun razzi-go-home ()
    (interactive)
    (beginning-of-line)
    (kill-line)
    (insert "~/"))

  (defun razzi-minibuffer-bindings ()
    (local-set-key (kbd "~") 'razzi-go-home)
    (local-set-key (kbd "M-v") 'yank)
    (local-set-key (kbd "C-h") 'razzi-delete-backward-to-slash))

  (add-hook 'minibuffer-setup-hook 'razzi-minibuffer-bindings))

(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el" :files ("selectrum-prescient.el"))
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; (defun razzi-continue-last-search ()
;;   (previous-history-element 1))

;; (defun razzi-m-f ()
;;   (interactive)
;;   (add-hook 'minibuffer-setup-hook 'razzi-continue-last-search)
;;   (unwind-protect
;;       (ctrlf-forward-literal)
;;     (remove-hook 'minibuffer-setup-hook 'razzi-continue-last-search)))

;; (general-define-key :states 'normal "n" 'razzi-m-f)

(use-package ctrlf
  :straight (:host github :repo "raxod502/ctrlf")
  :general (:states 'normal "M-f" 'ctrlf-forward-literal))

;; (use-package tern
;;   :init (add-hook 'js2-mode-hook 'tern-mode))

(use-package golden-ratio
  :config
  (golden-ratio-mode)
  :blackout)


(use-package eval-sexp-fu)
(use-package iedit
  :config
  (defun razzi-iedit-quit-and-quit ()
    (interactive)
    (when (and (boundp 'iedit-mode) iedit-mode) (iedit-mode))
    (keyboard-quit))

  (general-define-key :states 'normal "C-g" 'razzi-iedit-quit-and-quit)

  (general-define-key :states 'normal
		      :prefix "SPC"
		      "ie" 'iedit-mode))

(use-package smartparens
  :config
  (smartparens-global-mode)
  (sp-with-modes sp--lisp-modes
    ;; Disable ' as it's the quote character.
    (sp-local-pair "'" nil :actions nil))
  :blackout)

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

(use-package super-save
  :config
  (super-save-mode)
  :blackout)

(use-package yasnippet
  :config
  (yas-minor-mode))

(add-hook 'js2-mode-hook 'flycheck-mode)
(razzi-associate-extension-mode "js" 'rjsx-mode)

(general-define-key "C-`" 'describe-key
		    "M-w" 'kill-current-buffer
		    "M-q" 'save-buffers-kill-terminal)

(general-auto-unbind-keys)

(general-define-key :states 'normal
		    :prefix ","
		    "ee" 'eval-last-sexp
		    "ec" 'eval-defun)

(general-define-key :states 'emacs
		    :prefix "M-m"
		    "fi" 'razzi-find-init)

(defun razzi-find-init ()
  (interactive)
  (find-file (expand-file-name
	      (concat (cdadr (assoc chemacs-current-emacs-profile chemacs-emacs-profiles))
		      "/init.el"))))

(general-define-key :states 'normal
		    :prefix "SPC"
		    "," 'razzi-append-comma
		    "bb" 'switch-to-buffer
		    "bd" 'kill-buffer
		    "bn" 'next-buffer
		    "bp" 'razzi-previous-useful-buffer
		    "bs" 'razzi-switch-to-scratch-buffer
		    "el" 'flycheck-list-errors
		    "en" 'flycheck-next-error
		    "ep" 'flycheck-previous-error
		    "ev" 'flycheck-verify-setup
		    "f RET" 'razzi-copy-project-file-path
		    "hdf" 'describe-function
		    "hdv" 'describe-variable
		    "ff" 'find-file
		    "fi" 'razzi-find-init
		    "pf" 'projectile-find-file
		    "qq" 'save-buffers-kill-terminal
		    "qr" 'razzi-restart-emacs
		    "td" 'toggle-debug-on-error
		    "w-" 'evil-window-split
		    "w2" 'evil-window-vsplit
		    "wd" 'delete-window
		    "wh" 'evil-window-left
		    "wj" 'evil-window-down
		    "wk" 'evil-window-up
		    "wl" 'evil-window-right
		    "wm" 'delete-other-windows
		    "wo" 'other-window
		    "fp" 'razzi-copy-project-file-path
		    "fr" 'razzi-recentf
		    "f SPC" 'razzi-copy-file-name-to-clipboard
		    "o" 'razzi-put-after
		    "tg" 'golden-ratio-mode
		    "O" 'razzi-put-before
		    "u" 'universal-argument
		    "ESC" 'kill-this-buffer
		    "SPC" 'execute-extended-command)

(general-define-key :states 'normal
		    "<up>" 'evil-scroll-line-up
		    "<down>" 'evil-scroll-line-down
		    "<C-i>" 'evil-jump-forward
		    "/" 'evil-search-forward
		    "0" 'evil-first-non-blank
		    "C-c r" 'rjsx-rename-tag-at-point
		    "[ SPC" 'razzi-insert-newline-before
		    "] SPC" 'razzi-insert-newline-after
		    "c" (general-key-dispatch 'evil-change
			  "ru" 'string-inflection-upcase
			  "rs" 'string-inflection-underscore
			  "rt" 'string-inflection-camelcase
			  "rc" 'string-inflection-lower-camelcase
			  "rd" 'string-inflection-kebab-case
			  "c" 'magit-commit)
		    "C" 'razzi-change-line
		    "D" 'razzi-kill-line-and-whitespace
		    "K" 'evil-previous-line  ; Protect against typo
		    "M-/" 'evil-commentary-line
		    "M-[" 'evil-backward-paragraph
		    "M-]" 'evil-forward-paragraph
		    "M-l" 'evil-visual-line
		    "M-n" 'flycheck-next-error
		    "M-p" 'flycheck-previous-error
		    "M-r" 'raise-sexp
		    "M-s" 'razzi-flycheck-and-save-buffer
		    "M-u" 'razzi-update-current-package
		    "M-w" 'kill-current-buffer
		    "Q" 'razzi-replay-q-macro
		    ;; "g/" 'razzi-ivy-search-at-point
		    "g]" 'dumb-jump-go
		    "gb" 'magit-blame-addition
		    "gs" 'magit-status
		    "o" 'razzi-open-with-comma
		    "<backtab>" 'razzi-previous-useful-buffer)

(general-define-key :states 'insert
		    "<tab>" 'yas-expand
		    "<C-i>" 'hippie-expand
		    "C-h" 'delete-backward-char
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

(general-define-key :states 'operator
		    "E" 'forward-symbol
		    "ae" 'whole-buffer
		    "SPC" 'evil-inner-symbol)

(general-define-key :states 'visual
		    "$" 'evil-last-non-blank
		    "'" 'razzi-surround-with-single-quotes
		    ")" 'razzi-surround-with-parens
		    "0" 'evil-first-non-blank
		    "K" 'evil-previous-line  ; Protect against typo
		    "M-RET" 'eval-region
		    "M-l" 'evil-next-line
		    "SPC RET" 'eval-region
		    "SPC SPC" 'execute-extended-command
		    "\"" 'razzi-surround-with-double-quotes
		    "]" 'razzi-surround-with-brackets
		    "c" 'evil-change
		    "il" 'razzi-mark-line-text
		    "s" 'evil-surround-region
		    "v" 'evil-normal-state)

(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-strict-missing-semi-warning nil)
(setq js-indent-level 2)

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
(add-hook 'python-mode-hook (lambda ()
			      (flycheck-mode)
			      (setq evil-shift-width 4)))

(defun razzi-make-parent-directories (filename &optional wildcards)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
	(make-directory dir)))))

(defun cd-project-root ()
  (let ((root (projectile-project-root)))
    (when root (cd root))))

(add-hook 'find-file-hook 'cd-project-root)

(advice-add 'find-file :before 'razzi-make-parent-directories)

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

(use-package which-key
  :config
  (which-key-mode)
  :blackout)

(defun razzi-isearch-transpose-char ()
  (interactive)
  (let* ((string isearch-string)
         (len (length isearch-string))
         (second-to-last-char (aref string (- len 2)))
         (last-char (aref string (- len 1))))
    (isearch-pop-state)
    (isearch-pop-state)
    (isearch-process-search-char last-char)
    (isearch-process-search-char second-to-last-char)))

(define-key isearch-mode-map (kbd "C-t") 'razzi-isearch-transpose-char)
(define-key isearch-mode-map (kbd "C-g") 'isearch-quit)
