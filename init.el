;;; Bootstrap straight and use-package.
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
 enable-local-variables nil
 fill-column 100
 frame-title-format "%f"
 inhibit-startup-screen t
 kill-buffer-query-functions nil
 make-backup-files nil
 ns-pop-up-frames nil
 recentf-max-saved-items 100
 ring-bell-function 'ignore
 save-abbrevs 'silently
 shell-file-name "fish"
 vc-follow-symlinks t)

(global-eldoc-mode -1)
(global-linum-mode)
(tool-bar-mode -1)
(column-number-mode)
(global-auto-revert-mode 1)
(scroll-bar-mode -1)
(server-start)

(define-key input-decode-map "\C-i" [C-i])
(add-hook 'focus-out-hook 'garbage-collect)

;;; Configure packages that others depend on.
(use-package general)

;;; Configure builtin packages.
(use-package recentf
  :config
  (recentf-mode))

(use-package help
  :straight nil
  :custom
  (help-window-select t)
  :general (:keymaps 'help-mode-map
		     "<tab>" 'forward-button))

(use-package blackout)

(use-package subword
  :config
  (global-subword-mode)
  :blackout)

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
  :config
  (load-theme 'zerodark 'noconfirm)
  (set-face-attribute 'default nil :height 180)
  (set-face-attribute 'region nil :background "white" :foreground "black")
  ;; (set-face-background 'hl-line "black")
  (set-background-color "black")
  (set-foreground-color "white"))

(use-package flow-js2-mode)

(use-package evil
  :custom
  (evil-cross-lines t
   evil-ex-substitute-global t
   evil-regexp-search nil
   evil-shift-width 2)

  :config
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
  :general
  (:keymaps 'vterm-mode-map
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

  (:keymaps 'vterm-mode-map
	    :prefix "C-SPC"
	    "" nil
	    "\"" 'razzi-vterm-split-vertically
	    "%" 'razzi-vterm-split-horizontally)

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

  (defun razzi-vterm-split-vertically ()
    (interactive)
    (split-window-vertically)
    (windmove-down)
    (vterm))

  (defun razzi-vterm-split-horizontally ()
    (interactive)
    (split-window-horizontally)
    (windmove-right)
    (vterm))

  (defun razzi-setup-vterm ()
    (setq-local mode-line-format nil)
    (golden-ratio-mode 0)
    (linum-mode 0))

  (defun razzi-vterm-cleanup-on-exit (buf event)
    (when buf (kill-buffer buf))

    ;; Close the split of a closed vterm buffer.
    (when (> (count-windows) 1)
      (delete-window))

    ;; Close the vterm perspective if no more vterm buffers.
    (when (not (razzi-vterm-buffers))
      (persp-kill "vterm")))

  (defun razzi-vterm-buffer-vterm-p (buffer)
    (eq (razzi-buffer-major-mode buffer) 'vterm-mode))

  (defun razzi-vterm-buffers ()
    (seq-filter #'razzi-vterm-buffer-vterm-p (buffer-list)))

  (add-hook 'vterm-mode-hook #'razzi-setup-vterm)
  (setq vterm-exit-functions '(razzi-vterm-cleanup-on-exit)))

(use-package perspective
  :general
  ("M-`" 'razzi-switch-between-terminal)

  :config
  (defun razzi-switch-between-terminal ()
    (interactive)
    (if (> (length (persp-names)) 1)
	(persp-next)
      (progn
	(make-persp :name "vterm")
	(persp-switch "vterm")
	(vterm))))

  (persp-mode)
  (persp-turn-off-modestring))

(use-package crux
  :general (:states 'normal
		    :prefix "SPC"
		    "TAB" 'crux-switch-to-previous-buffer))

(use-package dumb-jump
  :config (dumb-jump-mode))

(use-package flow-minor-mode)

(use-package eval-sexp-fu
  :custom
  (eval-sexp-fu-flash-duration 1)

  :config
  ;; The default duration disappears for some forms that take a while
  ;; to evaluate, like use-package
  (defun razzi-flash-eval-defun ()
    "Hack to make the thing flash even when on an opening parenthesis."
    (interactive)
    (let ((char (razzi-char-at-point)))
      (cond
       ((eq char ?\)) (save-excursion
			(forward-char)
			(call-interactively #'eval-last-sexp)))
	((eq char ?\() (save-excursion
			 (forward-sexp)
			 (call-interactively #'eval-last-sexp)))
	(t (call-interactively #'eval-defun)))))

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
(straight-use-package 'rjsx-mode)

(use-package string-inflection
  :general
  (:states 'normal
	   "c" (general-key-dispatch 'evil-change
		 "ru" 'string-inflection-upcase
		 "rs" 'string-inflection-underscore
		 "rt" 'string-inflection-camelcase
		 "rc" 'string-inflection-lower-camelcase
		 "rd" 'string-inflection-kebab-case
		 "c" 'magit-commit)))

(use-package markdown-mode)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :blackout)

(use-package ripgrep)

(use-package projectile
  :general
  (:states 'normal
	   :prefix "SPC"
	   "p f" 'projectile-find-file
	   "p p" 'projectile-switch-project)

  :custom (projectile-completion-system 'default)
  :config (projectile-mode 1)
  :blackout)

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
	   "M-o" 'sp-end-of-next-sexp
	   "M-v" 'razzi-paste))

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :config
  (selectrum-mode +1)
  (set-face-attribute 'selectrum-current-candidate 'nil :foreground "#34ffff")

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

(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :config (apheleia-global-mode +1)
  :blackout)

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
  ;; (setq golden-ratio-auto-scale t)
  (golden-ratio-mode 1)
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
  :custom
  (sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil)
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
  :custom
  (yas-verbosity 2)
  :config
  (yas-global-mode)
  :blackout yas-minor-mode)

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

(defvar razzi-last-window nil)

(defun razzi-track-selected-window (&rest arguments)
  (setq razzi-last-window (selected-window)))

(advice-add 'windmove-do-window-select :before 'razzi-track-selected-window)

(defun razzi-toggle-window ()
  (interactive)
  (setq previous-window (selected-window))

  (let ((window-length (length (window-list)))
	(window-active (window-live-p razzi-last-window)))
    (when (and window-active (> window-length 2))
      (select-window razzi-last-window))

    (when (and window-active (= window-length 2))
      (other-window 1)))

  (setq razzi-last-window previous-window))

(general-define-key :prefix "C-SPC"
		    "" nil
		    "c" #'vterm
		    "h" #'windmove-left
		    "j" #'windmove-down
		    "k" #'windmove-up
		    "l" #'windmove-right
		    "SPC" #'razzi-toggle-window)

(general-define-key :prefix "M-m"
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
  :general
  ("<C-i>" 'hippie-expand)
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

(use-package python
  :general
  (:states 'normal
	   :prefix "SPC"
	   "i i" 'razzi-import-it-import-this)

  :config
  (defun razzi-import-it-get-import-path ()
    (when (get-buffer "*import_it-output*")
      (with-current-buffer "*import_it-output*"
	(erase-buffer)))

    (let ((error-file (make-temp-file "import_it-error")))
      (call-process
       "import_it"
       nil
       (list "*import_it-output*" error-file)
       nil
       (thing-at-point 'symbol) (s-trim (shell-command-to-string "git root")) (buffer-file-name)))

    (razzi-buffer-string "*import_it-output*"))

  (defun razzi-python-autoflake ()
    (interactive)
    (razzi-run-script-on-file "autoflake --remove-all-unused-imports -i"))

  (defun razzi-python-isort ()
    (interactive)
    (razzi-run-script-on-file "isort"))

  (defun razzi-import-it-import-this ()
    (interactive)
    (save-buffer)
    (let ((import-path (razzi-import-it-get-import-path)))
      (save-excursion
	(goto-char (point-min))
	(insert import-path))
      (razzi-python-isort)
      (razzi-python-autoflake)
      (flycheck-buffer)))

  (defun razzi-configure-python-mode ()
    (flycheck-mode)
    (setq evil-shift-width 4))

  (add-hook 'python-mode-hook 'razzi-configure-python-mode))


(defun razzi-make-parent-directories (filename &optional wildcards)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
	(make-directory dir)))))

(use-package centaur-tabs
  :custom
  (centaur-tabs-cycle-scope 'tabs
   centaur-tabs-set-close-button nil
   centaur-tabs-hide-tab-function 'razzi-tabs-hide-special-tabs)

  :general
  ("C-<tab>" 'centaur-tabs-forward
   "C-S-<tab>" 'centaur-tabs-backward
   "M-1" 'centaur-tabs-select-beg-tab
   "M-2" 'centaur-tabs-select-visible-tab
   "M-3" 'centaur-tabs-select-visible-tab
   "M-4" 'centaur-tabs-select-visible-tab
   "M-5" 'centaur-tabs-select-visible-tab
   "M-6" 'centaur-tabs-select-visible-tab
   "M-7" 'centaur-tabs-select-visible-tab
   "M-8" 'centaur-tabs-select-visible-tab
   "M-9" 'centaur-tabs-select-end-tab)
  (:states 'normal
   "g t" 'centaur-tabs-forward
   "g T" 'centaur-tabs-backward)

   :config
   (defun razzi-tabs-hide-special-tabs (buffer)
     (let ((name (format "%s" buffer)))
       (or
	(string-prefix-p "*" name)
	(centaur-tabs-hide-tab buffer)
	(and (string-prefix-p "magit" name)
	     (s-contains? ":" name)))))

   (centaur-tabs-mode))

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

(use-package isearch
  :straight nil
  :general
  (:keymaps 'isearch-mode-map
	    "C-t" 'razzi-isearch-transpose-char
	    "C-g" 'isearch-exit)
  :config
  (defun razzi-isearch-transpose-char ()
    (interactive)
    (let* ((string isearch-string)
	   (len (length isearch-string))
	   (second-to-last-char (aref string (- len 2)))
	   (last-char (aref string (- len 1))))
      (isearch-pop-state)
      (isearch-pop-state)
      (isearch-process-search-char last-char)
      (isearch-process-search-char second-to-last-char))))
