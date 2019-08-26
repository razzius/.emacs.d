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
 enable-local-variables :safe
 ring-bell-function 'ignore
 evil-cross-lines t
 evil-ex-substitute-global t
 evil-regexp-search nil
 save-abbrevs 'silently
 indent-tabs-mode nil
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
 vterm-shell "fish"
 ns-pop-up-frames nil)

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

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
(straight-use-package 'golden-ratio)
(straight-use-package 'ivy)
(straight-use-package 'js2-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'restart-emacs)
(straight-use-package 'ripgrep)
(straight-use-package 'rjsx-mode)
(straight-use-package 'smartparens)
(straight-use-package 'swiper)
(straight-use-package 'string-inflection)
(straight-use-package 'use-package)

(straight-use-package
 '(zerodark-theme :host github :repo "NicolasPetton/zerodark-theme"))

(straight-use-package
 '(razzi :host github :repo "razzius/razzi.el"))

(straight-use-package
 '(vterm :local-repo "~/forks/emacs-libvterm"))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package flycheck-flow)

(use-package golden-ratio
  :config
  (golden-ratio-mode))

(use-package eval-sexp-fu)

;; (use-package evil-mc
;;   :config
;;   (global-evil-mc-mode 1))

(use-package web-mode)

(use-package iedit)

;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-copy-env "SHELL"))

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
(evil-mode 1)
(evil-magit-init)
(evil-commentary-mode)
(set-face-attribute 'default nil :height 180)
(dumb-jump-mode)
(recentf-mode)
(tool-bar-mode -1)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(global-subword-mode)
(global-linum-mode)
(global-whitespace-mode 1)
(setq whitespace-style '(trailing tabs tab-mark))

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
  (yas-global-mode 1))

(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)

(razzi-associate-extension-mode "js" 'rjsx-mode)
(razzi-associate-extension-mode "html" 'web-mode)

(setq-default
 evil-symbol-word-search t)

(general-define-key "C-`" 'describe-key)

;; (general-define-key "<s-right>" nil)

(general-auto-unbind-keys)

(defun razzi-evil-mc-quit-and-quit ()
  (interactive)
  (when (and (boundp 'iedit-mode) iedit-mode) (iedit-mode))
  (keyboard-quit))

(defun razzi-add-normal-keybinding ()
  (interactive)
  (message "TODO"))

(general-define-key :states 'normal
                    "$" 'razzi-almost-end-of-line
                    "-" 'razzi-transpose-next-line
                    "_" 'razzi-transpose-previous-line
                    "[ SPC" 'razzi-insert-newline-before
                    "] SPC" 'razzi-insert-newline-after
                    ;; "C-c r" 'rjsx-rename-tag-at-point
                    "C-g" 'razzi-evil-mc-quit-and-quit
                    "c" (general-key-dispatch 'evil-change
                          "ru" 'string-inflection-upcase
                          "rs" 'string-inflection-underscore
                          "rt" 'string-inflection-camelcase
                          "rc" 'string-inflection-lower-camelcase
                          "rd" 'string-inflection-kebab-case
                          "c" 'magit-commit)
                    "<" (general-key-dispatch 'evil-shift-left
                          "p" 'razzi-surround-paragraph
                          "1" 'razzi-surround-h1
                          "2" 'razzi-surround-h2
                          "3" 'razzi-surround-h3
                          "d" 'razzi-surround-div)
                    "C" 'razzi-change-line
                    "D" 'razzi-kill-line-and-whitespace
                    "Q" 'razzi-replay-q-macro
                    "g]" 'dumb-jump-go
                    "gb" 'magit-blame-addition
                    "gs" 'magit-status
                    "g/" 'razzi-ivy-search-at-point
                    "M-e" 'eval-buffer
                    "M-l" 'evil-visual-line
                    "M-f" 'evil-search-forward
                    "M-`" '(lambda () (interactive)) ; todo something useful
                    "M-[" 'evil-backward-paragraph
                    "M-]" 'evil-forward-paragraph
                    "M-n" 'flycheck-next-error
                    "M-p" 'flycheck-previous-error
                    "M-d" 'iedit-mode
                    "M-w" 'kill-current-buffer
                    "M-s" 'razzi-flycheck-and-save-buffer
                    "M-/" 'evil-commentary-line
                    "M-u" 'razzi-update-current-package
                    "M-r" 'raise-sexp
                    "M-RET" 'eval-defun
                    "o" 'razzi-open-with-comma
                    "<backtab>" 'razzi-previous-useful-buffer)

(general-define-key :states 'insert
                    "C-i" 'hippie-expand
                    "C-SPC" 'yas-expand
                    "C-l" 'sp-forward-slurp-sexp
                    "C-t" 'razzi-transpose-previous-chars
                    "s-<backspace>" 'evil-delete-backward-word
                    "M-/" 'evil-commentary-line
                    "M-l" 'evil-visual-line
                    "M-r" 'razzi-abbrev-or-add-global-abbrev
                    "M-s" 'razzi-exit-insert-and-save
                    "M-v" 'razzi-paste
                    "M-t" 'transpose-words
                    "M-RET" 'eval-defun)

(evil-define-text-object whole-buffer (count &optional beginning end type)
  (evil-range 0 (point-max)))

(general-define-key :states 'operator
                    "E" 'forward-symbol
                    "ae" 'whole-buffer
                    "SPC" 'evil-inner-symbol)

(general-define-key :states 'visual
                    "$" 'razzi-almost-end-of-line
                    "c" 'evil-change
                    "s" 'evil-surround-region
                    "il" 'razzi-mark-line-text
                    "SPC SPC" 'execute-extended-command
                    "'" 'razzi-surround-with-single-quotes
                    ")" 'razzi-surround-with-parens
                    "]" 'razzi-surround-with-brackets
                    "M-l" 'evil-next-line
                    "M-RET" 'eval-region
                    "\"" 'razzi-surround-with-double-quotes)


(general-define-key :modes ivy-mode
                    "C-h" 'ivy-backward-delete-char)

(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-strict-missing-semi-warning nil)
(setq js-indent-level 2)
(setq split-window-preferred-function 'split-window-horizontally)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'focus-out-hook 'garbage-collect)

(use-package typescript-mode
  :config (setq typescript-indent-level 2))

(use-package virtualenvwrapper)

(use-package pipenv)

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

(advice-add 'find-file :before 'razzi-make-parent-directories)

(setq ivy-recursive-restore nil)


;; (ivy-set-actions
;;  'ivy-switch-buffer
;;  '(("f"
;;     ivy--find-file-action
;;     "find file")
;;    ("j"
;;     ivy--switch-buffer-other-window-action
;;     "other window")
;;    ("k"
;;     ivy--kill-buffer-action
;;     "kill")
;;    ("r"
;;     ivy--rename-buffer-action
;;     "rename")))

(defun razzi/open-file-in-split-from-minibuffer (f)
  (other-window 1)
  (split-window-horizontally)
  (find-file f))

(defun razzi/counsel-find-file-other-window ()
  (interactive)
  (let ((current-symbol (ivy-state-current ivy-last)))
    (ivy-exit-with-action #'razzi/open-file-in-split-from-minibuffer)))

(define-key counsel-find-file-map (kbd "M-s") 'razzi/counsel-find-file-other-window)
;; (define-key counsel-find-file-map (kbd "M-a") 'razzi/counsel-find-file-other-window)

(defun counsel-describe-function-or-variable ()
  "Display help about the currently selected ivy result.
Assumes the symbol is a function and tries with a variable describe-function fails."
  (interactive)
  (let ((inhibit-message t)
        (current-symbol (intern (ivy-state-current ivy-last))))
    (condition-case nil
        (describe-function current-symbol)
      ('error
       (describe-variable current-symbol)))))

(define-key counsel-describe-map (kbd "TAB") 'counsel-describe-function-or-variable)


;; from magnars
(defun spacemacs/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (and (configuration-layer/package-usedp 'projectile)
                          (projectile-project-p))
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(use-package better-defaults)

(defun razzi/gray ()
  (interactive)
  (razzi-run-script-on-file "gray"))

(use-package zerodark-theme
  :demand t)

; abbrev populate current word
; prevent prettier from popping up when it finds an error
; 0 to start of text (swap ^)
; uncomment clears out intermediate //
;;;###autoload
;; (defun razzi-open-file (command)
;;   (interactive)
;;   (save-buffer)
;;   (shell-command ("open" (buffer-file-name))))
; cursor here evals NEXT line?!?

(global-hl-line-mode -1)

(use-package which-key
  :config (which-key-mode))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme `tron t)

(setq ivy-extra-directories ())

(setq comment-column 0)
(add-to-list 'load-path "~/forks/emacs-libvterm")
(require 'vterm)
(evil-set-initial-state 'vterm-mode 'insert)
(add-hook 'vterm-mode-hook (lambda ()
                             (yas-minor-mode -1)
                             (linum-mode -1)))

(define-key vterm-mode-map (kbd "<s-right>") '(lambda () (interactive) (term-send-raw-string "\e[1;5C")))
;;
(define-key vterm-mode-map (kbd "<s-left>") '(lambda () (interactive) (term-send-raw-string "\e[1;5D")))
(define-key vterm-mode-map (kbd "C-c") #'vterm-send-ctrl-c)
;; (define-key evil-insert-state-map (kbd "C-SPC") nil)
;; (define-key evil-insert-state-map (kbd "C-d") nil)

(use-package anzu
  :config (anzu-mode))

(use-package evil-anzu)

(use-package ert
  :config
  (define-key ert-results-mode-map (kbd "SPC") nil))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (setq evil-leader/leader "SPC")
  (setq evil-leader/in-all-states t)
  (setq evil-leader/no-prefix-mode-rx '("ert-results-mode"))

  (evil-leader/set-key
    "; n" 'razzi-add-normal-keybinding
    "bb" 'ivy-switch-buffer
    "bn" 'next-buffer
    "bp" 'razzi-previous-useful-buffer
    "bd" 'kill-buffer
    "el" 'flycheck-list-errors
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "ev" 'flycheck-verify-setup
    "hdf" 'describe-function
    "hdv" 'describe-variable
    "d" 'razzi-put-debugger
    "wd" 'delete-window
    "wo" 'other-window
    "ww" 'other-window
    "w -" 'split-window-vertically
    "wh" 'windmove-left
    "wl" 'windmove-right
    "/" 'counsel-rg
    "'" 'vterm
    "," 'razzi-append-comma
    "qr" 'razzi-restart-emacs
    "qq" 'save-buffers-kill-terminal
    "qb" 'razzi-close-all-buffers
    "sl" 'ivy-resume
    "wk" 'evil-window-up
    "w2" 'evil-window-vsplit
    "wj" 'evil-window-down
    "wm" 'delete-other-windows
    "fc" 'write-file
    "fR" 'spacemacs/rename-current-buffer-file
    "fi" '(lambda () (interactive) (find-file user-init-file))
    "fn" 'razzi-copy-file-name
    "fd" 'razzi-copy-file-dir
    "fD" 'crux-delete-buffer-and-file
    "fo" 'crux-open-with
    "fp" 'razzi-copy-project-file-path
    "f RET" 'razzi-copy-project-file-path
    "ff" 'counsel-find-file
    "ft" '(lambda () (interactive) (find-file ()))
    "fr" 'crux-recentf-find-file
    "f SPC" 'razzi-copy-file-name-to-clipboard
    "ig" 'razzi/gray
    "o" 'razzi-put-after
    "tg" 'golden-ratio-mode
    "O" 'razzi-put-before
    "ESC" 'kill-this-buffer
    "SPC" 'execute-extended-command
    "RET" 'razzi-split-after-comma
    "TAB" 'crux-switch-to-previous-buffer))

(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)

; c-a to increment number
; rjsx c-c r replace tab
