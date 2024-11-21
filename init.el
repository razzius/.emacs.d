(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

(tool-bar-mode -1)

(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-dark 'noconfirm)
  (set-face-attribute 'default nil :height 180)

  (defvar razzi-font-list '("DejaVu Sans Mono",
                            "Bitstream Vera Sans Mono"))

  (dolist (font razzi-font-list)
    (when (member font (font-family-list))
      (set-frame-font font))))

(setq inhibit-startup-screen t
      scroll-conservatively 1
      scroll-margin 2
      ring-bell-function 'ignore)

(setq mac-command-modifier 'meta)

(use-package general)

(general-define-key
 :states 'normal
 :prefix "SPC" "qq" 'kill-emacs)

(use-package blackout)

(use-package simple
  :straight nil
  :custom
  (indent-tabs-mode nil)
  :general
  ("M-m" nil
   "C-SPC" nil)

  (:states 'normal
   :prefix "SPC"
   "bd" 'kill-current-buffer
   "bs" 'scratch-buffer)

  (:states 'insert
   "C-h" 'backward-delete-char-untabify))

(use-package vc-hooks
  :straight nil
  :custom
  (vc-follow-symlinks t))

(use-package help
  :straight nil
  :custom
  (help-window-select t)

  :general
  ("C-`" 'describe-key)
  (:keymaps 'help-mode-map
            "<tab>" 'forward-button))

(use-package recentf
  :straight nil
  :general (:states 'normal
            :prefix "SPC"
            "fr" 'recentf))

(use-package help-fns
  :straight nil
  :general
  (:states 'normal :prefix "SPC"
   "hdf" 'describe-function
   "hdk" 'describe-key
   "hdv" 'describe-variable))

(use-package windmove
  :straight nil
  :config
  (defun razzi-do-window-resize (origin horizontal)
    "Resize the current window either towards the ORIGIN or away.

     Goes either HORIZONTAL or vertical."
    (let* ((left-edge (nth 0 (window-edges)))
           (leftmost (eq left-edge 0))
           (sign (if origin 1 -1))
           (offset (if leftmost -5 5))
           (delta (* sign offset)))
      (enlarge-window delta horizontal)))

  (defun razzi-resize-window-left ()
    (interactive)
    (razzi-do-window-resize t t))

  (defun razzi-resize-window-right ()
    (interactive)
    (razzi-do-window-resize nil t))

  :straight nil
  :general
  (:prefix "C-SPC"
   :states 'normal
   "h" 'windmove-left
   "j" 'windmove-down
   "k" 'windmove-up
   "l" 'windmove-right
   "H" 'razzi-resize-window-left
   "L" 'razzi-resize-window-right)
  (:states 'normal :prefix "SPC"
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
  :general
  ("M-q" 'save-buffers-kill-terminal)
  (:states 'normal :prefix "SPC"
   "SPC" 'save-buffer
   "ff" 'find-file
   "fs" 'save-buffer)
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package elisp-mode
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :straight nil
  :general ("M-RET" 'eval-defun))

(use-package menu-bar
  :straight nil
  :general
  (:states 'normal :prefix "SPC"
   "ESC" 'kill-this-buffer))

(use-package evil
  :init
  (setq
   evil-want-keybinding nil
   evil-undo-system 'undo-fu)

  :custom
  (evil-symbol-word-search t)
  (evil-cross-lines t)

  :config
  (evil-mode 1)

  :general
  (:states 'normal
   "0" 'evil-first-non-blank
   "^" 'evil-beginning-of-line))

(use-package undo-fu)

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode))

(use-package evil-commentary
  :config (evil-commentary-mode)
  :blackout)

(use-package evil-surround
  :config (global-evil-surround-mode)
  :general (:states 'visual
            "s" 'evil-surround-region))

(use-package evil-collection
  :config (evil-collection-init))

(use-package restart-emacs
  :general
  (:states 'normal :prefix "SPC"
   "qr" 'restart-emacs))

(defun razzi-vterm-cleanup-on-exit (buf event)
  (when buf (kill-buffer buf))

  ;; Close the split of a closed vterm buffer.
  (when (> (count-windows) 1)
    (delete-window)))

(use-package vterm
  :custom
  (vterm-exit-functions '(razzi-vterm-cleanup-on-exit))

  :config
  (evil-set-initial-state 'vterm-mode 'emacs)

  (defun razzi-vterm-new ()
    (interactive)
    (vterm 'new))

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

  :general
  (:states 'normal
   :prefix "SPC"
   "'" 'vterm)

  (:states 'normal
   :prefix "C-SPC"
   "%" 'razzi-vterm-split-horizontally
   "'" 'razzi-vterm-split-vertically
   "\"" 'razzi-vterm-split-vertically
   "c" 'razzi-vterm-new)

  (:keymaps 'vterm-mode-map
   "C-c" 'vterm--self-insert
   "C-g" 'vterm--self-insert
   "C-h" 'vterm--self-insert
   "C-u" 'vterm--self-insert)

  (:keymaps 'vterm-mode-map
   :prefix "C-SPC"
   "" nil
   "h" 'windmove-left
   "j" 'windmove-down
   "k" 'windmove-up
   "l" 'windmove-right
   "'" 'razzi-vterm-split-vertically
   "\"" 'razzi-vterm-split-vertically
   "%" 'razzi-vterm-split-horizontally
   "c" 'razzi-vterm-new))

(use-package crux
  :general
  (:states 'normal :prefix "SPC"
   "<tab>" 'crux-switch-to-previous-buffer
   "fd" 'crux-delete-file-and-buffer
   "fR" 'crux-rename-file-and-buffer))

(use-package eval-sexp-fu
  :custom
  (eval-sexp-fu-flash-duration .3))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-status-headers-hook '(magit-insert-error-header))

  :config
  (add-hook 'git-commit-mode-hook 'evil-insert-state)

  :general
  (:states 'normal
   "gs" 'magit-status-here))

(use-package projectile)

(use-package ripgrep)

(use-package vimrc-mode)

(use-package razzi
  :straight (:repo "https://git.sr.ht/~razzi/razzi.el")
  :general
  ("M-s" 'razzi-exit-insert-and-save
   "M-<return>" 'razzi-eval-current-sexp
   "<backtab>" 'razzi-previous-useful-buffer)
  (:prefix "M-m"
   "fi" 'razzi-edit-init)
  (:states 'insert
   "C-t" 'razzi-transpose-previous-chars)
  (:states 'normal
   "C" 'razzi-change-line
   "D" 'razzi-kill-line-and-whitespace)
  (:states 'normal
   :prefix "SPC"
   "fi" 'razzi-edit-init
   "f SPC" 'razzi-copy-full-file-name
   "o" 'razzi-put-after)
  (:states 'visual
   "il" 'razzi-mark-line-text
   "$" 'razzi-almost-end-of-line))

(use-package flycheck
  :config
  (global-flycheck-mode)
  :general
  (:states 'normal :prefix "SPC"
   "en" 'flycheck-next-error
   "ep" 'flycheck-previous-error))

(use-package esxml)

(use-package nov
  :straight (:repo "https://depp.brause.cc/nov.el.git"))

(use-package fuco1-redef-lisp-indent
  :straight (:repo "https://git.sr.ht/~razzi/fuco1-redef-lisp-indent.el"))

(use-package markdown-mode)

(use-package vertico
  :config (vertico-mode)
  :custom
  (read-file-name-completion-ignore-case t
   read-buffer-completion-ignore-case t
   completion-ignore-case t))

(use-package vue-mode
  :custom
  (sgml-quick-keys 'close)
  :config
  (add-hook 'mmm-mode-hook
            (lambda ()
              (set-face-background 'mmm-default-submode-face nil))))

(use-package smartparens
  :config
  (require 'smartparens-config)
  :ghook ('prog-mode-hook #'smartparens-strict-mode)
  :general (:states 'insert "C-l" 'sp-slurp-hybrid-sexp))

(use-package whitespace
  :config
  (global-whitespace-mode)
  :custom
  (whitespace-style '(tab-mark)))

(defun osc-52-yank-to-clipboard (&rest _)
  "Use ANSI OSC 52 escape sequence to attempt clipboard copy."
  (send-string-to-terminal
   (format "\033]52;c;%s\a"
           (base64-encode-string
            (encode-coding-string
             (substring-no-properties
              (nth 0 kill-ring)) 'utf-8) t))))

(unless (display-graphic-p)
  (advice-add 'evil-yank :after #'osc-52-yank-to-clipboard))

(use-package server
  :straight nil
  :config (server-start))

(defun razzi-string-startswith (prefix string)
  (let* ((len (length prefix)))))


(let* ((a 3)) a)

(razzi-string-startswith "a" "abc")

(use-package centaur-tabs
  :custom
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-mode t)
  (defun centaur-tabs-buffer-groups ()
    (list
     (cond
      (string-equal))))
  :general
  ("C-<tab>" 'centaur-tabs-forward
   "C-S-<tab>" 'centaur-tabs-backward))

(use-package yasnippet)
