; TODO make init.el a series of load-file / require and split things out nicely
(load-file "~/.emacs.d/lisp/init_use_package.el")

(setq
  ;; (setq mouse-wheel-progressive-speed nil)
  ;; auto-revert-tail-mode? might not scroll past eof
  ;; http://stackoverflow.com/a/4657856/1636613
  ;; scroll-preserve-screen-position t
  abbrev-file-name "~/.emacs.d/abbrev_defs.el"
  backup-directory-alist `((".*" . "~/.emacs.d/backups/"))
  column-number-mode t
  compilation-scroll-output t
  desktop-auto-save-timeout 100
  ;; desktop-base-file-name "emacs.desktop"
  desktop-dirname "~/.emacs.d/desktops/"
  desktop-save t
  dired-recursive-deletes 'always
  ediff-window-setup-function 'ediff-setup-windows-plain
  eshell-banner-message ""
  eshell-rc-script "~/.emacs.d/eshell/profile.el"
  frame-title-format "%f"
  gc-cons-threshold 20000000
  indent-tabs-mode nil
  inhibit-splash-screen t
  inhibit-startup-message t
  initial-scratch-message nil
  isearch-regexp nil
  mouse-wheel-scroll-amount '(1 ((shift) . 1))
  next-line-add-newlines nil
  ns-pop-up-frames nil
  python-python-command "/Users/razzi/.pyenv/shims/python"
  ring-bell-function 'ignore
  save-abbrevs 'silently
  tab-width 2
  tags-add-tables nil ; TODO is this a good default? having multiple merged tables could be cool
  use-package-always-ensure t ; move to init_use_package.el
  vc-follow-symlinks t
  visible-bell nil
  )

(setq-default
  abbrev-mode t
  indent-tabs-mode nil
  tab-width 2
  )

; Global vars
(defvar razzi/pre-visual-kill)
(setq razzi/pre-visual-kill nil)

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(add-to-list 'load-path "lisp")
(tool-bar-mode 0)
(menu-bar-mode -1)
;; (winner-mode 1)
(show-paren-mode)
(set-face-attribute 'default nil :height 182)
(set-face-foreground 'font-lock-comment-face "grey")
(set-face-foreground 'font-lock-doc-face "grey")
(server-start)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
;; (desktop-save-mode 1)
(display-time)
(electric-pair-mode)
(toggle-debug-on-error)

(use-package whitespace
  :config
  (setq
    whitespace-display-mappings
    '(
      (tab-mark ?\t [?\,A;(B ?\t]))
    )
  (setq-default
    whitespace-style '(face space-after-tab tabs tab-mark)
  )
  (global-whitespace-mode)
  )


(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'focus-out-hook 'save-if-file)

(use-package evil-numbers)

(use-package python-environment) ; pyenv

(use-package guide-key
  :config
  (guide-key-mode)
  (setq guide-key/guide-key-sequence t))

(use-package pyenv-mode)

(use-package thingatpt)

; TODO this doesn't work
(use-package ag)

(use-package emmet-mode
  :config
  (add-hook 'nxml-mode-hook (lambda () (emmet-mode)))
  (add-hook 'html-mode-hook (lambda () (emmet-mode)))
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  )

(use-package hippie-exp
  :config
  (setq hippie-expand-try-functions-list '(
      yas-hippie-try-expand
      ;; emmet-expand-line
      )
    )
  )

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package fish-mode)

;; (use-package undo-tree
;;   :config
;;   (global-undo-tree-mode)
;;   (setq
;;     undo-tree-auto-save-history t
;;     )
;;   )

(use-package ido
  :config
  (ido-mode t)
  (setq
    ido-everywhere t
    ido-cannot-complete-command 'ido-next-match)
  )

(defun razzi/magit-pull ()
  (interactive)
  (magit-pull "develop" nil))

(defun razzi/magit-reset-one-commit ()
  (interactive)
  (magit-reset "@^")
  )

(defun razzi/checkout-previous-branch ()
  (interactive)
  (magit-checkout "-")
  )

(use-package magit
  :config
  (add-to-list 'same-window-regexps "\*magit: .*\*")
  ;; (setq magit-display-buffer-function 'switch-to-buffer)
  (define-key magit-status-mode-map (kbd "]") 'razzi/magit-pull)
  (define-key magit-status-mode-map (kbd "-") 'razzi/checkout-previous-branch)
  (define-key magit-status-mode-map (kbd "_") 'magit-diff-less-context)
  (define-key magit-status-mode-map (kbd "C-`") 'describe-key)
  (define-key magit-status-mode-map (kbd "@") 'razzi/magit-reset-one-commit))

(use-package git-gutter
  :config
  (setq
    git-gutter:update-interval .4
    git-gutter:hide-gutter t
    )
  (git-gutter:linum-setup)
  (global-git-gutter-mode 1)
  (global-linum-mode)
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  )

(use-package anzu
  :config
  (global-anzu-mode 1)
  )

(use-package fzf)

(use-package helm-flx
  :config
  (helm-flx-mode 1)
  )

(use-package helm
  :defines
  helm-mode-fuzzy-match
  helm-M-x-fuzzy-match
  ;; helm-buffers-fuzzy-match
  helm-find-file-fuzzy-match
  helm-recentf-fuzzy-match
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq
    helm-mode-fuzzy-match t
    helm-M-x-fuzzy-match t
    ;; helm-autoresize-max-height 10
    ;; helm-buffers-fuzzy-match t
    helm-recentf-fuzzy-match t
    helm-find-file-fuzzy-match t
    ;; helm-split-window-in-side-p t
    ;; helm-split-window-default-side 'below
    ;; helm-always-two-windows nil
    )
  (define-key helm-map (kbd "C-v") 'find-file-other-window)
  )

(use-package helm-ag)

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  )

(use-package recentf
  :config
  (recentf-mode 1)
  )

;; (use-package flycheck-haskell)

(use-package flycheck
  :init
  (setq
    flycheck-display-errors-delay .4
    flycheck-highlighting-mode 'lines
    flycheck-disabled-checkers '(emacs-lisp-checkdoc)
    )
  :config
  (global-flycheck-mode nil)
  )

(use-package flycheck-mypy
  :config
  (add-hook 'python-mode-hook 'flycheck-mode))
; todo....
;; (use-package ido-ubiquitous
;;   :config
;;   (ido-ubiquitous-mode 1)
;;   )

(require 'company-simple-complete "~/.emacs.d/company-complete-cycle.el")

(defun my-company-hook ()
  (interactive)
  ;; (define-key company-active-map [return] 'nil)
  ;; (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  ; TODO turn off c-h
  )

;; (use-package company
;;   :config
;;   (global-company-mode)
;;   ;; (add-hook 'company-mode-hook 'my-company-hook)
;;   (setq
;;     company-minimum-prefix-length 2
;;     ;; company-backends '(company-ycmd)
;;     )
;;   )

;; (use-package company-flx
;;   :config
;;   (company-flx-mode 1))

;; (use-package ycmd
;;   :config
;;   (add-hook 'python-mode-hook
;;     (lambda ()
;;       (add-to-list 'flycheck-disabled-checkers 'ycmd)))
;;   (global-ycmd-mode)
;;   )

;; (use-package company-ycmd
;;   :config
;;   (company-ycmd-setup)
;;   (setq
;;     ycmd-server-command '("/Users/razzi/.pyenv/shims/python3" "/Users/razzi/forks/YouCompleteMe/third_party/ycmd/ycmd/__main__.py")
;;    ))

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  )

(use-package avy)

;; (use-package mmm-mode
;;    :config
;;    (setq mmm-global-mode 'maybe)
;;    (mmm-add-classes
;;      '((markdown-python
;;          :submode python-mode
;;          :front "^```python\n"
;;          :back "^```$")))
;;    )
;;   (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)

; sets the python interpreter to python2
;; (use-package python-mode)

(use-package restart-emacs)

; TODO mine for ideas
(use-package elpy
  :disabled t
  :config
  (elpy-enable)
  )

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*")
  )

(defun edit-notes ()
  (interactive)
  (find-file "~/notes.org")
  )

(defun edit-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )

(defun razzi/copy-paragraph ()
  (interactive)
  (move-beginning-of-line nil)
  (let ((sentence (thing-at-point 'defun)))
    (insert sentence)
    (insert "\n")
    ; TODO in python make this copy a method _or_ class!
    )
  )

(defun razzi/kill-buffer-and-window ()
  (interactive)
  (kill-this-buffer)
  (delete-window)
  )

(defun razzi/yank-file-name ()
  (interactive)
  (kill-new buffer-file-name)
  (message (format "Copied %s" buffer-file-name)))

(defun razzi/edit-eshell-profile ()
  (interactive)
  (find-file "~/.emacs.d/eshell/profile.el")
  )

(defun razzi/append-comma ()
  (interactive)
  (evil-append 0 0 nil)
  (move-end-of-line nil)
  (insert ",")
  (evil-normal-state)
  )

(defun razzi/magit-checkout-file ()
  (interactive)
  (magit-checkout-file "HEAD" buffer-file-name)
  ;; (git-gutter:update-all-windows)
  )

(defun razzi/blame ()
  (interactive)
  ; todo
  ;; (evil-scroll-line-to-top nil)
  (magit-blame "HEAD" buffer-file-name))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  (setq
    projectile-switch-project-action 'projectile-find-file))

;; (add-to-list 'load-path "~/.emacs.d/pytest-el")
;; (require 'pytest)

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "," 'razzi/append-comma
    ;; "-" 'razzi/run-test-pytest ; todo move to python mode
    "=" 'razzi/run-pytest ; todo move to python mode
    "8" 'razzi/autopep8 ; python mode
    ";" 'previous-buffer
    ;; "A" 'add-global-abbrev ; TODO ag current word
    "C" 'razzi/magit-checkout-file
    "E" 'eval-buffer
    "O" 'razzi/put-before
    "X" 'delete-file-and-buffer
    "DEL" 'restart-emacs
    "SPC" 'save-buffer
    ;; "a" 'add-global-abbrev ; TODO search
    "b" 'razzi/blame
    "c" 'razzi/copy-paragraph
    "d" 'magit-diff-unstaged
    "e" 'eshell
    "f" 'razzi/yank-file-name
    "g" 'magit-status
    "h" 'help-command
    "i" 'edit-init
    "j" 'avy-goto-char
    ;; "k" 'edit-private-xml
    "m" 'razzi/show-messages
    "n" 'edit-notes
    "o" 'razzi/put-after
    ; "p" 'razzi/edit-eshell-profile
    "p" 'razzi/importmagic
    ;; "q" 'razzi/kill-buffer-and-window
    "q" 'kill-this-buffer
    "r" 'helm-recentf
    "s" 'switch-to-scratch
    "t" 'helm-projectile-find-file
    "v" 'eval-last-sexp ; TODO move to lisp mode
    "x" 'compile
    ; TODO recompile
    "z" 'razzi/eshell-in-split
    ;; "b" 'eval-buffer
    )
  )

(use-package pt)

(defun razzi/eshell-in-split ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (eshell)
  )

(defun razzi/transpose-prev-chars ()
  (interactive)
  (backward-char 1)
  (transpose-chars nil)
  )

(defun razzi/importmagic ()
  (interactive)
  (shell-command
    (format "python ~/code/python_scripts/import_magic.py -i %s" buffer-file-name))
  )

(defun razzi/run-pytest ()
  (interactive)
  (compile "py.test")
  )

(defun razzi/autopep8 ()
  (interactive)
  (shell-command
    (format "autopep8 -i --max-line-length 100 %s" buffer-file-name))
  (revert-buffer nil t)
  )

(defun razzi/clear ()
  (interactive)
  (if magit-blame-mode
    (magit-blame-quit)
    (delete-other-windows)
    )
  )

(defun razzi/simple-newline ()
  "Insert a newline and indent"
  ; TODO indenting in a comment adds too much indentation
  (interactive)
  (electric-indent-just-newline nil)
  (indent-according-to-mode))

(defun razzi/paredit-change ()
  "Make vim C use paredit-kill"
  (interactive)
  (paredit-kill)
  (evil-insert 0))

(defun razzi/replay-q-macro ()
  (interactive)
  ; TODO could run @q directly rather than executing those chars as a command
  (evil-execute-macro 1 "@q"))


(defun razzi/replay-q-macro-new ()
  (interactive)
  ; TODO run q directly rather than executing those chars as a command
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Call.html
  (call-interactively 'evil-execute-macro nil [\q]))

(defun razzi/kill-line-and-whitespace ()
  (interactive)
  (paredit-kill)
  (delete-trailing-whitespace))

(defun razzi/mark-line-text ()
  (interactive)
  (move-end-of-line nil)
  (set-mark-command nil)
  (back-to-indentation)
  )

(use-package paredit)

(defun razzi/transpose-next-line ()
  "Switch the current and next lines"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun razzi/transpose-previous-line ()
  "Switch the current and previous lines"
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun razzi/wrap-in-parens ()
  "set|var -> set(var)"
  (interactive)
  (insert "(")
  (forward-symbol 1)
  (insert ")")
  )

(defun razzi/paste-replace (start end)
  "replace visual selection with last yank"
   ; TODO if clipboard has changed to not selection then use that instead
  (interactive "r")
  (goto-char end)
  (message razzi/pre-visual-kill)
  (if razzi/pre-visual-kill
      (insert razzi/pre-visual-kill)
      (evil-paste-after 1)
      ;; (insert "uyo")
      ;; (insert razzi/pre-visual-kill)
      ;; (evil-paste-after 1)
      )
  (kill-region start end))

(defun razzi/save-kill-visual ()
  "Before entering visual mode, save the last kill"
  (interactive)
  (setq razzi/pre-visual-kill (ns-get-pasteboard))
  ;; (message razzi/pre-visual-kill)
  (evil-visual-char)
  )

(defun razzi/erase-kill-visual ()
  "Before exiting visual mode, erase the last kill"
  (interactive)
  (setq razzi/pre-visual-kill nil)
  (evil-visual-char)
  )


(defun razzi/surround-with-single-quotes (start end)
  (interactive "r")
  (evil-surround-region start end nil ?'))


(defun razzi/surround-with-double-quotes (start end)
  (interactive "r")
  (evil-surround-region start end nil ?\"))


(use-package evil
  :config
  (evil-mode 1)

  (setq
    evil-regexp-search nil
    evil-cross-lines t
    evil-ex-substitute-global t
    evil-shift-width 2
    )

  (define-key evil-insert-state-map (kbd "C-`") 'describe-key)
  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-c a") 'inverse-add-global-abbrev)
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-j") 'razzi/simple-newline)
  (define-key evil-insert-state-map (kbd "C-k") 'paredit-kill)
  (define-key evil-insert-state-map (kbd "C-l") 'paredit-forward-slurp-sexp)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-complete-previous)
  (define-key evil-insert-state-map (kbd "C-t") 'razzi/transpose-prev-chars)
  (define-key evil-insert-state-map (kbd "C- ;") 'paredit-semicolon) ; TODO emacs only
  ;; (define-key evil-insert-state-map (kbd "<tab>") 'company-complete-selection)
  (define-key evil-insert-state-map (kbd "<C-return>") 'razzi/wrap-in-parens) ; consider rebinding to c-(

  (define-key evil-normal-state-map (kbd "#") 'razzi/pound-isearch)
  (define-key evil-normal-state-map (kbd "*") 'razzi/star-isearch)
  (define-key evil-normal-state-map (kbd "-") 'razzi/transpose-next-line)
  (define-key evil-normal-state-map (kbd "<C-i>") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "<tab>") 'next-buffer)
  (define-key evil-normal-state-map (kbd "<backtab>") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "=") 'razzi/run-pytest)
  (define-key evil-normal-state-map (kbd "C") 'razzi/paredit-change)
  (define-key evil-normal-state-map (kbd "C") 'razzi/paredit-change)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "D") 'razzi/kill-line-and-whitespace)
  (define-key evil-normal-state-map (kbd "M-RET") 'delete-window)
  (define-key evil-normal-state-map (kbd "M-[") 'my-toggle-frame-left)
  (define-key evil-normal-state-map (kbd "M-]") 'my-toggle-frame-right)
  (define-key evil-normal-state-map (kbd "M-a") 'mark-whole-buffer) ; TODO make this keep point where it is
  (define-key evil-normal-state-map (kbd "M-p") 'scroll-other-window)
  (define-key evil-normal-state-map (kbd "M-q") 'save-buffers-kill-terminal)
  (define-key evil-normal-state-map (kbd "Q") 'razzi/replay-q-macro)
  (define-key evil-normal-state-map (kbd "Q") 'razzi/replay-q-macro)
  (define-key evil-normal-state-map (kbd "RET") 'razzi/clear)
  (define-key evil-normal-state-map (kbd "[ SPC") 'razzi/insert-newline-before)
  (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  (define-key evil-normal-state-map (kbd "] SPC") 'razzi/insert-newline-after)
  (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "_") 'razzi/transpose-previous-line)
  (define-key evil-normal-state-map (kbd "g'") 'goto-last-change)
  (define-key evil-normal-state-map (kbd "g;") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd "gT") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)
  (define-key evil-normal-state-map (kbd "gf") 'razzi/file-at-point)
  (define-key evil-normal-state-map (kbd "gs") 'magit-status)
  (define-key evil-normal-state-map (kbd "gt") 'next-buffer) ; TODO file buffers only
  (define-key evil-normal-state-map (kbd "go") 'evil-open-above)
  (define-key evil-normal-state-map (kbd "v") 'razzi/save-kill-visual)
  ;; (define-key evil-normal-state-map (kbd "<backtab>") 'elscreen-previous)
  ; todo
  ;; (define-key evil-normal-state-map (kbd "C-]") 'razzi/tag-in-split)
  ;; (define-key evil-normal-state-map (kbd "j") 'evil-next-line-first-non-blank) ; TODO only if in whitespace already
  ;; (define-key evil-normal-state-map (kbd "k") 'evil-previous-line-first-non-blank)

  (define-key evil-visual-state-map (kbd "!") 'sort-lines)
  (define-key evil-visual-state-map (kbd "ae") 'mark-whole-buffer)
  (define-key evil-visual-state-map (kbd "il") 'razzi/mark-line-text)
  (define-key evil-visual-state-map (kbd "V") 'evil-a-paragraph)
  (define-key evil-visual-state-map (kbd "p") 'razzi/paste-replace)
  (define-key evil-visual-state-map (kbd "s") 'evil-surround-region)
  ;; (define-key evil-visual-state-map (kbd "v") 'razzi/erase-kill-visual)
  (define-key evil-visual-state-map (kbd "*") 'razzi/star-isearch)
  (define-key evil-visual-state-map (kbd "#") 'razzi/pound-isearch)
  (define-key evil-visual-state-map (kbd "'") 'razzi/surround-with-single-quotes)
  (define-key evil-visual-state-map (kbd "\"") 'razzi/surround-with-double-quotes)

  (define-key evil-operator-state-map (kbd "V") 'evil-a-paragraph)

  (add-hook 'evil-insert-state-exit-hook 'save-if-file)
  )

; TODO use to refactor star-isearch
;; (use-package evil-visualstar
;;   :config
;;   (global-evil-visualstar-mode t))

(use-package evil-nerd-commenter)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
  )

(use-package projectile
  :config
  (projectile-global-mode)
  (setq
    projectile-enable-caching t
    projectile-completion-system 'helm
    projectile-switch-project-action #'projectile-commander
    )
  )

(use-package change-inner)

(defun change-inner-parens ()
  (interactive)
  (let ((text (buffer-substring (point) (line-end-position))))
    (if (s-contains? "(" text)
      (change-inner* nil "(")
      (progn
        (search-backward "(" (line-beginning-position) nil 1)
        (change-inner* nil "("))
      ))
  )

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "dp" 'change-inner-parens)
  (setq key-chord-two-keys-delay 0.3)
  )

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

(defun razzi/eshell-abbrev-and-return ()
  (interactive)
  (expand-abbrev)
  (eshell-send-input))

(use-package virtualenvwrapper
  :config
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs")
  )

(use-package s)

(defun razzi/eshell-point-to-prompt ()
  (interactive)
  (goto-char (point-max))
  (evil-append 0 0 nil)
  )

(add-hook 'html-mode-hook (lambda ()
    (modify-syntax-entry ?_ "w")
    (setq
   ; todo?
    ;; nxml-child-indent 2
    ;; nxml-attribute-indent 2
      tab-width 2
      )
    )
  )

;; (add-hook 'js-mode-hook (lambda ()
;;     (modify-syntax-entry ?_ "w")
;;     (setq
;;       js-indent-level 2
;;       evil-shift-width 2
;;       )
;;     )
;;   )
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (setq
      js-indent-level 2
      evil-shift-width 2
      )
  )

(add-hook 'nxml-mode-hook (lambda ()
    (setq
      nxml-child-indent 2
      nxml-attribute-indent 2
      tab-width 2
      )
    )
  )

(use-package company-jedi)

(defun razzi/python-mode ()
  (interactive)
  ;; (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  ;; (add-to-list 'company-backends 'company-jedi)
  (evil-define-key 'insert python-mode-map
    (kbd "#") 'razzi/python-pound-and-space
    (kbd ";") 'razzi/python-pound-and-space)
  (evil-define-key 'insert python-mode-map (kbd "C-h") 'py-electric-backspace))

(add-hook 'python-mode-hook 'razzi/python-mode)

(add-hook 'ediff-startup-hook (lambda ()
  (local-set-key (kbd "q") 'my-ediff-quit)))

; TODO doesn't work
;; http://emacs.stackexchange.com/questions/9322/how-can-i-quit-ediff-immediately-without-having-to-type-y
(defun my-ediff-quit ()
  "If any of the ediff buffers have been modified, ask if changes
should be saved. Then quit ediff normally, without asking for
confirmation"
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let* ((buf-a ediff-buffer-A)
         (buf-b ediff-buffer-B)
         (buf-c ediff-buffer-C)
         (ctl-buf (current-buffer))
         (modified (remove-if-not 'buffer-modified-p
                                  (list buf-a buf-b buf-c))))
    (let ((save (if modified (yes-or-no-p "Save changes?")nil)))
      (loop for buf in modified do
            (progn
              (set-buffer buf)
              (if save
                  (save-buffer)
                (set-buffer-modified-p nil))))
      (set-buffer ctl-buf)
      (ediff-really-quit nil))))

; TODO move eshell stuff to own file
(defun fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

;; (defun eshell-rename-buffer-before-command ()
;;   (let* ((last-input (buffer-substring eshell-last-input-start eshell-last-input-end)))
;;     (rename-buffer (format "*eshell[%s]$ %s...*" default-directory last-input) t)))

;; (defun eshell-rename-buffer-after-command ()
;;   (rename-buffer (format "*eshell[%s]$ %s*" default-directory (eshell-previous-input-string 0)) t))

;; (add-hook 'eshell-pre-command-hook  'eshell-rename-buffer-before-command)
;; (add-hook 'eshell-post-command-hook 'eshell-rename-buffer-after-command)

(defun razzi/eshell-eof-or-delete (&optional use-region)
  (interactive)
  (if (eobp)
    (eshell-send-eof-to-process)
    (paredit-forward-delete)))

(defun razzi/eshell-up-window-or-kill-line ()
  (interactive)
  (if (eobp)
    (windmove-up)
    (paredit-kill)))

(add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)

(evil-define-key 'normal eshell-mode-map
  (kbd "A") 'razzi/eshell-point-to-prompt
)

(evil-define-key 'insert eshell-mode-map
  (kbd "C-a") 'eshell-bol
  (kbd "C-c") 'eshell-interrupt-process
  (kbd "C-d") 'razzi/eshell-eof-or-delete
  (kbd "C-e") 'end-of-line
  (kbd "C-j") 'razzi/eshell-abbrev-and-return
  (kbd "C-k") 'razzi/eshell-up-window-or-kill-line
  (kbd "C-n") 'eshell-next-input
  (kbd "C-p") 'eshell-previous-matching-input-from-input
  (kbd "C-u") 'eshell-kill-input
  (kbd "<tab>") 'eshell-pcomplete
)

  ; TODO npm is weird with eshell :O
  ;; (add-to-list 'eshell-output-filter-functions '(lambda ()
  ;;   (save-excursion
  ;;     (replace-regexp "\[[\?0-9]+[hlAGKJ]" "" nil eshell-last-output-start eshell-last-output-end)
  ;;     )
  ;;   )
  ;; )

; TODO make this ls without showing the prompt and ls
(add-hook 'eshell-post-command-hook
  (lambda ()
    (when (s-starts-with? "cd" (eshell-previous-input-string 0))
    (with-current-buffer "*eshell*"
      (eshell-return-to-prompt)
      (insert "ls")
      (eshell-send-input)))))

  ;; (setq
  ;;   eshell-prompt-function (lambda ()
  ;;     (concat
  ;;     (if venv-current-name
  ;;       (format "(%s) " venv-current-name)
  ;;       ""
  ;;       )
  ;;       (fish-path (eshell/pwd) 30) " $ "))
  ;;   eshell-prompt-regexp ".*\$ $"
  ;;   )
  ;; )

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(defun save-if-file ()
  (if (buffer-file-name)
    (save-buffer)
  )
)

; TODO make this not path dependent
(require 'razzi/interactive "~/.emacs.d/lisp/interactive.el")

(defun razzi/put-after ()
  (interactive)
  (evil-with-single-undo
    (evil-insert-newline-below)
    (indent-for-tab-command)
    (insert (s-trim (current-kill 0)))
    (forward-line)
  )
)

(defun razzi/file-at-point ()
  (interactive)
  (find-file-at-point (thing-at-point 'symbol)))

(defun razzi/put-before ()
  (interactive)
  (evil-with-single-undo
    (evil-insert-newline-above)
    (indent-for-tab-command)
    (insert (s-trim (current-kill 0)))
    (forward-line)
  )
)

;; asdf/a
;; asdf/hi
;; asdf/a
;; sadf
;; asdf/h

; TODO refactor
(defun razzi/star-isearch ()
  (interactive)
  (let ((inhibit-redisplay 1)
        (selection (evil-visual-state-p))
        (visual-type (evil-visual-type))
        (text (if (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    ; Go to the start of the word if not in visual and not already at the start
    (when (and (not selection)
               (not (looking-at "\\<")))
      (backward-word))
    (evil-exit-visual-state)
    (isearch-mode t)
    (isearch-yank-string text)
    (isearch-done)
    (evil-search-next)
    (when (and
            selection
            (not (eq visual-type 'line)))
      (evil-search-previous))))

(defun razzi/pound-isearch ()
   ; TODO it doesn't work in the middle of a word
  (interactive)
  (let ((inhibit-redisplay 1)
        (selection (evil-visual-state-p))
        (visual-type (evil-visual-type))
        (text (if (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    (when (and (not selection)
               (not (looking-at "\\<")))
      (backward-word))
    (evil-exit-visual-state)
    (isearch-mode nil)
    (isearch-yank-string text)
    (isearch-done)
    (evil-search-next)
    (when (and
            selection
            (not (eq visual-type 'line)))
      (evil-search-previous))))

; TODO paredit-semicolon
(defun razzi/elisp-semicolon-and-space ()
  (interactive)
  (if (and (not (bolp)) (eolp))
    (insert " ; ")
    (insert "; ")))

(defun razzi/python-pound-and-space ()
  (interactive)
  (if (and (not (bolp)) (eolp))
    (insert "  # ")
    (insert "# ")))

(add-hook 'emacs-lisp-mode-hook (lambda ()
    (enable-paredit-mode)

    (setq
      evil-shift-width 2
      tab-width 2
      indent-tabs-mode nil)

    (evil-define-key 'insert emacs-lisp-mode-map
      (kbd ";") 'razzi/elisp-semicolon-and-space)
  ))

(define-key input-decode-map "\C-i" [C-i])

(define-key isearch-mode-map (kbd "C-j") 'isearch-done)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-`") 'describe-key)
(define-key dired-mode-map (kbd "C-j") 'dired-find-file)

(global-set-key (kbd "M-v") 'nil)
(global-set-key (kbd "M-v") 'evil-paste-after)
(define-key minibuffer-local-isearch-map (kbd "M-v") 'nil)
(define-key minibuffer-local-isearch-map (kbd "M-v") 'isearch-yank-kill)

(defun minibuffer-config ()
  (interactive)
  (local-set-key (kbd "C-j") 'exit-minibuffer)
  ; todo
  ;; (local-set-key (kbd "M-v") 'isearch-yank-pop)
  )

(add-hook 'minibuffer-setup-hook 'minibuffer-config)

; TODO no confirm
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

; (this is a django mode for emacs)
;; (use-package pony-mode)

; todo
; smarter VV when line has opening paren
; VV ?
; if the line ends with (starts with?) ( {, jump to it's pair
; otherwise select paragraph
; insert mode c-l non-lisp modes
; wip elisp move stuff into own files

; eshell
; highlight valid commands

; set|var -> set(var)
; search c-t transpose chars
; :tag command

; search c-w delete word, not paste...
; visual block i to block insert
; persistent marks
; show marks in gutter
; c-j xml put cursor in between tags
; in docstring, auto indent after first

; rename current file
;persistent undo
; case insensitive completion eshell
; eshell in split
; ag bindings

; python tab to outdent a level?
; python newline when already outdented preserve outdent
; somehow correct things like pyton_source - perhaps using syntax for word versus symbol
; *** razzi/extract-as-variable
; reassign variables when I make the same call
; with open(fn) as f
;   x = f.read()
;   y = json.parse(f.read())
;                   ^ this turns into x

; make spc ; toggle between 2 buffers
; tab between only file buffers
;; compile window not overwrite
;; compile window c-l move window
; compile remove line that says mode compile
; no scroll past end of buffer
; projectile c-w kill word

; """ autoclose with formatted docstring!
; (see http://stackoverflow.com/questions/19676181/electric-pair-mode-and-python-triple-quotes?rq=1)
; newline after (setq should have a 2 space indent

; ** do the right close bracket outdent
        ;; return {
        ;;     issue['number'] for issue in content}

; why is this happening?
;; PIPELINE_IDS = {
;;     'backlog': '54c19ad8f748cd180f07b4b6',
;;     'in_development': '54c19ad8f748cd180f07b4b4',
;;     'new_issues': '54c19ad8f748cd180f07b4b7',
;;     'pr_outstanding': '55c2c7964e6d61ea173a1ce8'
;; }
;;     MILESTONE_IDS = { <- indent madness
;;         }
; o to indent new line if in class scope - if 2 lines open on toplevel (hard?)
; treat _ as a word separator for abbrev / spelling
; ending docstring indent correctly (autopep8?)

; xml auto close tag
; c-x c-f autocomplete file
; eshell smarter tab completion
; python: ]] isn't going to next class

; helm c-w delete word (clear?)
; after magit, update git gutter

;python if else indenting

; yp yank inside parens
; S to kill within quotes for example (maybe)
; use helm for find-tag
; recentf sort
; smerge mode bindings: next, rebind return, keep both
; rebind c-h to backspace in evil-ex
; simpler defun yasnippet
; ~ move to first char that can have its case switched? rather j / k move to nonblan
; magit commit autopopulate with ref, and go straight into insert mode
; magit Z stash no message
; magit some way to pop most recent stash
; bind substitute - looks like
; (define-key evil-normal-state-map (kbd "g / r") (lambda () (evil-ex "%s/")))
; magit k magit-discard-item no confirm
; star-isearch on whitespace should jump to last symbol
; visual ) surround in parens
; Y y$
; * and # show numbers
; lisp emacs ; no space before if only whitespace prior
; [|ret] throw the close bracket on the correct line
; spc q close split
; cs[ on a line before [ doesn't work
; no debug on error in eshell
; if I already have an abbrev, make c-c a just insert it
; consider turning off paredit-backslash
