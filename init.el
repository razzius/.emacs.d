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
  gc-cons-threshold 100000000
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
  tags-add-tables nil ; TODO is this a good default? having multiple merged tables could be cool
  vc-follow-symlinks t
  visible-bell nil)

(setq-default
  abbrev-mode t
  indent-tabs-mode nil
  tab-width 2)

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
    whitespace-style '(face space-after-tab tabs tab-mark))
  (global-whitespace-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'focus-out-hook 'garbage-collect)

(use-package diminish)

(use-package focus-autosave-mode
  :diminish focus-autosave-mode
  :config
  (focus-autosave-mode))

(use-package evil-numbers)

(use-package python-environment) ; pyenv

(use-package guide-key
  :config
  (guide-key-mode)
  (setq guide-key/guide-key-sequence t))

(use-package pyenv-mode)

(use-package thingatpt)

(use-package emmet-mode
  :config
  (add-hook 'nxml-mode-hook (lambda () (emmet-mode)))
  (add-hook 'html-mode-hook (lambda () (emmet-mode)))
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  )

(use-package hippie-exp
  :config
  (setq hippie-expand-try-functions-list '(
      try-expand-line
      try-expand-line-all-buffers
      ;; yas-hippie-try-expand
      ;; emmet-expand-line

      )
    )

  (defadvice hippie-expand-substitute-string (after he-paredit-fix)
    "Remove extra paren when expanding line in paredit"
    (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char)))))

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
  (magit-reset "@^"))

(defun razzi/checkout-previous-branch ()
  (interactive)
  (magit-checkout "-"))

(defun razzi/magit-stash ()
  "Stash with no prompt"
  (interactive)
  (magit-stash-save "WIP" t t nil t))

(use-package magit
  :config
  (setq same-window-regexps (append same-window-regexps '("\*magit: .*\*" "\*magit-diff: .*\*")))
  (define-key magit-status-mode-map (kbd "=") 'magit-diff-more-context)
  (define-key magit-status-mode-map (kbd "C-`") 'describe-key)
  (define-key magit-status-mode-map (kbd "@") 'razzi/magit-reset-one-commit)
  (define-key magit-status-mode-map (kbd "Z") 'razzi/magit-stash))

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
  (global-anzu-mode 1))

(use-package evil-anzu)

(use-package helm-flx
  :config
  (helm-flx-mode 1))

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
    flycheck-temp-prefix "/tmp/flycheck")
  :config
  (global-flycheck-mode nil))

(use-package flycheck-mypy
  :config
  (add-hook 'python-mode-hook 'flycheck-mode))

(require 'company-simple-complete "~/.emacs.d/company-complete-cycle.el")

(use-package company
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "C-w") 'nil)
  (define-key company-active-map (kbd "C-h") 'nil)
  ;; (define-key company-active-map [return] 'nil)
  ;; (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (setq
    company-minimum-prefix-length 2
    ;; company-backends '(company-ycmd)
    )
  )

(use-package company-flx
  :config
  (company-flx-mode 1))

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
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

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
  (switch-to-buffer "*scratch*"))

(defun edit-notes ()
  (interactive)
  (find-file "~/notes.org"))

(defun edit-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

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
  (when (> (length (window-list)) 1)
    (delete-window)))

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
  (evil-normal-state))

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
  (setq projectile-switch-project-action 'projectile-find-file))

(use-package helm-git-grep)

;; (add-to-list 'load-path "~/.emacs.d/pytest-el")
;; (require 'pytest)

(use-package paredit)

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "," 'razzi/append-comma
    ";" 'razzi/toggle-between-buffers
    "<left>" 'previous-buffer
    "<right>" 'previous-buffer
    "C" 'razzi/magit-checkout-file
    "DEL" 'restart-emacs
    "E" 'eval-buffer
    "G" 'helm-git-grep-at-point
    "R" 'razzi/rename-current-file
    "O" 'razzi/put-before
    "SPC" 'save-buffer
    "X" 'delete-file-and-buffer
    "]" 'find-tag
    "b" 'razzi/blame
    "c" 'razzi/copy-paragraph
    "d" 'magit-diff-unstaged
    "e" 'eshell
    "f" 'razzi/yank-file-name
    "g" 'helm-git-grep
    "h" 'help-command
    "i" 'edit-init
    "j" 'avy-goto-char
    "m" 'razzi/show-messages
    "n" 'edit-notes
    "o" 'razzi/put-after
    "q" 'razzi/kill-buffer-and-window
    "r" 'helm-recentf
    "s" 'switch-to-scratch
    "t" 'helm-projectile-find-file
    "v" 'eval-last-sexp ; TODO move to lisp mode
    "x" 'compile
    "z" 'razzi/eshell-in-split
    ; "p" 'razzi/edit-eshell-profile
    ; TODO recompile
    ;; "k" 'edit-private-xml
    )
  )

(use-package pt)

(defun razzi/eshell-in-split ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (eshell))

(defun razzi/rename-current-file (name)
  (interactive "MRename to:")
  (rename-file buffer-file-name name)
  (kill-this-buffer)
  (find-file name))

(defun razzi/transpose-prev-chars ()
  (interactive)
  (backward-char 1)
  (transpose-chars nil))

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
    (delete-other-windows)))

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
  (back-to-indentation))

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
  (move-end-of-line nil)
  (insert ")"))

(defun razzi/paste-replace (start end)
  "replace visual selection with last yank"
   ; TODO if clipboard has changed to not selection then use that instead
  (interactive "r")
  (goto-char end)
  (message razzi/pre-visual-kill)
  (if razzi/pre-visual-kill
      (insert razzi/pre-visual-kill)
      (evil-paste-after 1))
  (kill-region start end))

(defun razzi/save-kill-visual ()
  "Before entering visual mode, save the last kill"
  (interactive)
  (setq razzi/pre-visual-kill (shell-command-to-string "pbpaste"))
  (evil-visual-char))

(defun razzi/erase-kill-visual ()
  "Before exiting visual mode, erase the last kill"
  (interactive)
  (setq razzi/pre-visual-kill nil)
  (evil-exit-visual-state))

(defun razzi/surround-with-single-quotes (start end)
  (interactive "r")
  (evil-surround-region start end nil ?'))

(defun razzi/surround-with-double-quotes (start end)
  (interactive "r")
  (evil-surround-region start end nil ?\"))

(defun razzi/surround-with-parens (start end)
  (interactive "r")
  (evil-surround-region start end nil ?\))
  (goto-char (+ 1 end)))

(defun razzi/surround-with-brackets (start end)
  (interactive "r")
  (evil-surround-region start end nil ?\])
  (goto-char (+ 1 end)))

(defun razzi/increase-region (start end)
  (interactive "r")
  ;; TODO use (evil-jump-item)
  (let* ((matching-chars (list ?\( ?\{ ?\[ ?\) ?\} ?\]))
        (opening-chars (list ?\( ?\{ ?\[))
        (closing-chars (list ?\) ?\} ?\]))
        (visual-type (evil-visual-type))
        (char (char-before (point)))
        (line (if (eq visual-type 'line)
                  (progn
                    (forward-line -1)
                    (thing-at-point 'line)
                    )
                (thing-at-point 'line)))
        (last-line-char (car (last (string-to-list line) 2)))
        (first-line-char (string-to-char (s-trim line))))
    (cond
      ((member char opening-chars) (progn (evil-backward-char) (evil-jump-item) (evil-forward-char)))
      ((member char closing-chars) (evil-jump-item))
      ((member last-line-char closing-chars) (progn (move-end-of-line 1) (set-mark-command nil) (evil-execute-macro 1 "%")))
      ((member first-line-char opening-chars) (evil-execute-macro 1 "^%"))
      ((member last-line-char opening-chars) (progn (move-end-of-line 1) (evil-execute-macro 1 "%")))
      (t (forward-paragraph)))
    ))

(defun razzi/copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun razzi/almost-end-of-line ()
  (interactive)
  (move-end-of-line 1)
  (backward-char)
  (forward-char))

(defun razzi/abbrev-or-add-global-abbrev ()
  (interactive)
  (if (abbrev-expansion (thing-at-point 'word))
    (expand-abbrev)
    (inverse-add-global-abbrev 1)))

(defun razzi/magit-stash-wip ()
  (interactive)
  (magit-stash "WIP"))

(defun razzi/paredit-change-line ()
  (interactive)
  (while (and (>= (current-column) (current-indentation))
              (not (looking-at "\s(")))
    (backward-char))
  (forward-char)
  (paredit-kill)
  (evil-insert 0))

(use-package evil
  :config
  (evil-mode 1)

  (setq
    evil-regexp-search nil
    evil-cross-lines t
    evil-ex-substitute-global t)

  (setq-default
    evil-shift-width 2)

  (evil-set-initial-state 'text-mode 'insert)

  (define-key evil-insert-state-map (kbd "C-`") 'describe-key)
  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-c a") 'razzi/abbrev-or-add-global-abbrev)
  (define-key evil-insert-state-map (kbd "<C-i>") 'hippie-expand)
  (define-key evil-insert-state-map (kbd "C-f") 'company-files)
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-j") 'razzi/simple-newline)
  (define-key evil-insert-state-map (kbd "C-k") 'paredit-kill)
  (define-key evil-insert-state-map (kbd "C-l") 'paredit-forward-slurp-sexp)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-complete-previous)
  (define-key evil-insert-state-map (kbd "C-t") 'razzi/transpose-prev-chars)
  (define-key evil-insert-state-map (kbd "M-v") 'evil-paste-before)
  (define-key evil-insert-state-map (kbd "<C-return>") 'razzi/wrap-in-parens)
  ;; (define-key evil-insert-state-map (kbd "<tab>") 'company-complete-selection)

  (define-key evil-normal-state-map (kbd "#") 'razzi/pound-isearch)
  (define-key evil-normal-state-map (kbd "*") 'razzi/star-isearch)
  (define-key evil-normal-state-map (kbd "-") 'razzi/transpose-next-line)
  (define-key evil-normal-state-map (kbd "<C-i>") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "<backtab>") 'bs-cycle-previous)
  (define-key evil-normal-state-map (kbd "<tab>") 'bs-cycle-next)
  (define-key evil-normal-state-map (kbd "C") 'razzi/paredit-change)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "D") 'razzi/kill-line-and-whitespace)
  (define-key evil-normal-state-map (kbd "E") 'forward-symbol)
  (define-key evil-normal-state-map (kbd "S") 'razzi/paredit-change-line)
  (define-key evil-normal-state-map (kbd "M-RET") 'delete-window)
  (define-key evil-normal-state-map (kbd "M-[") 'my-toggle-frame-left)
  (define-key evil-normal-state-map (kbd "M-]") 'my-toggle-frame-right)
  (define-key evil-normal-state-map (kbd "M-a") 'mark-whole-buffer) ; TODO make this keep point where it is
  (define-key evil-normal-state-map (kbd "M-n") 'scroll-other-window)
  (define-key evil-normal-state-map (kbd "M-p") 'scroll-other-window-down)
  (define-key evil-normal-state-map (kbd "M-q") 'save-buffers-kill-terminal)
  (define-key evil-normal-state-map (kbd "Q") 'razzi/replay-q-macro)
  (define-key evil-normal-state-map (kbd "RET") 'razzi/clear)
  (define-key evil-normal-state-map (kbd "Y") 'razzi/copy-to-end-of-line)
  (define-key evil-normal-state-map (kbd "[ SPC") 'razzi/insert-newline-before)
  (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  (define-key evil-normal-state-map (kbd "] SPC") 'razzi/insert-newline-after)
  (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "_") 'razzi/transpose-previous-line)
  (define-key evil-normal-state-map (kbd "g'") 'goto-last-change)
  (define-key evil-normal-state-map (kbd "g-") 'razzi/checkout-previous-branch)
  (define-key evil-normal-state-map (kbd "g;") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd "gb") 'magit-blame)
  (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)
  (define-key evil-normal-state-map (kbd "gd") 'magit-diff-unstaged)
  (define-key evil-normal-state-map (kbd "gf") 'razzi/file-at-point)
  (define-key evil-normal-state-map (kbd "gl") 'razzi/magit-pull)
  (define-key evil-normal-state-map (kbd "go") 'evil-open-above)
  (define-key evil-normal-state-map (kbd "gp") 'razzi/magit-push)
  (define-key evil-normal-state-map (kbd "gs") 'magit-status)
  (define-key evil-normal-state-map (kbd "gz") 'razzi/magit-stash-wip)
  (define-key evil-normal-state-map (kbd "gZ") 'magit-stash-pop)
  (define-key evil-normal-state-map (kbd "g SPC") 'razzi/magit-commit)
  (define-key evil-normal-state-map (kbd "v") 'razzi/save-kill-visual)
  (define-key evil-normal-state-map (kbd "~") 'razzi/tilde)

  ; todo
  ;; (define-key evil-normal-state-map (kbd "C-]") 'razzi/tag-in-split)
  (define-key evil-visual-state-map (kbd "!") 'sort-lines)
  ;; (define-key evil-visual-state-map (kbd "~") 'razzi/switch-case-based-on-first-char)
  (define-key evil-visual-state-map (kbd "#") 'razzi/pound-isearch)
  (define-key evil-visual-state-map (kbd "C-`") 'describe-key)
  (define-key evil-visual-state-map (kbd "$") 'razzi/almost-end-of-line)
  (define-key evil-visual-state-map (kbd "'") 'razzi/surround-with-single-quotes)
  (define-key evil-visual-state-map (kbd ")") 'razzi/surround-with-parens)
  (define-key evil-visual-state-map (kbd "]") 'razzi/surround-with-brackets)
  (define-key evil-visual-state-map (kbd "*") 'razzi/star-isearch)
  (define-key evil-visual-state-map (kbd "E") 'forward-symbol)
  (define-key evil-visual-state-map (kbd "V") 'razzi/increase-region)
  (define-key evil-visual-state-map (kbd "\"") 'razzi/surround-with-double-quotes)
  (define-key evil-visual-state-map (kbd "ae") 'mark-whole-buffer)
  (define-key evil-visual-state-map (kbd "il") 'razzi/mark-line-text)
  (define-key evil-visual-state-map (kbd "p") 'razzi/paste-replace)
  (define-key evil-visual-state-map (kbd "s") 'evil-surround-region)
  (define-key evil-visual-state-map (kbd "v") 'razzi/erase-kill-visual)
  (define-key evil-visual-state-map (kbd "SPC") 'evil-inner-symbol)

  (define-key evil-operator-state-map (kbd "V") 'evil-a-paragraph)
  (define-key evil-operator-state-map (kbd "E") 'forward-symbol)
  (define-key evil-operator-state-map (kbd "p") 'evil-inner-paren)
  (define-key evil-operator-state-map (kbd "SPC") 'evil-inner-symbol)

  (add-hook 'evil-insert-state-exit-hook 'save-if-file)
  )

; TODO use to refactor star-isearch
;; (use-package evil-visualstar
;;   :config
;;   (global-evil-visualstar-mode t))

(use-package circe
  :config
  ; TODO authenticate n sheit
  ;; https://github.com/jorgenschaefer/circe/wiki/Configuration#hiding-other-messages
  (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))
  (circe-set-display-handler "QUIT" (lambda (&rest ignored) nil))
  (evil-set-initial-state 'circe-mode 'emacs)
  )


(use-package evil-nerd-commenter)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package projectile
  :config
  (projectile-global-mode)
  (setq
    projectile-enable-caching t
    projectile-completion-system 'helm
    projectile-switch-project-action #'projectile-commander))

(use-package change-inner)

(defun razzi/kill-inside-parens ()
  (interactive)
  (let ((text (buffer-substring (point) (line-end-position))))
    (if (s-contains? "(" text)
      (change-inner* nil "(")
      (progn
        (search-backward "(" (line-beginning-position) nil 1)
        (change-inner* nil "("))
      )))

(defun razzi/exit-insert ()
  (interactive)
  (expand-abbrev)
  (evil-normal-state))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "kj" 'razzi/exit-insert)
  (key-chord-define evil-normal-state-map "dp" 'razzi/kill-inside-parens)
  (setq key-chord-two-keys-delay 0.3))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
  (define-key yas-keymap (kbd "<tab>") nil))

(use-package virtualenvwrapper
  :config
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs"))

(use-package s)

(defun razzi/eshell-abbrev-and-return ()
  (interactive)
  (expand-abbrev)
  (eshell-send-input))

(defun razzi/eshell-point-to-prompt ()
  (interactive)
  (goto-char (point-max))
  (evil-append 0 0 nil))

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

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (setq js-indent-level 2)
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
  ; TODO add operator to move to end of word
  ;; (modify-syntax-entry ?_ "w" python-mode-syntax-table)

  ;; (add-to-list 'company-backends 'company-jedi)

  (setq evil-shift-width 4)

  (evil-leader/set-key-for-mode 'python-mode
    "-" 'razzi/run-test-pytest
    "8" 'razzi/autopep8
    "=" 'razzi/run-pytest
    "p" 'razzi/importmagic)

  (evil-define-key 'insert python-mode-map
    (kbd "#") 'razzi/python-pound-and-space
    (kbd ";") 'razzi/python-pound-and-space
    ;(kbd "C-h") 'py-electric-backspace)
    ))

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
  (kbd "C-j") 'nil
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

(defun razzi/magit-commit ()
  (interactive)
  (when (not (magit-anything-staged-p))
    (magit-run-git "add" "-u" "."))
  (let ((same-window-regexps nil))
    (magit-commit)))

(defun razzi/magit-push ()
  (interactive)
  (magit-run-git "push"))

(defun razzi/put-before ()
  (interactive)
  (evil-with-single-undo
    (evil-insert-newline-above)
    (indent-for-tab-command)
    (insert (s-trim (current-kill 0)))
    (forward-line)))

(defun test ()
  (interactive)
  (if (looking-at "\\_<")
      (message "yes")
    (message "no")
    ))

; TODO refactor
(defun razzi/star-isearch ()
  (interactive)
  (while (not (looking-at "[A-z]"))
      (forward-char))

  (let ((inhibit-redisplay 1)
        (selection (evil-visual-state-p))
        (visual-type (evil-visual-type))
        (text (if (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    ; Go to the start of the word if not in visual and not already at the start
    (when (and (not selection)
               (not (looking-at "\\_<")))
      (backward-sexp))
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
  (interactive)
  (while (not (looking-at "[A-z]"))
      (backward-char))
  (let ((inhibit-redisplay 1)
        (selection (evil-visual-state-p))
        (visual-type (evil-visual-type))
        (text (if (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    (when (and (not selection)
               (not (looking-at "\\_<")))
      (progn
        (backward-sexp)
        (if (eq ?' (char-before (point)))
            (forward-char))))
    (evil-exit-visual-state)
    (isearch-mode nil)
    (isearch-yank-string text)
    (isearch-done)
    (evil-search-next)
    (when (and
            selection
            (not (eq visual-type 'line)))
      (evil-search-previous))))

(defun current-line-empty-p ()
   (string-match-p "^\s*$" (thing-at-point 'line)))

(defun razzi/elisp-semicolon-and-space ()
  (interactive)
  ; TODO broken
  ; space preceeds if line is not empty and at end of line
  (if (and (not (current-line-empty-p)) (eolp))
    (insert " ; ")
    (progn
      (paredit-semicolon)
      (insert " "))))

(defun razzi/python-pound-and-space ()
  (interactive)
  (if (and (not (current-line-empty-p)) (eolp))
    (insert "  # ")
    (insert "# ")))

(defun razzi/toggle-between-buffers ()
  (interactive)
  (let ((target (cl-some 'buffer-file-name (cdr (buffer-list)))))
    (find-file target)
    ))

(defun razzi/tilde (count)
  (interactive "P")
  (let ((repeat (or count 1)))
    (if (> repeat 0)
        (if (looking-at "[A-z]")
            (progn
              (evil-invert-char (point) (+ (point) 1))
              (right-char)
              (razzi/tilde (- repeat 1)))
          (progn
            (right-char)
            (razzi/tilde count))))))

(add-hook 'emacs-lisp-mode-hook (lambda ()
    (enable-paredit-mode)

    (setq
      evil-shift-width 2
      tab-width 2)

    (evil-define-key 'insert emacs-lisp-mode-map
      (kbd ";") 'razzi/elisp-semicolon-and-space)
  ))

(define-key input-decode-map "\C-i" [C-i])

(defun razzi/transpose-last-2-chars (string)
  "If string is less than 2 chars, return it."
  (if (< (length string) 2)
      (string)
    (let ((prefix (substring string 0 -2))
          (second-to-last-char (aref string (- (length string) 2)))
          (last-char (aref string (- (length string) 1))))
      (format "%s%c%c" prefix last-char second-to-last-char))))

(defun razzi/isearch-transpose-char ()
  (interactive)
  (let* ((string isearch-string)
         (len (length isearch-string))
         (second-to-last-char (aref string (- len 2)))
         (last-char (aref string (- len 1))))
    (isearch-pop-state)
    (isearch-pop-state)
    (isearch-process-search-char last-char)
    (isearch-process-search-char second-to-last-char)
    )
  )

(define-key isearch-mode-map (kbd "C-j") 'isearch-done)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-t") 'razzi/isearch-transpose-char)
(define-key isearch-mode-map (kbd "C-`") 'describe-key)
(define-key isearch-mode-map (kbd "M-v") 'isearch-yank-pop)
(define-key isearch-mode-map (kbd "C-w") 'bp/isearch-delete-word)

(define-key dired-mode-map (kbd "C-j") 'dired-find-file)

(global-set-key (kbd "M-v") 'evil-paste-after)

(define-key minibuffer-local-isearch-map (kbd "M-v") 'nil)
(define-key minibuffer-local-isearch-map (kbd "M-v") 'isearch-yank-kill)
(define-key minibuffer-local-isearch-map (kbd "C-w") 'bp/isearch-delete-word)

; TODO understand and refactor
(defun isearch--remove-nonword-suffixes (str el)
  (if (s-ends-with? el str)
      str
    (isearch--remove-nonword-suffixes
     (substring str 0 (1- (string-width str)))
     el)))

(defun isearch--push-states-of-string (str len)
  (cl-loop
   for iter from 0 to (1- len) do
   (let* ((i (s-left iter str))
          (isearch-string i)
          (isearch-message i))
     (save-excursion
       (setf (point) (+ isearch-other-end iter))
       (push (isearch--get-state) isearch-cmds)))))

(defun bp/isearch-delete-word ()
  "Delete word in the `isearch-string'.
Split strings by whitespace, dashes, underscores and camelcase.

Push the current isearch-string to the `isearch-cmds' stack of
search status elements to allow for a subsequent
`isearch-delete-char' to further manipulate the string at hand."
  (interactive)
  ;; keep isearch prompt when string is empty
  (if (equal isearch-string "")
      (isearch-update)
    (let* ((str isearch-string)
           (lst (s-split-words str))
           (el (car (last lst)))
           (nosuf (isearch--remove-nonword-suffixes str el))
           (res (s-chop-suffix el nosuf))
           (len (string-width res)))
      (setq isearch-string res
            isearch-message res)
      (isearch--push-states-of-string str len)
      (isearch-search-and-update))))

(defun minibuffer-config ()
  (interactive)
  (local-set-key (kbd "C-j") 'exit-minibuffer)
  (local-set-key (kbd "C-h") 'delete-backward-char)
  (define-key minibuffer-local-map (kbd "C-`") 'describe-key))

(add-hook 'minibuffer-setup-hook 'minibuffer-config)

; (this is a django mode for emacs)
;; (use-package pony-mode)

; todo
; persistent undo
; persistent marks
; show marks in gutter
; c-j xml put cursor in between tags
; in docstring, auto indent after first
; case insensitive completion eshell
; python tab to outdent a level?

; visual block i to block insert (may be impossible as i is a prefix)
; eshell highlight valid commands
; *** razzi/extract-as-variable
; prompt for a var name and then extract the current region into a var
; with open(fn) as f
;   x = f.read()
;   y = json.parse(f.read())
;                   ^ this turns into x

; no scroll past end of buffer
; projectile c-w kill word

; o to indent new line if in class scope - if 2 lines open on toplevel (hard?)
; xml auto close tag
; eshell smarter tab completion
; helm c-w delete word (clear?)

; use helm for find-tag
; smerge mode bindings: next, rebind return, keep both
; simpler defun yasnippet
; magit commit autopopulate with ref
; [|ret] throw the close bracket on the correct line

; bind substitute - looks like
; (define-key evil-normal-state-map (kbd "g / r") (lambda () (evil-ex "%s/")))
; cs[ on a line before [ doesn't work
; no debug on error in eshell
; eshell make - word syntax
