(load-file "~/.emacs.d/lisp/init_use_package.el")

(setq
  abbrev-file-name "~/.emacs.d/abbrev_defs.el"
  backup-directory-alist `((".*" . "~/.emacs.d/backups/"))
  column-number-mode t
  dired-recursive-deletes 'always
  eshell-rc-script "~/.emacs.d/eshell/profile.el"
  eshell-banner-message ""
  frame-title-format "%f"
  indent-tabs-mode nil
  inhibit-splash-screen t
  inhibit-startup-message t
  initial-scratch-message nil
  isearch-regexp nil
  mouse-wheel-scroll-amount '(1 ((shift) . 1))
  next-line-add-newlines nil
  ns-pop-up-frames nil
  python-python-command "/usr/local/bin/python3.5"
  save-abbrevs 'silently
  use-package-always-ensure t
  vc-follow-symlinks t
  visible-bell nil
  gc-cons-threshold 20000000
  ring-bell-function 'ignore
  ;; scroll-preserve-screen-position t
  )

(setq-default
  abbrev-mode t
  tab-width 2
  )

(add-to-list 'load-path "lisp")
(tool-bar-mode 0)
(menu-bar-mode -1)
(winner-mode 1)
(set-face-attribute 'default nil :height 182)
(server-start)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
;; (scroll-restore-mode)

; TODO the close parents are jumpy
(electric-pair-mode)

;; (global-set-key (kbd "M-v") 'evil-paste-after)

;; (setq
;;   whitespace-display-mappings
;;   '(
;;     (space-mark 32)
;;     (newline-mark 10)
;;   )
;; )
;; (global-whitespace-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'focus-out-hook 'save-if-file)

(use-package evil-numbers)

(use-package pyenv-mode)

(use-package evil-tabs
  :config
  (global-evil-tabs-mode t))

(use-package thingatpt)

(use-package emmet-mode
	:config
	(add-hook 'nxml-mode-hook (lambda () (emmet-mode)))
	(add-hook 'html-mode-hook (lambda () (emmet-mode)))

	(define-key emmet-mode-keymap (kbd "C-j") nil)
	)

(use-package hippie-exp
  :bind ("<tab>" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list '(
      yas-hippie-try-expand
      ;; emmet-expand-line
      )
    )
  )

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  ;; (highlight-numbers-mode)
  )

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
    ido-everywhere t)
    ido-cannot-complete-command 'ido-next-match
  )

(use-package magit)

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

(use-package helm-flx
  :config
  (helm-flx-mode 1)
  )

(use-package helm
  :defines
  ;; helm-mode-fuzzy-match
  ;; helm-M-x-fuzzy-match
  ;; helm-buffers-fuzzy-match
  ;; helm-find-file-fuzzy-match
  ;; helm-recentf-fuzzy-match
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq
    ;; helm-mode-fuzzy-match t
    helm-M-x-fuzzy-match t
    ;; helm-autoresize-max-height 10
    ;; helm-buffers-fuzzy-match t
    ;; helm-recentf-fuzzy-match t
    ;; helm-find-file-fuzzy-match t
    ;; helm-split-window-in-side-p t
    ;; helm-split-window-default-side 'below
    ;; helm-always-two-windows nil
    )
  )

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
  :disabled t
  :init
  (setq
    flycheck-display-errors-delay .4
    flycheck-highlighting-mode 'lines
    flycheck-disabled-checkers '(emacs-lisp-checkdoc)
    )
  :config
  (global-flycheck-mode nil)
  )

; todo....
;; (use-package ido-ubiquitous
;;   :config
;;   (ido-ubiquitous-mode 1)
;;   )

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq
    company-minimum-prefix-length 1
    )
  )

;; (require 'company-simple-complete "~/.emacs.d/company-complete-cycle.el")

(use-package company-flx
  :config
  (with-eval-after-load 'company
    (company-flx-mode +1))
  )

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  )

;; (use-package helm
;;   :config
;;   (setq helm-autoresize-max-height 20)
;;   )

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
(use-package python-mode)

(use-package restart-emacs)

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

(defun edit-private-xml ()
  (interactive)
  (find-file "~/Library/Application Support/Karabiner/private.xml")
  )

(defun razzi/copy-paragraph ()
  (interactive)
  (move-beginning-of-line nil)
  (let ((sentence (thing-at-point 'defun)))
    (insert sentence)
    (insert "\n")
    ; TODO does this work in python?
    )
  )

(defun razzi/kill-buffer-and-window ()
  (interactive)
  (kill-this-buffer)
  (delete-window)
  )

(defun razzi/show-messages ()
  (interactive)
  (message "TODO")
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

(use-package helm-projectile)

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
    "8" 'razzi/autopep8
    ";" 'previous-buffer
    "A" 'add-global-abbrev
    "C" 'razzi/magit-checkout-file
    "E" 'eval-buffer
    "DEL" 'restart-emacs
    "SPC" 'save-buffer
    "a" 'add-global-abbrev
    "b" 'magit-blame
    "c" 'razzi/copy-paragraph
    "d" 'magit-diff-unstaged
    "e" 'eshell
    "f" 'razzi/yank-file-name
    "g" 'magit-status
    "h" 'help-command
    "i" 'edit-init
    "j" 'avy-goto-char
    ;; "k" 'edit-private-xml
    "l" 'paredit-forward-slurp-sexp
    "m" 'razzi/show-messages
    "n" 'edit-notes
    "o" 'razzi/put-after
    ; "p" 'razzi/edit-eshell-profile
    "p" 'razzi/importmagic
    ;; "q" 'razzi/kill-buffer-and-window
    "q" 'evil-tab-sensitive-quit
    "r" 'helm-recentf
    "s" 'switch-to-scratch
    "t" 'helm-projectile
    "v" 'eval-last-sexp
    ;; "b" 'eval-buffer
    )
  )

(use-package pt)

(defun razzi/transpose-prev-chars ()
  (interactive)
  (backward-char 1)
  (transpose-chars nil)
  )


(defun razzi/importmagic ()
  (interactive)
  ; todo use the -o flag and prepend the imports
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
  (delete-other-windows)
  (magit-blame-quit)
  )

(use-package paredit)
; todo no delete closing parens

(use-package evil
  :config
  (evil-mode 1)

  (setq
    evil-regexp-search nil
    evil-cross-lines t
    evil-ex-substitute-global t
    )

  ;
  (define-key evil-insert-state-map (kbd "C-`") 'describe-key)

  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-c a") 'inverse-add-global-abbrev)
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-complete-previous)
  (define-key evil-insert-state-map (kbd "C-t") 'razzi/transpose-prev-chars)

  (define-key evil-normal-state-map (kbd "#") 'razzi/pound-isearch)
  (define-key evil-normal-state-map (kbd "*") 'razzi/star-isearch)
  (define-key evil-normal-state-map (kbd "<C-i>") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "<backtab>") 'elscreen-previous)
  (define-key evil-normal-state-map (kbd "<tab>") 'evil-tabs-goto-tab)
  (define-key evil-normal-state-map (kbd "=") 'razzi/run-pytest)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-s") 'paredit-forward-slurp-sexp)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "M-RET") 'my-toggle-frame-maximized)
  (define-key evil-normal-state-map (kbd "M-[") 'my-toggle-frame-left)
  (define-key evil-normal-state-map (kbd "M-]") 'my-toggle-frame-right)
  (define-key evil-normal-state-map (kbd "M-n") 'elscreen-create)
  (define-key evil-normal-state-map (kbd "M-p") 'scroll-other-window)
  (define-key evil-normal-state-map (kbd "M-q") 'save-buffers-kill-terminal)
  (define-key evil-normal-state-map (kbd "RET") 'razzi/clear)
  (define-key evil-normal-state-map (kbd "[ SPC") 'razzi/insert-newline-before)
  (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  (define-key evil-normal-state-map (kbd "] SPC") 'razzi/insert-newline-after)
  (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)

  (define-key evil-visual-state-map (kbd "!") 'sort-lines)
  (define-key evil-visual-state-map (kbd "ae") 'mark-whole-buffer)
  (define-key evil-visual-state-map (kbd "V") 'evil-a-paragraph)
  (define-key evil-visual-state-map (kbd "s") 'evil-surround-region)

  (define-key evil-operator-state-map (kbd "V") 'evil-a-paragraph)

  (add-hook 'evil-insert-state-exit-hook 'save-if-file)

  ;; todo
  ;; (define-key evil-insert-state-map (kbd "C-j") 'newline-and-indent)
  )

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
    )
  )

(use-package change-inner)

(defun change-inner-parens ()
  (interactive)
  (change-inner* nil "(")
  ; TODO fails in the middle of words
  ;; (command-execute 'change-inner nil ["("])
  )

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "dp" 'change-inner-parens)
  (setq
    key-chord-two-keys-delay 0.5
    )
  )

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

(defun abbrev-and-return ()
  (interactive)
  (expand-abbrev)
  (eshell-send-input)
  )


(use-package virtualenvwrapper
  :config
  (venv-initialize-eshell)
  (setq venv-location "~/.emacs.d/venvs")
  )

(use-package s)

(defun razzi/eshell-point-to-prompt ()
  (interactive)
  (end-of-buffer)
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

(add-hook 'js-mode-hook (lambda ()
    (modify-syntax-entry ?_ "w")
    (setq
      js-indent-level 2
      evil-shift-width 2
      )
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

(add-hook 'python-mode-hook (lambda ()
    ;; (superword-mode)
    (modify-syntax-entry ?_ "w" python-mode-syntax-table)
    )
  )

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

(add-hook 'eshell-mode-hook (lambda ()
    (local-set-key (kbd "C-u") 'eshell-kill-input)
    (local-set-key (kbd "M-RET") 'my-toggle-frame-maximized)
    (local-set-key (kbd "C-j") 'nil)
    (local-set-key (kbd "C-j") 'abbrev-and-return)
    (local-set-key (kbd "C-a") 'previous-line)

    (evil-define-key 'normal eshell-mode-map
      (kbd "A") 'razzi/eshell-point-to-prompt
      )

    (evil-define-key 'insert eshell-mode-map
      (kbd "C-c") 'eshell-interrupt-process
      (kbd "C-a") 'eshell-bol
      (kbd "C-n") 'eshell-next-input
      (kbd "C-p") 'eshell-previous-matching-input-from-input
      )

    ; TODO npm is weird with eshell :O
    ;; (add-to-list 'eshell-output-filter-functions '(lambda ()
    ;;   (save-excursion
    ;;     (replace-regexp "\[[\?0-9]+[hlAGKJ]" "" nil eshell-last-output-start eshell-last-output-end)
    ;;     )
    ;;   )
    ;; )

    ; todo
    ;; (add-hook 'eshell-post-command-hook
    ;;   (lambda ()
    ;;     ;; (eshell/ls)
    ;;     )
    ;;   )

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
    )
  )



(defun my-toggle-frame-maximized ()
  (interactive)
  ;; (setq ns-auto-hide-menu-bar nil)
  (set-frame-position nil 0 0)
  ;; (set-frame-size nil 113 66)
  (set-frame-size nil 113 35)
  )

; TODO this is unnecessary?
(defun my-toggle-frame-left () ;(small)
  (interactive)
  ;; (let screen-size-alist (small . (113 35)))
  ;; (if (small))
  (set-frame-position nil 0 0)
  ;; (set-frame-size nil 113 66)
  (set-frame-size nil 113 35)
  )

(defun my-toggle-frame-right ()
  (interactive)
  (set-frame-position nil (/ (display-pixel-width) 2) 0)
  ;; TODO do the math
  (set-frame-size nil 113 66)
  )

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

(defun razzi/star-isearch ()
  (interactive)
  (let ((inhibit-redisplay 1))
    (isearch-mode t)
    (isearch-yank-word)
    (isearch-done)
    (evil-search-next)))

(defun razzi/pound-isearch ()
  (interactive)
  (let ((inhibit-redisplay 1))
    (isearch-mode nil)
    (isearch-yank-word)
    (isearch-done)
    (evil-search-next)))

(add-hook 'emacs-lisp-mode-hook (lambda ()
    (enable-paredit-mode)
    (setq
      evil-shift-width 2
      tab-width 2
      indent-tabs-mode nil
      )
    )
  )

;; (global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C-j") 'indent-new-comment-line)

(define-key input-decode-map "\C-i" [C-i])

(define-key isearch-mode-map (kbd "C-j") 'isearch-done)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
;; (define-key isearch-mode-map (kbd "C-`") 'describe-key)
(global-set-key (kbd "M-v") 'nil)
(global-set-key (kbd "M-v") 'isearch-yank-pop)
(define-key minibuffer-local-isearch-map (kbd "M-v") 'nil)
(define-key minibuffer-local-isearch-map (kbd "M-v") 'isearch-yank-pop)

(defun minibuffer-config ()
  (interactive)
  (local-set-key (kbd "C-h") 'nil)
  (local-set-key (kbd "C-h") 'delete-backward-char)
  (local-set-key (kbd "C-j") 'exit-minibuffer)
  ; todo
  ;; (local-set-key (kbd "M-v") 'isearch-yank-pop)
  )

(add-hook 'minibuffer-setup-hook 'minibuffer-config)
;; (setq ring-bell-function 'ignore)
;; (setq
;;  eshell-banner-message ""
;;  eshell-scroll-to-bottom-on-input t
;;  )

(use-package monokai-theme
  :config
  (load-theme 'monokai t)
  )

(electric-indent-mode -1)

; todo
; VV ?
; insert mode c-l
; wip elisp move stuff into own files
; hide undo-tree files
; automatic space after comment!
                                        ; eshell
; highlight valid commands
; space as first char to switch back to other frame

; `. goto last changed spot
; http://acroca.com/blog/2013/09/13/speed-up-github-connection.html
; http://wikemacs.org/wiki/Shell#Shell_completion_with_a_nice_menu_.C3.A0_la_zsh
; disable scratch save status indicator
;; todo ido... http://stackoverflow.com/questions/7860894/ido-mode-and-tab-key-not-working-as-expected-in-24-0-x0-builds
; v a e
;; http://emacs.stackexchange.com/questions/4129/how-do-i-make-ido-switch-to-the-buffer-suggested-by-the-tab-completion-candidate
					; clever parens >:(
; set|var -> set(var)
; don't move to the right like that emacs...
; search c-t transpose chars
; search c-g cancel
; :tag command
; open most recent by default?
; open recentf list in scratch buffer? x

; search c-w delete word, not paste...
; visual block i to block insert
																				; persistent marks
; show marks in gutter
; c-j xml put cursor in between tags
; !!! disable tab to emmet expand in minibuffer x.x
; textobj i l
;paredit is being overzealous in matching closing
; copy path to function
; in docstring, auto indent after first
; electric pair mode

; search c-g cancel ?
; rename current file
;persistent undo
; c-l c-h switch window left / right
; case insensitive completion eshell
; evil f case insensitive
; google search!
; magit rebind j,k
; eshell ... up 2 dirs
; eshell in split
; rgrep bindings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-cannot-complete-command (quote ido-next-match)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; python tab to outdent a level?
; python newline when already outdented preserve outdent
; paste during visual should use clipboard
; rebind c-h to paredit-backward-delete once I figure out what's going on with my init file
; somehow correct things like pyton_source - perhaps trigger an action on insert _
; *** reassign variables when I make the same call
; with open(fn) as f
;   x = f.read()
;   y = json.parse(f.read())
;                   ^ this turns into x

; make spc ; toggle between 2 buffers
; tab between only file buffers
;; compile window not overwrite
;; compile window c-l move window
; compile remove line that says mode compilehttp://rsiguard.remedyinteractive.com/products/
; no scroll past end of buffer
; smarter VV when line has opening paren
;; prevent scroll past end of buffer
; m-v paste
; projectile c-w kill word

; switching to eshell messes up keybindings
; c-c only set to cancel in eshell mode
; c-n only set in eshell mode
; make comments way more visible
; tabnew take filename

; # automatic insert space after python mode
;; (my-toggle-frame-right)
; persistent winner
; spc O put before
; eshell c-d send exit
; """ autoclose with formatted docstring
; newline after (setq should have 2 indents
; eshell isn't putting the cursor on at eol

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

; xml auto close tag
; Q repeat macro
; c-x c-f autocomplete file
