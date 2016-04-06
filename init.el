(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq
  abbrev-file-name "~/.emacs.d/abbrev_defs.el"
  backup-directory-alist `((".*" . "~/.emacs.d/backups/"))
  initial-scratch-message nil
  use-package-always-ensure t
  isearch-regexp nil
  save-abbrevs 'silently
  indent-tabs-mode nil
  tab-width 2
  js-indent-level 2
; todo!!
  ns-pop-up-frames nil
  vc-follow-symlinks t
  frame-title-format "%f"
  ;; tags-file-name "~/code/clint/etags"
  )

(server-start)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(set-face-attribute 'default nil :height 182)
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

; todo (these work now)
(defun razzi/focus-out-hook ()
  (interactive)
  ; TODO have timer or sth
  ;; (evil-normal-state)
  (save-if-file)
  )

(add-hook 'focus-out-hook 'razzi/focus-out-hook)
;; (add-hook 'focus-in-hook (lambda () (interactive) (save-buffer)))

;; (use-package focus-autosave-mode
;;   :config
;;   (focus-autosave-mode 1)
;;   )

(use-package thingatpt)

(use-package highlight-numbers
  :config
  (highlight-numbers-mode)
  )

(use-package fish-mode)

;; (use-package undo-tree
;;   :config
;;   (global-undo-tree-mode)
;;   (setq
;;     undo-tree-auto-save-history t
;;     )
;;   )


(add-to-list 'load-path "lisp")

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
  :defines helm-M-x-fuzzy-match
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq
    helm-M-x-fuzzy-match t
    helm-autoresize-max-height 10
    helm-buffers-fuzzy-matching t
    helm-recentf-fuzzy-matching t
    helm-find-file-fuzzy-matching t
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
    flycheck-display-errors-delay .08
    flycheck-highlighting-mode 'lines
    flycheck-disabled-checker '(emacs-lisp-checkdoc)
    )
  :config
  (global-flycheck-mode nil)
  )

; todo....
;; (use-package ido-ubiquitous
;;   :config
;;   (ido-ubiquitous-mode 1)
;;   )


; (fuzzy-match-score "cf" "colorful" 'fuzzy-jaro-winkler-score)
; (fuzzy-match "cf" "colorful")

;; (use-package auto-complete
;;   :config
;;   (ac-config-default)
;;   (add-to-list 'ac-modes 'eshell-mode)
;;   (setq
;;     ac-auto-show-menu 0.1
;;     ac-delay 0.05
;;     ac-use-fuzzy t
;;     ac-auto-start 0
;;     ac-ignore-case t
;;     )
;;   )

;; (use-package fuzzy
;;   :config
;;   (setq
;;     fuzzy-match-accept-error-rate .5
;;     fuzzy-match-accept-length-difference 20
;;     )
;;   )


;; (defun my-company-hook ()
;;   (interactive)
;;   (define-key company-active-map [return] 'nil)
;;   (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
;;   )

;; (use-package company
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   (add-hook 'company-mode-hook 'my-company-hook)
;;   (setq
;;     company-minimum-prefix-length 1
;;     )
;;   )

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  )

;; (use-package helm
;;   :config
;;   (setq helm-autoresize-max-height 20)
;;   )

(use-package avy)

(use-package shackle
  :config
  (shackle-mode)
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :other t :ratio 0.3))
    ;; shackle-default-rule '(:align t)
    )
  )

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

(use-package restart-emacs)

(use-package elpy
  :disabled t
  :config
  (elpy-enable)
  )

(defun insert-newline-after ()
  (interactive)
  (save-excursion
    (forward-line)
    (move-beginning-of-line 1)
    (newline)
  )
)

(defun insert-newline-before ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (newline)
  )
)

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*")
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
  ; (mark-paragraph) ??
  (interactive)
  (move-beginning-of-line nil)
  (let ((sentence (thing-at-point 'sentence)))
    (message "sentence is")
    (message sentence)
    (insert sentence)
    (insert "\n\n")
    ; TODO
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
  (kill-new (buffer-file-name))
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
					; todo get current branch (revision)
  ; todo get current file
  (magit-checkout-file "HEAD" (buffer-file-name))
  ;; (git-gutter:update-all-windows)
  )

(add-to-list 'load-path "~/.emacs.d/helm-projectile")
(require 'helm-projectile)
; (use-package helm-projectile)

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "," 'razzi/append-comma
    ";" 'previous-buffer
    "A" 'add-global-abbrev
    "C" 'razzi/magit-checkout-file
    "DEL" 'restart-emacs
    "SPC" 'save-buffer
    "a" 'add-global-abbrev
    "b" 'magit-blame
    "c" 'razzi/copy-paragraph
    "e" 'eshell
    "f" 'razzi/yank-file-name
    "g" 'magit-status
    "h" 'help-command
    "i" 'edit-init
    "j" 'avy-goto-char
    "k" 'edit-private-xml
    "m" 'razzi/show-messages
    "o" 'put-after
    "q" 'razzi/kill-buffer-and-window
    "r" 'helm-recentf
    "s" 'switch-to-scratch
    "t" 'helm-projectile
    "v" 'eval-last-sexp
    ;; "b" 'eval-buffer
    )
  )

(use-package pt)

(defun transpose-prev-chars ()
  (interactive)
  (backward-char 1)
  (transpose-chars nil)
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
    )

  (define-key evil-insert-state-map (kbd "C-`") 'describe-key)
  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-complete-previous)
  (define-key evil-insert-state-map (kbd "C-t") 'transpose-prev-chars)

  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "M-RET") 'my-toggle-frame-maximized)
  (define-key evil-normal-state-map (kbd "M-[") 'my-toggle-frame-left)
  (define-key evil-normal-state-map (kbd "M-]") 'my-toggle-frame-right)
  (define-key evil-normal-state-map (kbd "M-a") 'mark-whole-buffer)
  (define-key evil-normal-state-map (kbd "RET") 'razzi/clear)
  (define-key evil-normal-state-map (kbd "[ SPC") 'insert-newline-before)
  (define-key evil-normal-state-map (kbd "] SPC") 'insert-newline-after)
  (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)

  ; todo n and N don't work with * and #

  ; doesn't seem to wrap
  ;; (define-key evil-normal-state-map (kbd "/") 'isearch-forward)

  (define-key evil-operator-state-map (kbd "V") 'evil-a-paragraph)

  (define-key evil-visual-state-map (kbd "V") 'evil-a-paragraph)
  (define-key evil-visual-state-map (kbd "s") 'evil-surround-region)

  (add-hook 'evil-insert-state-exit-hook 'save-if-file)

  ;; todo
  ;; (define-key evil-insert-state-map (kbd "C-j") 'newline-and-indent)
  )

;; (use-package evil-remap)

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
  ; TODO fails in the middle of words?
  ;; (command-execute 'change-inner nil ["("])
  )

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "dp" 'change-inner-parens)
  ;; (key-chord-define evil-insert-state-map "SPC SPC" 'evil-normal-state)
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
    (setq js-indent-level 2)
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
    (superword-mode)
    (modify-syntax-entry ?_ "w" python-mode-syntax-table)
    )
  )

(add-hook 'eshell-mode-hook (lambda ()
    (abbrev-mode)
    ;; (eshell-cmpl-initialize)
    (local-set-key (kbd "C-u") 'eshell-kill-input)

    (local-set-key (kbd "C-p") 'eshell-previous-input)

    (local-set-key (kbd "C-j") 'nil)
    (local-set-key (kbd "C-j") 'abbrev-and-return)
    (local-set-key (kbd "C-a") 'previous-line)
    ;; (local-set-key (kbd "<tab>") 'ac-fuzzy-complete)
    (local-set-key (kbd "C-p") 'eshell-previous-matching-input-from-input)
    ;; (local-set-key (kbd "<tab>") 'pcomplete-list)
    (define-key evil-insert-state-map (kbd "C-c") 'eshell-interrupt-process)
    (define-key evil-insert-state-map (kbd "C-a") 'eshell-bol)
    ;; (local-set-key (kbd "A") nil)
    ; todo
    ;; (define-key evil-normal-state-map (kbd "A") 'razzi/eshell-point-to-prompt)
    ;; (local-set-key (kbd "<tab>") 'company-complete)
    ;; (setq ac-sources (
    ;; 	append '(
    ;; 	  ac-source-abbrev
    ;; 	  ; ac-source-words-in-same-mode-buffers
    ;; 	  ac-source-files-in-current-dir
    ;; 	  ac-source-pcomplete
    ;; 	  ac-source-library
    ;; 	  )
    ;; 	  ;; ac-sources
    ;; 	)
    ;;   )
    ;; )
    )
  )

; todo
;; (add-hook 'eshell-post-command-hook
;;   (lambda ()
;;     ;; (eshell/ls)
;;     )
;;   )
  ;; (local-set-key (kbd "<tab>") 'helm-esh-pcomplete)
  ;; (setq pcomplete-cycle-completions nil
  ;;       pcomplete-ignore-case t)))


(defun my-toggle-frame-maximized ()
  (interactive)
  ;; (setq ns-auto-hide-menu-bar nil)
  (set-frame-position nil 0 0)
  ;; (set-frame-size nil 113 66)
  (set-frame-size nil 113 35)
  )

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
  (set-frame-size nil 113 66)
  )

(tool-bar-mode 0)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))


(defun save-if-file ()
  (if (buffer-file-name)
    (save-buffer)
  )
)

(require 'utils "~/.emacs.d/lisp/utils.el")
(defun s-trim (s)
  "Remove whitespace at the beginning and end of a string."
  (s-trim-left (s-trim-right s)))

(defun put-after ()
  (interactive)
  (evil-with-single-undo
    (evil-insert-newline-below)
    (indent-for-tab-command)
    (insert (s-trim (current-kill 0)))
    (forward-line)
  )
)


(add-hook 'emacs-lisp-mode-hook (lambda ()
    (abbrev-mode)
    (enable-paredit-mode)
    (setq
      evil-shift-width 2
      tab-width 2
      )
    )
  )

;(use-package icicles
; (setq eshell-cmpl-cycle-completions t)
; (setq pcomplete-cycle-completions t)
;; (require 'eshell)
;; (require 'em-smart)
;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands nil)
;; (setq eshell-smart-space-goes-to-end t)

(global-set-key (kbd "C-j") 'newline-and-indent)
; scroll-other-window
; scroll-other-window-down

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
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq
 eshell-banner-message ""
 eshell-scroll-to-bottom-on-input t
 )

(use-package monokai-theme
  :config
  (load-theme 'monokai t)
  )

;; (use-package pony-mode)


; todo
; VV ?
; insert mode c-l
; wip elisp move stuff into own files
; hide undo-tree files
; automatic space after semicolon
; auto save abbrev defs
                                        ; eshell
; highlight valid commands
; don't right align stuff
; jump to next input line
; space as first char to switch back to other frame

; `. goto last changed spot
; turn off elisp doccheck
; paste setq combine
; electric pair
; http://acroca.com/blog/2013/09/13/speed-up-github-connection.html
; http://wikemacs.org/wiki/Shell#Shell_completion_with_a_nice_menu_.C3.A0_la_zsh
; disable scratch save status indicator
; autopep8!!
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

"
todo
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
"

(my-toggle-frame-right)
