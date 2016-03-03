;;; init.el -- Summary
;;; Commentary:
; Not enthused about the opinionated spacemacs
; attempting to recreate it here
; Packages and stuff


;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package ido
  :config
  (ido-mode t)
  (setq
    ido-everywhere t)
  )

; (use-package magit)

(use-package anzu
  :config
  (global-anzu-mode 1)
  )

(use-package helm
  :defines helm-M-x-fuzzy-match
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-M-x-fuzzy-match t)
  )

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  )

(use-package recentf
  :config
  (recentf-mode 1)
  )

(use-package flycheck-haskell)

(use-package flycheck
  :init
  (setq
    flycheck-display-errors-delay .05
    flycheck-highlighting-mode 'lines
    )
  :config
  (global-flycheck-mode)
  )
; todo....
;; (use-package ido-ubiquitous
;;   :config
;;   (ido-ubiquitous-mode 1)
;;   )


; (fuzzy-match-score "cf" "colorful" 'fuzzy-jaro-winkler-score)
; (fuzzy-match "cf" "colorful")

(use-package auto-complete
  :config
  (ac-config-default)
  (add-to-list 'ac-modes 'eshell-mode)
  (setq
    ac-auto-show-menu 0.1
    ac-delay 0.05
    ac-use-fuzzy t
    ac-auto-start 0
    ac-ignore-case t
    )
  )

(use-package fuzzy
  :config
  (setq
    fuzzy-match-accept-error-rate .5
    fuzzy-match-accept-length-difference 20
    )
  )


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

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; (use-package shackle
;;   :config
;;   (shackle-mode)
;;   (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))
;;   ;; (setq shackle-default-rule '(:same t))
;;   )

(use-package mmm-mode
   :config
   (setq mmm-global-mode 'maybe)
   (mmm-add-classes
     '((markdown-python
         :submode python-mode
         :front "^```python\n"
         :back "^```$")))
   )
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)

(use-package restart-emacs)

(defun insert-newline-after ()
  (interactive)
  (save-excursion
    (forward-line)
    (move-beginning-of-line 1)
    (newline))
)

(defun insert-newline-before ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (newline)
  )
)

(use-package evil-nerd-commenter)

(defun edit-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "SPC" 'save-buffer
    "e" 'eshell
    "DEL" 'restart-emacs
    "o" 'put-after
    "q" 'kill-this-buffer
    "j" 'avy-goto-char
    "i" 'edit-init
    "v" 'eval-last-sexp
    "b" 'eval-buffer
    "," 'previous-buffer
    )
  )

(defun transpose-prev-chars ()
  (interactive)
  (backward-char 1)
  (transpose-chars nil)
  )

(use-package paredit)

(use-package evil
  :config
  (evil-mode 1)

  ;; (setq evil-escape-key-sequence "kj")
  (setq
    evil-regexp-search nil
    evil-cross-lines t
    )

  (define-key evil-normal-state-map (kbd "[ SPC") 'insert-newline-before)
  (define-key evil-normal-state-map (kbd "] SPC") 'insert-newline-after)
  (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)
  (define-key evil-visual-state-map (kbd "V") 'evil-a-paragraph)
  (define-key evil-operator-state-map (kbd "V") 'evil-a-paragraph)
  (define-key evil-normal-state-map (kbd "RET") 'delete-other-windows)

  ;; todo
  ;; (define-key haskell-error-mode-map (kbd "q") 'quit-window)

  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-`") 'describe-key)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-a") nil)
  ;; todo
  ;; (define-key evil-insert-state-map (kbd "C-j") 'newline-and-indent)
  (define-key evil-insert-state-map (kbd "C-t") 'transpose-prev-chars)
  (define-key evil-visual-state-map (kbd "s") 'evil-surround-region)
  (add-hook 'evil-insert-state-exit-hook 'save-if-file)
  )

;; (use-package evil-remap)

(use-package restclient)

(use-package projectile
  :config
  (projectile-global-mode)
  (setq
    projectile-enable-caching t
    )
  )


(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
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

(setq
  undo-tree-auto-save-history t
  isearch-regexp nil
  )

(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

(use-package virtualenvwrapper
  :config
  (venv-initialize-eshell)
  (setq venv-location "~/.emacs.d/venvs")
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
  (local-set-key (kbd "<tab>") 'pcomplete-list)
  (define-key evil-insert-state-map (kbd "C-c") 'eshell-interrupt-process)
  (define-key evil-insert-state-map (kbd "C-a") 'eshell-bol)
  ;; (local-set-key (kbd "<tab>") 'company-complete)
  (setq ac-sources (
      append '(
        ac-source-abbrev
        ; ac-source-words-in-same-mode-buffers
        ac-source-files-in-current-dir
        ac-source-pcomplete
        ac-source-library
        )
        ;; ac-sources
      )
    )
  )
)

; todo
(add-hook 'eshell-post-command-hook
  (lambda ()
    ;; (eshell/ls)
    )
  )
  ;; (local-set-key (kbd "<tab>") 'helm-esh-pcomplete)
  ;; (setq pcomplete-cycle-completions nil
  ;;       pcomplete-ignore-case t)))

(set-face-attribute 'default nil :height 182)

(setq ns-auto-hide-menu-bar t)
(tool-bar-mode 0)
(menu-bar-mode -1)
(setq scratch-initial-message nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))


(defun save-if-file ()
  (if (buffer-file-name)
    (save-buffer)
    ;; (message "no file is associated to this buffer")
  )
)

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

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


(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(add-hook 'emacs-lisp-mode-hook (lambda ()
   (setq evil-shift-width 2)))
;(use-package icicles
; (setq eshell-cmpl-cycle-completions t)
; (setq pcomplete-cycle-completions t)
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(global-set-key (kbd "C-j") 'newline-and-indent)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(set-frame-position nil 0 -25)
(set-frame-size nil 129 43)
(setq abbrev-mode t)
(setq abbrev-file-name "~/.emacs.d/abbrev_defs.el")
(define-abbrev global-abbrev-table "g" "git")

(define-key isearch-mode-map (kbd "C-j") 'isearch-done)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(defun minibuffer-config ()
  (interactive)
  (local-set-key (kbd "C-h") 'nil)
  (local-set-key (kbd "C-h") 'delete-backward-char)
  (local-set-key (kbd "C-j") 'exit-minibuffer)
  )

(add-hook 'minibuffer-setup-hook 'minibuffer-config)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq eshell-banner-message "")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(use-package monokai-theme)
(load-theme 'monokai t)

(use-package pony-mode)


; todo
; VV ?
; i c-l

(defun ac-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                    (string= pcomplete-stub ; Emacs 24
                            (substring arg
                                        (max 0
                                            (- (length arg)
                                                (length pcomplete-stub))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

;; (defvar ac-source-pcomplete
;;   '((candidates . ac-pcomplete)))

;; (defvar ac-source-library
;;   '((candidates . (list "Library"))))
