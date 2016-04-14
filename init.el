(load-file "~/.emacs.d/lisp/init_use_package.el")

(setq
  ;; tags-file-name "~/code/clint/etags"
  abbrev-file-name "~/.emacs.d/abbrev_defs.el"
  backup-directory-alist `((".*" . "~/.emacs.d/backups/"))
  column-number-mode t
  column-number-mode t
  dired-recursive-deletes 'always
  eshell-rc-script "~/.emacs.d/eshell/profile.el"
  frame-title-format "%f"
  frame-title-format "%f"
  indent-tabs-mode nil
  inhibit-splash-screen t
  inhibit-startup-message t
  initial-scratch-message nil
  isearch-regexp nil
  mouse-wheel-scroll-amount '(1 ((shift) . 1))
  next-line-add-newlines nil
  ns-pop-up-frames nil
  save-abbrevs 'silently
  tab-width 2
  use-package-always-ensure t
  vc-follow-symlinks t
  )

(add-to-list 'load-path "lisp")
(tool-bar-mode 0)
(menu-bar-mode -1)

(set-face-attribute 'default nil :height 182)
(server-start)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
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

(add-to-list 'load-path "lisp")

(use-package evil-numbers)

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
  (highlight-numbers-mode)
  )

;; (use-package fish-mode)

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
  helm-mode-fuzzy-match
  ;; helm-M-x-fuzzy-match
  ;; helm-buffers-fuzzy-match
  ;; helm-find-file-fuzzy-match
  ;; helm-recentf-fuzzy-match
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq
    helm-mode-fuzzy-match t
    ;; helm-M-x-fuzzy-match t
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

;; (defun my-company-hook ()
;;   (interactive)
;;   (define-key company-active-map [return] 'nil)
;;   (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
;;   )

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (add-hook 'company-mode-hook 'my-company-hook)
  (setq
    company-minimum-prefix-length 1
    )
  )

(require 'company-simple-complete "~/.emacs.d/company-complete-cycle.el")

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
    "q" 'razzi/kill-buffer-and-window
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
    )

  (define-key evil-insert-state-map (kbd "C-`") 'describe-key)
  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-complete-previous)
  (define-key evil-insert-state-map (kbd "C-t") 'razzi/transpose-prev-chars)

  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  ;; (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "<C-i>") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "M-RET") 'my-toggle-frame-maximized)
  (define-key evil-normal-state-map (kbd "M-[") 'my-toggle-frame-left)
  (define-key evil-normal-state-map (kbd "M-]") 'my-toggle-frame-right)
  (define-key evil-normal-state-map (kbd "M-a") 'mark-whole-buffer)
  (define-key evil-normal-state-map (kbd "M-p") 'scroll-other-window)
  (define-key evil-normal-state-map (kbd "M-n") 'scroll-other-window-down)
  (define-key evil-normal-state-map (kbd "M-q") 'save-buffers-kill-terminal)
  (define-key evil-normal-state-map (kbd "RET") 'razzi/clear)
  (define-key evil-normal-state-map (kbd "[ SPC") 'razzi/insert-newline-before)
  (define-key evil-normal-state-map (kbd "] SPC") 'razzi/insert-newline-after)
  (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)
  (define-key evil-normal-state-map (kbd "=") 'razzi/run-pytest)
  (define-key evil-normal-state-map (kbd "TAB") 'previous-buffer)

  ; todo n and N don't work with * and #

  ; doesn't seem to wrap
  ;; (define-key evil-normal-state-map (kbd "/") 'isearch-forward)

  (define-key evil-operator-state-map (kbd "V") 'evil-a-paragraph)
  (define-key evil-normal-state-map (kbd "RET") 'delete-other-windows)
  (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "M-]") 'my-toggle-frame-right)
  (define-key evil-normal-state-map (kbd "M-[") 'my-toggle-frame-left)
  (define-key evil-normal-state-map (kbd "M-a") 'mark-whole-buffer)

  (define-key evil-visual-state-map (kbd "V") 'evil-a-paragraph)
  (define-key evil-visual-state-map (kbd "s") 'evil-surround-region)

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
    (abbrev-mode)
    ;; (eshell-cmpl-initialize)
    (local-set-key (kbd "C-u") 'eshell-kill-input)
    (local-set-key (kbd "M-RET") 'my-toggle-frame-maximized)
    (local-set-key (kbd "C-j") 'nil)
    (local-set-key (kbd "C-j") 'abbrev-and-return)
    (local-set-key (kbd "C-a") 'previous-line)

  ;; (local-set-key (kbd "C-p") 'eshell-previous-input)
  ;; (local-set-key (kbd "<tab>") 'pcomplete-list)
  ;; (eshell-cmpl-initialize)

  ;; (local-set-key (kbd "<tab>") 'ac-fuzzy-complete)
  ;; (local-set-key (kbd "C-p") 'nil)
  ;; (define-key evil-insert-state-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
  (define-key evil-insert-state-map (kbd "C-n") 'eshell-next-input)
  (define-key evil-insert-state-map (kbd "C-c") 'eshell-interrupt-process)
  (define-key evil-insert-state-map (kbd "C-a") 'eshell-bol)
  ;; (local-set-key (kbd "A") nil)
  ; todo
  (evil-define-key 'normal eshell-mode-map (kbd "A") 'razzi/eshell-point-to-prompt)
  ;; (local-set-key (kbd "<tab>") 'company-complete)
  (add-to-list 'eshell-output-filter-functions '(lambda ()
    (save-excursion
      (replace-regexp "\[[\?0-9]+[hlAGKJ]" "" nil eshell-last-output-start eshell-last-output-end)
      )
    )
  )

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
  (setq eshell-prompt-function
    (lambda ()
      (concat
       (if venv-current-name
	  (format "(%s) " venv-current-name)
	  ""
	)
	(fish-path (eshell/pwd) 30) " $ ")))

  ;; (setq ac-sources (
  ;;     append '(
  ;;       ac-source-abbrev
  ;;       ; ac-source-words-in-same-mode-buffers
  ;;       ac-source-files-in-current-dir
  ;;       ac-source-pcomplete
  ;;       ac-source-library
  ;;       )
  ;;       ;; ac-sources
  ;;     )
  ;;   )
  ;; )
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

; TODO make these not path dependent
(require 'utils "~/.emacs.d/lisp/utils.el")
(require 'razzi/interactive "~/.emacs.d/lisp/interactive.el")

(defun s-trim (s)
  "Remove whitespace at the beginning and end of a string."
  (s-trim-left (s-trim-right s)))

(defun razzi/put-after ()
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
      indent-tabs-mode nil
      )
    )
  )

(global-set-key (kbd "C-j") 'newline-and-indent)

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
(setq visible-bell nil)
;; (setq ring-bell-function 'ignore)
;; (setq
;;  eshell-banner-message ""
;;  eshell-scroll-to-bottom-on-input t
;;  )

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
; automatic space after semicolon?
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
; have to install from source
;; https://github.com/bbatsov/helm-projectile
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
; wtf dabbrevs making completion slow... time to company
; smarter VV when line has opening paren
;; prevent scroll past end of buffer

;; (my-toggle-frame-right)
