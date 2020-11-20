;;; emacs --- base config
;;; Commentary:
;;; My custom config, package management, mode config and functions.

;;; Code:
(setq user-full-name "Alexander Hagerman"
      user-mail-address "alexander@burningdaylight.io")

;; Init the package facility
(require 'package)
(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("gnu" . "https://elpa.gnu.org/packages/")
    ))
;;(custom-set-variables '(use-package-verbose nil))
(setq load-prefer-newer t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))
(setq use-package-always-ensure t)

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(use-package auto-compile
  :defer nil
  :config (auto-compile-on-load-mode))

;; Fix zsh
(setq default-directory (getenv "HOME"))
(setenv "ESHELL" (expand-file-name "~/bin/eshell"))

;; Setup layout
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-display-line-numbers-mode)
(global-visual-line-mode 1)
(column-number-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq x-stretch-cursor t)

(setq
   inhibit-startup-screen t
   initial-scratch-message nil
   sentence-end-double-space nil
   ring-bell-function 'ignore
   ;; Prompts should go in the minibuffer, not in a GUI.
   use-dialog-box nil
   ;; Fix undo in commands affecting the mark.
   mark-even-if-inactive nil
   ;; search case-sensitive by default
   case-fold-search nil
   ;; no hard tabs
   indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p) ; Accept 'y' in lieu of 'yes'.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)
(set-face-attribute 'default nil
		     :family "JetBrains Mono"
		     :height 160)

;; Setup keyboard
(setq mac-command-modifier 'meta
  mac-option-modifier 'super
  mac-control-modifier 'control
  ns-function-modifier 'hyper)

;; Configure encoding
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(global-prettify-symbols-mode +1)

;; Configure backups
(defvar --backup-directory (concat (getenv "HOME") "/.emacs.d/backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))

(custom-set-variables
    '(backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups")))))

(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
)

;; Load custom scripts
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; Configure packages
(use-package all-the-icons)

(use-package company
  :init
    (add-hook 'after-init-hook 'global-company-mode))

(use-package dash-at-point
  :bind ("C-c d" . dash-at-point))

(use-package evil
  :init ;; tweak evil's configuration before loading it
    (setq evil-search-module 'evil-search)
    (setq evil-ex-complete-emacs-commands nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (setq evil-shift-round nil)
    (setq evil-want-fine-undo t)
    (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
    (evil-mode))

(use-package expand-region
  :config
    (global-set-key (kbd "C-=") 'er/expand-region))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :config
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (add-hook 'flycheck-mode-hook 'jc/use-eslint-from-node-modules)
    (add-to-list 'flycheck-checkers 'proselint)
    (setq-default flycheck-highlighting-mode 'lines)
    ;; Define fringe indicator / warning levels
    (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-info))

(use-package flyspell
  :bind (("C-`" . ispell-word)
  ("C-~" . ispell-buffer))
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list"))

(use-package ivy
  :diminish (ivy-mode . "")
  :init
    (ivy-mode 1)
  :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 20)
    (setq ivy-count-format "%d/%d "))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

(use-package kaolin-themes
  :init
    (setq kaolin-themes-distinct-fringe t)
    (setq kaolin-themes-distinct-company-scrollbar t)
    (setq kaolin-themes-git-gutter-solid t)
  :config
    (load-theme 'kaolin-breeze t))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package neotree
  :bind
    ("<f8>" . neotree-toggle)
  :config
    (setq neo-theme (if (display-graphic-p) 'icons 'icons))
    ;; Disable line-numbers minor mode for neotree
    (add-hook 'neo-after-create-hook
              (lambda (&rest _) (display-line-numbers-mode -1)))
    (setq-default neo-show-hidden-files t)
    (setq neo-window-width 35)
    (setq neo-smart-open t)
    (set-face-attribute 'neo-button-face      nil :family "CozetteVector")
    (set-face-attribute 'neo-file-link-face   nil :family "CozetteVector")
    (set-face-attribute 'neo-dir-link-face    nil :family "CozetteVector")
    (set-face-attribute 'neo-header-face      nil :family "CozetteVector")
    (set-face-attribute 'neo-expand-btn-face  nil :family "CozetteVector"))

(use-package prescient)

(use-package projectile
  :diminish
  :bind (("C-c k" . 'projectile-kill-buffers)
  ("C-c M" . 'projectile-compile-project))
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode))

(use-package rainbow-delimiters
  :init
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package which-key
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  (which-key-idle-delay 1.2)
  :config
  (which-key-setup-minibuffer))

(use-package yasnippet
      :config
      (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
      (yas-global-mode 1))

(use-package yasnippet-snippets)


;; Lang tools
(use-package cargo
  :hook ((rust-mode toml-mode) . cargo-minor-mode))

(use-package csharp-mode
  :defer t
  :mode "\\.cs\\'"
  :config
  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile)
  :init
    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-to-list 'company-backends 'company-omnisharp)
    (add-hook 'csharp-mode-hook 'company-mode))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose\\'")

(use-package fsharp-mode
  :defer t
  :config
  (setq-default fsharp-indent-offset 2)
  :init
  (require 'eglot-fsharp)
  (add-hook 'fsharp-mode-hook 'highlight-indentation-mode))

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package plantuml-mode
  :defer t
  :mode "\\.plantuml\\'"
  :init
  (setq plantuml-executable-path "/usr/local/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable))

(use-package python-mode
  :defer t
  :mode "\\.py\\'")

(use-package flycheck-rust
  :defer t)

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  :config (setq rust-format-on-save t))

(use-package racket-mode
  :defer t
  :mode "\\.rkt\\'"
  :init
  (setq font-lock-maximum-decoration 3)
  (add-hook 'racket-mode-hook
            (lambda ()
              (define-key racket-mode-map (kbd "<f5>") 'racket-run))))

(use-package flycheck-swift
  :defer t
  :config
  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS14.2.sdk")
  (setq flycheck-swift-target "arm64-apple-ios14"))

(use-package swift-helpful
  :defer t
  :mode "\\.swift\\'")

(use-package swift-mode
  :mode "\\.swift\\'"
  :init
  (eval-after-load 'flycheck '(flycheck-swift-setup)))

(use-package swift-playground-mode
  :defer t
  :init
  (autoload 'swift-playground-global-mode "swift-playground-mode" nil t)
  (add-hook 'swift-mode-hook 'swift-playground-global-mode))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package yaml-mode
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :bind ("\C-m" . 'newline-and-indent))

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((python-mode . eglot-ensure)
         (racket-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (swift-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rls")))
  (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
  (add-to-list 'eglot-server-programs '(swift-mode . ("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))))

;; Extra modes
(use-package elfeed
  :init
    (setq elfeed-feeds
	  '("http://planet.emacsen.org/atom.xml"
	    "https://blog.acolyer.org/feed/"
	    "http://worrydream.com/feed.xml"
	    "https://lobste.rs/rss")))

;; Org
(setq sentence-end-double-space nil)
(defvar my-org-journal-dir "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
(defvar formatted-today (format-time-string "%Y%m%d"))
(defvar todays-journal (concat my-org-journal-dir formatted-today ".org"))

(use-package org
  :hook ((org-mode . visual-line-mode))
  :custom
  (org-directory my-org-journal-dir)
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-return-follows-link t)
  :config
  (defun org-mode-insert-code ()
    "Like markdown-insert-code, but for org instead."
    (interactive)
    (org-emphasize ?~))
  (defun tufte-css-numeralify (with-underscores)
    "Express the provided underscore-grouped numeral quantity in old-style capitals."
    ;; e.g. 10_000_000 becomes 10,000,000 (except fancy in the browser)
    (format
     "@@html:<span class=\"numeral\">%s</span>@@"
     (replace-regexp-in-string "_" "," with-underscores)))
  :init
  (setq org-capture-templates
	'(("t" "Todo" entry
	   (file+headline todays-journal "Tasks")
	   "* TODO %?\n  %i\n  %a")
          ("e" "Weekly Status" entry
	   (file+datetree todays-journal)
           (file "~/.emacs.d/org-templates/weekly-status.org"))
          ("i" "Interview" entry
	   (file+datetree todays-journal)
           (file "~/.emacs.d/org-templates/interview.org"))
          ("p" "Code Review" entry
	   (file+datetree todays-journal)
           (file "~/.emacs.d/org-templates/pr-review.org"))
          ("s" "New Service" entry
	   (file+datetree todays-journal)
           (file "~/.emacs.d/org-templates/new-service.org"))
	  ("j" "Journal" entry (file+datetree todays-journal)
	   "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-todo-keywords
        '((sequence "TODO(t)"
                    "STARTED(s)"
                    "WAITING(w)"
                    "SOMEDAY(.)"
                    "MAYBE(m)" "|" "DONE(x!)"
		    "QUESTION(q)"
                    "CANCELLED(c)")))
  (add-hook 'org-mode-hook
	 (lambda ()
	   (push '("TODO"  . ?▲) prettify-symbols-alist)
	   (push '("STARTED"  . ?⛿) prettify-symbols-alist)
	   (push '("WAITING"  . ?♾) prettify-symbols-alist)
	   (push '("SOMEDAY"  . ?☁) prettify-symbols-alist)
	   (push '("MAYBE"  . ?♢) prettify-symbols-alist)
	   (push '("DONE"  . ?✓) prettify-symbols-alist)
	   (push '("CANCELLED"  . ?✘) prettify-symbols-alist)
	   (push '("QUESTION"  . ?⚑) prettify-symbols-alist))))

(use-package org-cliplink
  :bind ("C-x p i" . org-cliplink))

(use-package org-journal
  :config
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-date-prefix "#+TITLE: Daily Notes ")
  (setq org-journal-dir my-org-journal-dir))

(define-key global-map (kbd "C-c j t")
  (lambda () (interactive) (org-capture nil "t")))
(define-key global-map (kbd "C-c j i")
  (lambda () (interactive) (org-capture nil "i")))
(define-key global-map (kbd "C-c j e")
  (lambda () (interactive) (org-capture nil "e")))
(define-key global-map (kbd "C-c j j")
  (lambda () (interactive) (org-capture nil "j")))
(define-key global-map (kbd "C-c j p")
  (lambda () (interactive) (org-capture nil "p")))
(define-key global-map (kbd "C-c j s")
  (lambda () (interactive) (org-capture nil "s")))


;; Custom functions - TODO move to custom.el
(defun jc/use-eslint-from-node-modules ()
    "Set local eslint if available."
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

(defun switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(bind-key "C-c a s" 'switch-to-scratch-buffer)

(setq confirm-nonexistent-file-or-buffer nil)

(defun create-non-existent-directory ()
  "Check whether a given file's parent directories exist; if they do not, offer to create them."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p? (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'create-non-existent-directory)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets which-key git-gutter counsel dap-mode omnisharp ## zenburn-theme labburn-theme elfeed yasnippet kaolin-themes yaml-mode wrap-region use-package treemacs-projectile treemacs-magit treemacs-icons-dired sql-indent smartparens realgud-lldb realgud-ipdb rainbow-delimiters racket-mode paradox neotree lsp-mode json-mode ivy highlight-escape-sequences flycheck fill-column-indicator expand-region exec-path-from-shell dockerfile-mode csv-mode cider captain all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; .emacs ends here
