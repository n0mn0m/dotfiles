;;; start config

(setq default-directory (getenv "HOME"))
(setenv "ESHELL" (expand-file-name "~/bin/eshell"))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-display-line-numbers-mode)
(global-visual-line-mode 1)
(column-number-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Configure encoding
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

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

(setq-default
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

(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Init the package facility
(require 'package)
(package-initialize)
(setq load-prefer-newer t)
(custom-set-variables '(use-package-verbose nil))

(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("gnu" . "https://elpa.gnu.org/packages/")
    ))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

;; first, declare repositories
(use-package package
  :init
    (package-refresh-contents)
    ;; Declare packages
    (defvar my-packages)
    (setq my-packages
	  '(auto-package-update
            all-the-icons
	    async
	    cargo
	    company
	    counsel
	    csv-mode
	    dockerfile-mode
            eglot
	    evil
	    expand-region
	    exec-path-from-shell
	    fill-column-indicator
	    flycheck
            flycheck-aspell
            flycheck-inline
            flycheck-projectile
            flycheck-rust
            flycheck-yamllint
            ggtags
	    highlight-escape-sequences
	    ivy
	    ivy-hydra
	    json-mode
	    kaolin-themes
            keychain-environment
	    magit
	    markdown-mode
	    neotree
	    org
	    prescient
	    projectile
            racket-mode
	    rainbow-delimiters
	    rust-mode
	    sql-indent
	    toml-mode
            typescript-mode
	    use-package
            which-key
	    yaml-mode
	    yasnippet
            yasnippet-snippets
	    ))

    ;; Iterate on packages and install missing ones
    (dolist (pkg my-packages)
      (unless (package-installed-p pkg)
	(package-install pkg))))

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4
	 use-package-always-ensure t)
   (auto-package-update-maybe))

(use-package auto-compile
  :defer nil
  :config (auto-compile-on-load-mode))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(use-package rainbow-delimiters
  :init
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package expand-region
  :config
    (global-set-key (kbd "C-=") 'er/expand-region))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package kaolin-themes
  :init
    (setq kaolin-themes-distinct-fringe t)
    (setq kaolin-themes-distinct-company-scrollbar t)
    (setq kaolin-themes-git-gutter-solid t)
  :config
    (load-theme 'kaolin-breeze t))

;; load evil
(use-package evil
  :init ;; tweak evil's configuration before loading it
    (setq evil-search-module 'evil-search)
    (setq evil-ex-complete-emacs-commands nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (setq evil-shift-round nil)
    (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
    (evil-mode))

(use-package all-the-icons)

;; Neotree config
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

(use-package ivy
  :diminish (ivy-mode . "")
  :init
    (ivy-mode 1)
  :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 20)
    (setq ivy-count-format "%d/%d "))

(use-package prescient)

(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

(use-package which-key
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  (which-key-idle-delay 1.2)
  :config
  (which-key-setup-minibuffer))

(use-package company
  :init
    (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :config
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (add-to-list 'flycheck-checkers 'proselint 'tex-aspell-dynamic)
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

(use-package flycheck-aspell
  :config
  (setq ispell-dictionary "some_dictionary")
  (setq ispell-program-name "aspell")
  (setq ispell-silently-savep t))

(use-package flycheck-inline
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))))

(use-package flycheck-yamllint
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

(use-package dash-at-point
  :bind ("C-c d" . dash-at-point))

(use-package ggtags
  :config
    (add-hook 'c-mode-common-hook
    (lambda ()
    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
          (ggtags-mode 1)))))

(use-package cargo
  :hook ((rust-mode toml-mode) . cargo-minor-mode))

(add-hook 'python-mode (lambda () 
  (setq-local
    python-shell-interpreter "ipython"
    python-shell-interpreter-args "--colors=Linux --profile=default"
    python-shell-prompt-regexp "In \\[[0-9]+\\]: "
    python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
    python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
    python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
    python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  :config (setq rust-format-on-save t))

(use-package racket-mode
  :mode "\\.rkt\\'"
  :init
  (setq font-lock-maximum-decoration 3)
  (add-hook 'racket-mode-hook
            (lambda ()
              (define-key racket-mode-map (kbd "<f5>") 'racket-run))))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((python-mode . eglot-ensure)
         (racket-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rls")))
  (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server"))))

(use-package yasnippet
      :config
      (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
      (yas-global-mode 1))

(use-package projectile
  :diminish
  :bind (("C-c k" . 'projectile-kill-buffers)
  ("C-c M" . 'projectile-compile-project))
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode))

;; Org
(use-package org
  :hook ((org-mode . visual-line-mode))
  :custom
  (org-directory "~/Library/Mobile Documents/com\~apple\~CloudDocs/beorg/org")
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
     (replace-regexp-in-string "_" "," with-underscores))))

;; Custom functions
(defun switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(bind-key "C-c a s" 'switch-to-scratch-buffer)

(set-face-attribute 'default nil
		     :family "JetBrains Mono"
		     :height 160)

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
