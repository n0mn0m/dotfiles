;;; start config

(setq default-directory (getenv "HOME"))

(setenv "ESHELL" (expand-file-name "~/bin/eshell"))

;; Enable built in line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Configure backups
(defvar --backup-directory (concat (getenv "HOME") "/.emacs.d/backups"))

(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))

(setq backup-directory-alist `(("." . ,--backup-directory)))

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

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-visual-line-mode 1)
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Init the package facility
(require 'package)
(package-initialize)

(setq load-prefer-newer t)

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
    (setq my-packages
	  '(auto-package-update
            all-the-icons
	    async
	    cargo
	    company
	    counsel
	    csharp-mode
	    csv-mode
	    dockerfile-mode
            eglot
	    elfeed
	    evil
	    expand-region
	    exec-path-from-shell
	    fill-column-indicator
	    flycheck
            flycheck-rust
            flycheck-swift
            fsharp-mode
	    highlight-escape-sequences
	    ivy
	    ivy-hydra
	    json-mode
	    kaolin-themes
	    magit
	    markdown-mode
	    neotree
	    org
	    omnisharp
	    prescient
	    projectile
            python-mode
            racket-mode
	    rainbow-delimiters
	    rust-mode
            soft-morning-theme
	    sql-indent
            swift-helpful
            swift-mode
            swift-playground-mode
	    toml-mode
            typescript-mode
	    use-package
	    yaml-mode
	    yasnippet
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

(use-package company
  :init
    (add-hook 'after-init-hook 'global-company-mode))

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

(use-package cargo
  :hook ((rust-mode toml-mode) . cargo-minor-mode))

(use-package csharp-mode
  :mode "\\.cs\\'"
  :init
    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-to-list 'company-backends 'company-omnisharp)
    (add-hook 'csharp-mode-hook 'company-mode))

(use-package python-mode
  :mode "\\.py\\'")

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  :config (setq rust-format-on-save t))

(use-package racket-mode
  :mode "\\.rkt\\'"
  :init
  (setq font-lock-maximum-decoration 3)
  (add-hook 'racket-mode-hook
            (lambda ()
              (define-key racket-mode-map (kbd "<f5>") 'racket-run))))

(use-package swift-mode
  :mode "\\.swift\\'"
  :init
  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS14.2.sdk")
  (setq flycheck-swift-target "arm64-apple-ios14")
  (eval-after-load 'flycheck '(flycheck-swift-setup)))

(use-package swift-playground-mode
  :defer t :init
  (autoload 'swift-playground-global-mode "swift-playground-mode" nil t)
  (add-hook 'swift-mode-hook #'swift-playground-global-mode))

(use-package toml-mode
  :mode "\\.toml\\'")

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


(use-package elfeed
  :init
    (setq elfeed-feeds
	  '("http://planet.emacsen.org/atom.xml"
	    "https://blog.acolyer.org/feed/"
	    "http://worrydream.com/feed.xml"
	    "https://lobste.rs/rss")))

(use-package yasnippet
      :config
      (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
      (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

;; Custom functions
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
