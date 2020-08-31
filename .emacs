;; Init the package facility
(require 'package)
(package-initialize)

;; first, declare repositories
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
        ))

(when (not package-archive-contents)
    (package-refresh-contents))

;; Declare packages
(setq my-packages
      '(all-the-icons
	async
	cargo
	company
	counsel
	csharp-mode
	csv-mode
	dap-mode
	dockerfile-mode
	eglot
	elfeed
	evil
	expand-region
	exec-path-from-shell
	fill-column-indicator
	flycheck
	highlight-escape-sequences
        json-mode
	kaolin-themes
        magit
        markdown-mode
	neotree
	org
	omnisharp
	projectile
	rainbow-delimiters
	rust-mode
	sql-indent
	toml-mode
	use-package
        yaml-mode
	yasnippet
        ))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

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

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package rainbow-delimiters
  :ensure t
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package expand-region
  :ensure t
  :config
    (global-set-key (kbd "C-=") 'er/expand-region))

(use-package kaolin-themes
  :ensure t
  :init
    (setq kaolin-themes-distinct-fringe t)
    (setq kaolin-themes-distinct-company-scrollbar t)
    (setq kaolin-themes-git-gutter-solid t)
  :config
    (load-theme 'kaolin-light t))

;; load evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
    (setq evil-search-module 'evil-search)
    (setq evil-ex-complete-emacs-commands nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (setq evil-shift-round nil)
    (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
    (evil-mode))

(use-package all-the-icons
  :ensure t)

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
  (setq neo-smart-open t))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :init
    (ivy-mode 1)
  :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 20)
    (setq ivy-count-format "%d/%d "))

(use-package company
  :ensure t
  :init
    (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :ensure t
  :init
    (global-flycheck-mode))

(use-package eglot
  :ensure t
  :demand)

(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'"
  :init
    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-to-list 'company-backends 'company-omnisharp)
    (add-hook 'csharp-mode-hook 'company-mode))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'eglot-ensure)
  :config (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook ((rust-mode toml-mode) . cargo-minor-mode))

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package elfeed
  :ensure t
  :init
    (setq elfeed-feeds
	  '("http://planet.emacsen.org/atom.xml"
	    "https://blog.acolyer.org/feed/"
	    "http://worrydream.com/feed.xml"
	    "https://lobste.rs/rss")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel dap-mode omnisharp ## zenburn-theme labburn-theme elfeed yasnippet kaolin-themes yaml-mode wrap-region use-package treemacs-projectile treemacs-magit treemacs-icons-dired sql-indent smartparens realgud-lldb realgud-ipdb rainbow-delimiters racket-mode paradox neotree lsp-mode json-mode ivy highlight-escape-sequences flycheck fill-column-indicator expand-region exec-path-from-shell dockerfile-mode csv-mode cider captain all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; .emacs ends here

