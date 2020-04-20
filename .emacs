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
	cider
	company
	csv-mode
	dockerfile-mode
	evil
	eglot
	expand-region
	exec-path-from-shell
	fill-column-indicator
	highlight-escape-sequences
        json-mode
	kaolin-themes
        magit
        markdown-mode
	neotree
	org
	projectile
	rainbow-delimiters
	sql-indent
	use-package
        yaml-mode
        ))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(setq default-directory (concat (getenv "HOME") "/projects"))

;; Custom mode hooks
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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
  (setq neo-smart-open t))

;; Enable built in line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Init windows
(defun my-layout()
  (interactive)
  (split-window-right)
  (other-window -1)
  (eshell)
  (neotree-toggle)
  )

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

;; After packages are installed and init is done initialize
;; last steps focus on layout.
(add-hook 'after-init-hook
	  (my-layout)
	  (evil-mode 1)
	  )
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-temple t))

(setq kaolin-themes-distinct-fringe t)  
(setq kaolin-themes-distinct-company-scrollbar t)
(setq kaolin-themes-git-gutter-solid t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (kaolin-themes yaml-mode wrap-region use-package treemacs-projectile treemacs-magit treemacs-icons-dired sql-indent smartparens realgud-lldb realgud-ipdb rainbow-delimiters racket-mode paradox neotree lsp-mode json-mode ivy highlight-escape-sequences flycheck fill-column-indicator expand-region exec-path-from-shell dockerfile-mode csv-mode cider captain all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (set-face-attribute 'default nil
		     :family "JetBrains Mono"
		     :height (+ (face-attribute 'default :height)
                         10))
  (set-face-attribute 'neo-button-face      nil :family "CozetteVector")
  (set-face-attribute 'neo-file-link-face   nil :family "CozetteVector")
  (set-face-attribute 'neo-dir-link-face    nil :family "CozetteVector")
  (set-face-attribute 'neo-header-face      nil :family "CozetteVector")
  (set-face-attribute 'neo-expand-btn-face  nil :family "CozetteVector")
 )
;; .emacs ends here
