;; first, declare repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Init the package facility
(require 'package)
(package-initialize)

;; (package-refresh-contents) ;; this line is commented 
;; since refreshing packages is time-consuming and should be done on demand

;; Declare packages
(setq my-packages
      '(all-the-icons
	async
	captain
	cider
	company
	csv-mode
	dockerfile-mode
	evil
	expand-region
	exec-path-from-shell
	fill-column-indicator
	flycheck
	highlight-escape-sequences
	ivy
        json-mode
	kaolin-themes
	lsp-mode
	lsp-ui
	lsp-ivy
        magit
        markdown-mode
	neotree
	org
	paradox
	projectile
	rainbow-delimiters
	realgud-ipdb
	realgud-lldb
	smartparens
	sql-indent
	undo-tree
	use-package
        wrap-region
	yasnippet
        yaml-mode
        ))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))


;; Custom mode hooks
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook
   (lambda ()
     (setq captain-predicate (lambda () (nth 8 (syntax-ppss (point)))))))

(add-hook 'text-mode-hook
          (lambda ()
            (setq captain-predicate (lambda () t))))

(add-hook
 'org-mode-hook
 (lambda ()
   (setq captain-predicate
         (lambda () (not (org-in-src-block-p))))))

(add-hook 'prog-mode-hook #'lsp-deferred)

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (prog-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-indentation nil)
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

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

;; After packages are installed and init is done initialize
;; last steps.
(add-hook 'after-init-hook
	  (global-flycheck-mode)
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
