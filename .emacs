(setq default-directory "~/projects")

;; Init the package facility
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

;; since refreshing packages is time-consuming and should be done on demand
;; (package-refresh-contents) ;; this line is commented 

;; Declare packages
(setq my-packages
      '(all-the-icons
	async
	captain
	company
	eglot
	evil
	expand-region
	exec-path-from-shell
	fill-column-indicator
	highlight-escape-sequences
	ivy
	kaolin-themes
        magit
	neotree
	org
	paradox
	projectile
	rainbow-delimiters
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

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 3               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 6               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; Custom mode hooks
(add-hook 'python-mode-hook #'smartparens-mode)
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
  (setq neo-window-width 45)
  (neotree-toggle)
  (neotree-hidden-file-toggle)
  )

;; After packages are installed and init is done initialize
;; last steps.
(add-hook 'after-init-hook
	  (global-company-mode)
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
 '(custom-safe-themes
   (quote
    ("f7b0f2d0f37846ef75157f5c8c159e6d610c3efcc507cbddec789c02e165c121" "3ee39fe8a6b6e0f1cbdfa33db1384bc778e3eff4118daa54af7965e9ab8243b3" "53993d7dc1db7619da530eb121aaae11c57eaf2a2d6476df4652e6f0bd1df740" "2f945b8cbfdd750aeb82c8afb3753ebf76a1c30c2b368d9d1f13ca3cc674c7bc" "58c2c8cc4473c5973e77f4b78a68c0978e68f1ddeb7a1eb34456fce8450be497" "0eb3c0868ff890b0c4ee138069ce2a8936a8a69ba150efa6bfb9fb7c05af5ec3" "ed573618e4c25fa441f12cbbb786fb56d918f216ae4a895ca1c74f34a19cfe67" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (terraform-mode kaolin-themes yaml-mode wrap-region use-package treemacs-projectile treemacs-magit treemacs-icons-dired sql-indent smartparens realgud-lldb realgud-ipdb rainbow-delimiters racket-mode paradox neotree lsp-mode json-mode ivy highlight-escape-sequences flycheck fill-column-indicator expand-region exec-path-from-shell dockerfile-mode csv-mode cider captain all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; .emacs ends here
