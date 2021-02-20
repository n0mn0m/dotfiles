;;; emacs --- base config
;;; Commentary:
;;; My custom config, package management, mode config and functions.

;;; Code:
(setq user-full-name "Alexander Hagerman"
      user-mail-address "alexander@burningdaylight.io")

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

;; Instead of having custom defined at the end of files place it in one location
(setq custom-file "~/.emacs.d/cust/custom.el")
(load custom-file)

;; Begin loading packages and custom configurations
(org-babel-load-file (expand-file-name "~/.emacs.d/cust/global-config.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/cust/package-config.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/cust/org-config.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/cust/func-config.org"))

;; Load custom lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

