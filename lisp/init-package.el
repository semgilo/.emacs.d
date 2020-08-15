;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

(require 'package)

;; Make "site-lisp" if it is not exists.
(setq site-lisp-path (expand-file-name "site-lisp" user-emacs-directory))
(unless (file-exists-p site-lisp-path)
  (make-directory site-lisp-path))

;; Set ELPA packages
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '( "gnu" . "https://elpa.gnu.org/packages/") t)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Initialization benchmark
(use-package benchmark-init
  :defines swiper-font-lock-exclude
  :commands (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate)
  :init (benchmark-init/activate)
  :config
  (with-eval-after-load 'swiper
    (add-to-list 'swiper-font-lock-exclude 'benchmark-init/tree-mode)))

(provide 'init-package)

;;; init-package.el ends here
