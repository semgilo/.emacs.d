;; init-lua

;; company
(use-package company-lua
  :ensure t
  :after company
  :config)

(use-package counsel-etags
  :ensure t
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

;; set company bachends
(defun set-company-backends-for-lua()
  "Set lua company backend."
  (setq-local company-backends '(
                                 (
                                  company-lua
                                  ;; company-lsp
                                  company-yasnippet
                                  company-keywords
                                  )

                                 company-gtags
                                 company-capf
                                 company-dabbrev-code
                                 company-files
                                 )))


(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua"
  :hook (
         ;; (lua-mode . lsp-mode)
         (lua-mode . set-company-backends-for-lua))
  :config
  (setq lua-indent-level 4)
  (setq lua-indent-string-contents t)
  (setq lua-prefix-key nil)
  )

;; (use-package lsp-lua-emmy
;;   :demand
;;   :ensure nil
;;   :load-path "~/.emacs.d/site-lisp/lsp-lua-emmy"
;;   :hook (lua-mode . lsp)
;;   :config
;;   (setq lsp-lua-emmy-jar-path (expand-file-name "3rd/EmmyLua-LS-all.jar" user-emacs-directory))
;;   )

(provide 'init-lua)
