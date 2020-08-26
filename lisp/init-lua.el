;; init-lua

;; company
(use-package company-lua
  :ensure t
  :after company
  :config)

;; set company bachends
(defun set-company-backends-for-lua()
  "Set lua company backend."
  (setq-local company-backends '(
                                 (
                                  company-lua
                                  company-lsp
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
  :hook (
         (lua-mode . set-company-backends-for-lua)
         (lua-mode . lsp))
  :config
  (setq lua-indent-level 4)
  (setq lua-indent-string-contents t)
  (setq lua-prefix-key nil)
  )
(provide 'init-lua)
