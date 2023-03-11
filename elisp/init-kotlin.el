;;; init-kotlin.el --- Summary kotlin file -*- lexical-binding: t -*-

;;; Commentary:
;; kotlin

;;; Code:

(use-package kotlin-mode
  :after (lsp-mode dap-mode)
  :config
  (require 'dap-kotlin)
  ;; should probably have been in dap-kotlin instead of lsp-kotlin
  (setq lsp-kotlin-debug-adapter-path (or (executable-find "kotlin-debug-adapter") ""))
  :hook
  (kotlin-mode . lsp))

(provide 'init-kotlin)
;;; init-kotlin.el ends here
