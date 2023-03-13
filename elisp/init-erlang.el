;;; init-erlang.el --- Summary init erlang -*- lexical-binding: t -*-

;;; Commentary:
;; init erlang

;;; Code:

(use-package erlang
  :if (executable-find "erl")
  :mode (("\\.erl\\'" . erlang-mode))
  :defer

  :hook
  (erlang-mode . lsp)
  )

;; elixir
(use-package elixir-mode
  :ensure t)

;; disable company
;; (use-package alchemist
;; :ensure t)

(provide 'init-erlang)
;;; init-erlang.el ends here
