;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package rustic
  :ensure
  :init
  (setq rustic-treesitter-derive t)
  :custom
  ;; (rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer"))
  (rustic-analyzer-command (list (string-trim (shell-command-to-string "which rust-analyzer"))))
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-r l" . flycheck-list-errors)
              ("C-c C-r a" . lsp-execute-code-action)
              ("C-c C-r R" . lsp-rename)
              ("C-c C-r q" . lsp-workspace-restart)
              ("C-c C-r Q" . lsp-workspace-shutdown)
              ("C-c C-r s" . lsp-rust-analyzer-status)
              ("C-c C-r e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-r f" . rustic-format-buffer)
              ("C-c C-r h" . lsp-rust-analyzer-inlay-hints-mode)
              ("C-c C-r b" . rustic-cargo-build)
              ("C-c C-r c" . rustic-cargo-clean)
              ("C-c C-r r" . rustic-cargo-run)
              ("C-c C-r t" . rustic-cargo-test)
              ("C-c C-r y" . rustic-cargo-clippy)
              ;; ("C-c C-c d" . dap-hydra)
              ("C-c C-r g" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-lsp-format t)
  (setq rustic-format-trigger 'on-compile)
  (setq compilation-read-command nil) ;; not prompt on minibuffer when do compile.
  (push 'rustic-clippy flycheck-checkers)
  ;; (setq lsp-rust-analyzer-cargo-watch-enable nil)
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save nil)
  (setq rustic-lsp-format nil)

  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-lsp-client 'lsp-mode)
  ;;(setq rustic-lsp-client 'eglot)
  (defun rk/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
      (setq-local buffer-save-without-query t))
    ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
    )
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; Create / cleanup rust scratch projects quickly
(use-package rust-playground
  :hook ((rust-mode . rust-playground-mode))
  :custom (rust-playground-run-command "cargo run --color never")
  :commands (rust-playground-get-snippet-basedir)
  :config
  (add-hook 'conf-toml-mode 'rust-playground-mode)
  (setq rust-playground-basedir (expand-file-name "~/test/rust/playground"))
  )

;; copy from https://gitter.im/emacs-lsp/lsp-mode?at=5f7fea9824a20801a8d60649
(use-package rust-mode
  :ensure t
  :mode
  ("\\.rs\\'" . rust-mode)
  :hook
  (rust-mode . flycheck-mode)
  (rust-mode . lsp)
  (rust-mode . flycheck-rust-setup)
  :init (setq lsp-rust-server 'rust-analyzer)
  :config
  (setq rust-format-on-save t)
  ;;(setq lsp-completion-provider :capf)
  (setq lsp-progress-via-spinner t)
  ;;(require 'lsp-mode)
  ;; (add-hook 'rust-mode-hook (lambda ()
  ;;                             (flycheck-rust-setup)
  ;;                             (lsp)
  ;;                             (flycheck-mode)
  ;;                             (yas-minor-mode)
  ;;                             ))
  )

(use-package cargo
  :ensure t
  :commands cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(provide 'init-rust)
;;; init-rust ends here
