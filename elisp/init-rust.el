;; copy from https://github.com/rksm/emacs-rust-config
;; When using this directly, you will need to have use-package installed:
;; M-x package-install, select use-package. But if you start via
;; `standalone.el', this is being taken care of automatically.


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

(use-package rustic
  :ensure
  :init
  (setq rustic-treesitter-derive t)
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-lsp-format t)
  (setq rustic-format-trigger 'on-compile)
  (setq compilation-read-command nil) ;; not prompt on minibuffer when do compile.
  (push 'rustic-clippy flycheck-checkers)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")

  ;; comment to disable rustfmt on save
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-lsp-client 'lsp-mode)
  ;;(setq rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;;(use-package eglot
;;  :ensure t
;;  :config
;;  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
;;  (add-hook 'rustic-mode-hook 'eglot-ensure)
;;  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package flycheck-rust :ensure)
(use-package flycheck :ensure)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (setq lsp-enable-file-watchers nil)
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-auto-guess-root nil)
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure
  :config

  (global-company-mode 1)
  (setq company-idle-delay 0.01)
  (setq company-minimum-prefix-length 1)
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  ;;(:map company-mode-map
  ;;      ("<tab>". tab-indent-or-complete)
  ;;      ("TAB". tab-indent-or-complete))
  )

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Create / cleanup rust scratch projects quickly

(use-package rust-playground :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	       :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
(add-hook 'rust-mode-hook (lambda ()
                            (flycheck-rust-setup)
                            (lsp)
                            (flycheck-mode)
			                (yas-minor-mode)
                            ))

;;(require 'rust-mode)
(use-package rust-mode :ensure)
;;(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;;(setq company-tooltip-align-annotations t)

;; copy from https://zenn.dev/yukit/articles/25a88b33a35633
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (add-to-list 'exec-path (expand-file-name  "~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin"))
    (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
    ))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (add-to-list 'exec-path (expand-file-name  "~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin"))
    (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
    (setq rustic-analyzer-command '("~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/rust-analyzer"))
    (setq lsp-rust-analyzer-server-command '("~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/rust-analyzer"))
    ))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (add-to-list 'exec-path (expand-file-name "/backup/backup/rust_installation/cargo/bin"))
    (add-to-list 'exec-path (expand-file-name "/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-apple-darwin/bin"))
    (setq rustic-analyzer-command '("/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))
    (setq lsp-rust-analyzer-server-command '("/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))
    )))

;; copy from [Rust development environment for Emacs](https://rustrepo.com/repo/brotzeit-rustic-rust-ides)
(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))
(add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

(with-eval-after-load "lsp-rust"
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       `(,(or (executable-find
                               (cl-first lsp-rust-analyzer-server-command))
                              (lsp-package-path 'rust-analyzer)
                              "rust-analyzer")
                         ,@(cl-rest lsp-rust-analyzer-server-args))))
    :remote? t
    :major-modes '(rust-mode rustic-mode)
    :initialization-options 'lsp-rust-analyzer--make-init-options
    :notification-handlers (ht<-alist lsp-rust-notification-handlers)
    :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
    :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
    :after-open-fn (lambda ()
                     (when lsp-rust-analyzer-server-display-inlay-hints
                       (lsp-rust-analyzer-inlay-hints-mode)))
    :ignore-messages nil
    :server-id 'rust-analyzer-remote)))

(provide 'init-rust)
