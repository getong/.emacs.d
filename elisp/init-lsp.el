;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package lsp-lua
  :ensure lsp-mode

  :hook
  (lua-mode . lsp)

  :custom
  (lsp-lua-hint-enable t)
  (lsp-lua-telemetry-enable nil)

  :config
  (setq lsp-lua-workspace-max-preload 8192
        lsp-lua-workspace-preload-file-size 1024
        ;; copy from https://github.com/emacs-lsp/lsp-mode/issues/2818
        ;; lsp-lua-workspace-library (ht ("~/vbox_share/cpp_client/game/Debug_x86"  t))
        ;; copy from https://emacs-china.org/t/doom-emacs-lsp-lua-mode/16432/7
        ;; lua
        ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-lua-language-server/
        lsp-clients-lua-language-server-install-dir (substring (file-name-directory (file-truename (executable-find "lua-language-server"))) 0 -4)
        lsp-clients-lua-language-server-bin (f-join lsp-clients-lua-language-server-install-dir "bin/lua-language-server")
        lsp-clients-lua-language-server-main-location (f-join lsp-clients-lua-language-server-install-dir "libexec/main.lua")
        )
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-lua-language-server-bin)
                    :major-modes '(lua-mode)
                    :server-id 'lua-langserver
                    :remote? nil
                    :priority -1
                    :notification-handlers
                    (lsp-ht
                     ("emmy/progressReport" 'ignore))
                    ))
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-stdio-connection '("python3" "-m" "ffi_navigator.langserver"))
  ;;   :major-modes '(lua-mode python-mode c++-mode)
  ;;   :server-id 'ffi-navigator
  ;;   :add-on? t))
  )

;; copy fromhttps://devbins.github.io/post/emacs_flutter/
(use-package lsp-dart
  :ensure t
  ;; :after dap-mode
  :config
  (defun find-path-by-executable (exec)
    (when-let (path (executable-find exec))
      (file-name-directory
       (directory-file-name
        (file-name-directory
         (file-chase-links path))))))
  (dap-register-debug-template "Flutter :: Custom debug"
                               (list :flutterPlatform "x86_64"
                                     :program "lib/main_debug.dart"
                                     :args '("--flavor" "customer_a")))

  ;; (setq lsp-dart-sdk-dir (concat (file-name-directory (file-truename (executable-find "flutter"))) "cache/dart-sdk"))
  ;; copy from https://jwill.dev/blog/2022/01/24/EmacsAsIDE-Flutter.html
  ;; copy from https://github.com/thiagokokada/dotfiles/blob/28ba3b683c1c9cda5b0d98d0ca7505be188a9e02/doom-emacs/.config/doom/config.el#L181
  :custom
  (setq
   ;; lsp-dart-sdk-dir (find-path-by-executable "dart")
   lsp-dart-sdk-dir (concat (find-path-by-executable "dart") "bin/cache/dart-sdk")
   lsp-flutter-sdk-dir (find-path-by-executable "flutter")
   lsp-dart-flutter-sdk (find-path-by-executable "flutter")
   flutter-sdk-path (find-path-by-executable "flutter")
   )

  :hook (dart-mode . lsp))

(use-package lsp-treemacs
  :ensure t)

;; lsp-workspace-restart switching lsp-mode on and off
;; lsp-describe-session will print a nice tree showing what’s running and which buffers are connected with which backends.
;; lsp-version should return the version of lsp-mode
;; lsp-doctor is a diagnostic convenience functions
;; copy from https://nyk.ma/posts/emacs-write-your-own/
;; 由于 lsp-mode 的一次大更新，把使用方式变成如今调用 (lsp) 即可，所以目前 eglot 竞争力不强了。
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init ;; 在 (reuqire) 之前执行
  (setq
   ;; 尝试自动配置自己
   lsp-auto-configure t
   ;; 尝试自动猜测项目根文件夹
   lsp-auto-guess-root t
   ;; 多少时间idle后向服务器刷新信息
   lsp-idle-delay 0.500
   lsp-server-install-dir (no-littering-expand-var-file-name "lsp/")
   ;; 给缓存文件换一个位置
   lsp-session-file (concat lsp-server-install-dir "lsp-session-v1")
   lsp-eslint-library-choices-file (concat lsp-server-install-dir "lsp-eslint-choices")
   lsp-yaml-schema-store-local-db (concat lsp-server-install-dir "lsp-yaml-schemas.json")
   ;; lsp-vetur-global-snippets-dir (no-littering-expand-etc-file-name "yasnippet/snippets/vetur")
   )
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  ;;(lsp-eldoc-render-all t)
  (lsp-enable-snippet nil)
  ;; Auto-kill LSP server once you've killed the last buffer associated with its
  ;; project.
  (lsp-keep-workspace-alive nil)
  (lsp-enable-xref t)
  (lsp-enable-imenu t)
  (lsp-enable-completion-at-point t)
  (lsp-enable-suggest-server-download t)
  ;; copy from https://github.com/emacs-lsp/lsp-mode/issues/3231
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-headerline-breadcrumb-segments '(file symbols))
  (lsp-modeline-diagnostics-enable  t)
  (lsp-modeline-diagnostics-scope  :project)
  ;; (lsp-completion-provider  :capf)
  ;; Disable "use" statement insertion.
  (lsp-intelephense-completion-insert-use-declaration nil)
  ;; Decrease completion suggestions to 25.
  (lsp-intelephense-completion-max-items 25)
  ;; Disable breadcrumbs for all modes.
  (lsp-headerline-breadcrumb-enable nil)
  ;; Setup licence key.
  (lsp-intelephense-licence-key "KEY-GOES-HERE")
  ;; Disable multi-root server.
  (lsp-intelephense-multi-root nil)
  (lsp-intelephense-storage-path (no-littering-expand-var-file-name "lsp-cache"))
  (lsp-intelephense-global-storage-path (no-littering-expand-var-file-name "intelephense"))
  ;; Disable telemetry.
  (lsp-intelephense-telemetry-enabled nil)
  ;; Show verbose output from the intelephense server.
  (lsp-intelephense-trace-server "verbose")
  ;; Add "exclude_me" directory to list of excluded directories.
  ;; (setq lsp-intelephense-files-exclude
  ;; (vconcat ["**/exclude_me/**"] lsp-intelephense-files-exclude))
  ;; Reduce max file size to 100kb
  (lsp-intelephense-files-max-size 100000)
  :hook
  ;; (php-mode . lsp)
  ;; 在哪些语言 major mode 下启用 LSP
  ((c-mode
    c++-mode
    python-mode
    sh-mode
    rust-mode
    rustic-mode
    php-mode
    lua-mode
    html-mode
    json-mode
    dockerfile-mode
    css-mode
    yaml-mode
    typescript-mode
    go-mode) .
    (lambda ()
      (hack-local-variables)
      (lsp)
      (which-function-mode)
      ))
  (lsp-mode . lsp-enable-which-key-integration)
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3055
  ;; (lsp-mode . (lambda () (mapc (lambda (client) (setf (lsp-client-download-server-fn client) nil))
  ;;                              (ht-values lsp-clients))))
  :config

  ;; (setq company-minimum-prefix-length 1
  ;;       company-idle-delay 0.500)
  ;; default is 0.2
  (setq lsp-completion-provider :none) ;; 阻止 lsp 重新设置 company-backend 而覆盖我们 yasnippet 的设置
  (setq lsp-headerline-breadcrumb-enable t)

  (setq lsp-enable-file-watchers nil)
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  (setq lsp-print-performance t)
  (setq lsp-auto-guess-root t) ; auto detect workspace and start lang server
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

(use-package lsp-ui
  :ensure t
  ;; 仅在某软件包被加载后再加载
  :after (lsp-mode)
  ;; :requires use-package-hydra
  :commands lsp-ui-mode
  :custom-face
  ;; (lsp-ui-doc-background :inherit 'tooltip)
  ;; (lsp-ui-peek-filename :inherit 'mode-line-buffer-id)
  ;; (lsp-ui-peek-header :foreground fg :background (doom-lighten bg 0.1) :bold bold)
  ;; (lsp-ui-peek-selection :foreground bg :background blue :bold bold)
  ;; (lsp-ui-peek-list :background (doom-darken bg 0.1))
  ;; (lsp-ui-peek-peek :background (doom-darken bg 0.1))
  ;; (lsp-ui-peek-highlight :inherit 'lsp-ui-peek-header :background region :foreground bg :box t)
  ;; (lsp-ui-peek-line-number :foreground success)
  ;; (lsp-ui-sideline-code-action :foreground (doom-blend highlight bg 0.85))
  ;; (lsp-ui-sideline-current-symbol :inherit 'highlight)
  ;; (lsp-ui-sideline-symbol-info :foreground (doom-blend comments bg 0.85)
  ;;                              :background bg-alt :extend t)
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  (lsp-ui-peek-highlight ((t (:inherit nil :background nil :foreground nil :weight semi-bold :box (:line-width -1)))))
  :bind
  (:map lsp-ui-mode-map
        ;; 查询符号定义：使用 LSP 来查询。通常是 M-.
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ;; 查询符号引用：使用 LSP 来查询。通常是 M-?
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ;; 该文件里的符号列表：类、方法、变量等。前提是语言服务支持本功能。
        ("C-c u" . lsp-ui-imenu))
  ;; 当 lsp 被激活时自动激活 lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; copy from [A guide on disabling/enabling lsp-mode features](https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/)
  (setq
   ;; Symbol highlighting
   lsp-enable-symbol-highlighting t
   lsp-print-io nil
   ;; lsp-prefer-flymake :none
   flycheck-checker-error-threshold 10000
   lsp-ui-flycheck-enable t
   lsp-ui-flycheck-list-position 'right
   lsp-ui-flycheck-live-reporting t
   lsp-ui-peek-enable t
   lsp-ui-peek-list-width 60
   lsp-ui-peek-peek-height 25
   lsp-ui-imenu-enable t
   ;; you could manually request them via `lsp-signature-activate`
   lsp-signature-auto-activate nil
   ;; Signature help documentation (keep the signatures)
   lsp-signature-render-documentation nil
   ;; lsp-ui-doc - on hover dialogs. * disable via
   lsp-ui-doc-enable t
   ;; Lenses
   lsp-lens-enable t
   ;; Headerline
   lsp-headerline-breadcrumb-enable t
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-show-with-mouse nil
   ;; Sideline code actions * disable whole sideline via
   lsp-ui-sideline-enable t
   lsp-ui-sideline-show-code-actions t
   ;; hover symbols
   lsp-ui-sideline-show-hover t
   lsp-completion-show-detail t
   lsp-completion-show-kind t
   lsp-ui-sideline-show-diagnostics t
   lsp-modeline-code-actions-enable nil
   lsp-eldoc-enable-hover nil
   lsp-modeline-diagnostics-enable nil
   lsp-ui-peek-always-show t
   lsp-ui-sideline-ignore-duplicate t)
  )

 ;;; lsp-rust
(use-package lsp-rust
  :ensure lsp-mode
  :defer t
  :custom
  ;; copy from https://fasterthanli.me/articles/the-bottom-emoji-breaks-rust-analyzer
  (lsp-rust-analyzer-server-command (list (string-trim (shell-command-to-string "rustup which rust-analyzer"))))
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-chaining-hints t)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       `(,(or (executable-find
                               (cl-first lsp-rust-analyzer-server-command))
                              (lsp-package-path 'rust-analyzer)
                              "rust-analyzer")
                         ,@(cl-rest lsp-rust-analyzer-server-args))))
    :remote? nil
    :major-modes '(rust-mode rustic-mode)
    :initialization-options 'lsp-rust-analyzer--make-init-options
    :notification-handlers (ht<-alist lsp-rust-notification-handlers)
    :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
    :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
    :after-open-fn (lambda ()
                     (when lsp-rust-analyzer-server-display-inlay-hints
                       (lsp-rust-analyzer-inlay-hints-mode)))
    :ignore-messages nil
    :priority -1
    :server-id 'rust-analyzer-remote))
  )

(use-package lsp-docker
  :ensure t)

;; Julia
(use-package lsp-julia
  :after (lsp-mode julia-mode)
  (add-hook 'julia-mode-hook #'lsp-julia-enable)
  :config
  (setq
   lsp-julia-command "julia"
   lsp-julia-package-dir "@emacs-lspconfig"
   lsp-julia-flags `(,(concat "--project=" lsp-julia-package-dir)
                     "--startup-file=no"
                     "--history-file=no"
                     ,(concat "-J" (getenv "HOME") "/.julia/environments/emacs-lspconfig/languageserver.so"))
   lsp-julia-default-environment (shell-command-to-string "julia --startup-file=no --history-file=no -e 'print(dirname(Base.active_project()))'")))

(use-package lsp-sourcekit
  :ensure t
  :when (eq system-type 'darwin)
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp")))
  )

;; lsp python server
(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3"))
  :config
  (setq lsp-pyright-disable-organize-imports t
        lsp-pyright-log-level "error")
  )

;; lsp erlang server
(use-package lsp-erlang
  :ensure lsp-mode
  :config
  (setq lsp-erlang-server-path (no-littering-expand-var-file-name "erlang_ls/bin/erlang_ls"))
  )

(use-package lsp-bash
  :ensure lsp-mode
  :hook
  (sh-mode-hook . lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("bash-language-server" "start"))
                    :major-modes '(sh-mode)
                    :remote? nil
                    :server-id 'bash-ls))
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
