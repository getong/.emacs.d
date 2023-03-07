;;; -*- coding: utf-8; lexical-binding: t -*-
;; copy from https://devbins.github.io/post/emacs_flutter/
;; 编写好代码之后按下 M-x 输入 flutter-run 或 flutter-run-or-hot-reload 运行项目
;; 也可以使用快捷键 C-M-x 来启动项目。
;; 项目启动后会开启一个 *Flutter* 的 buffer ，在这里你可以做如下操作
;; r Hot reload. 🔥🔥🔥
;; R Hot restart.
;; h List all available interactive commands.
;; d Detach (terminate “flutter run” but leave application running).
;; c Clear the screen
;; q Quit (terminate the application on the device).
(use-package flutter
  :ensure t
  :after dart-mode
  :config
  (defun my/flutter-goto-logs-buffer()
    "Go to buffer logs buffer."
    (interactive)
    (let ((buffer (get-buffer flutter-buffer-name)))
      (unless buffer
        (user-error "flutter is not running."))
      (switch-to-buffer buffer)
      (goto-line (point-max))))
  :bind (:map dart-mode-map
              ("C-c C-r" . #'flutter-run-or-hot-reload)
              ("C-c C-l" . #'my/flutter-goto-logs-buffer))
  :hook (dart-mode . flutter-test-mode)
  ;; :custom
  ;; ;; sdk path will be the parent-parent directory of flutter cli
  ;; (flutter-sdk-path (directory-file-name
  ;;                    (file-name-directory
  ;;                     (directory-file-name
  ;;                      (file-name-directory (file-truename (executable-find "flutter")))))))
  )


(use-package flutter-l10n-flycheck
  :after flutter
  :config (flutter-l10n-flycheck-setup))

(use-package dart-mode
  :ensure t
  :mode
  ("\\.dart\\'" . dart-mode)
  :defines (projectile-project-root-files-bottom-up)
  :if (or (executable-find "dart") (executable-find "flutter"))
  :bind (:map dart-mode-map
              ("C-c C-f" . dart-format-buffer)
              ("C-c C-c" . my/dart-run-file))
  :config
  (defun my/dart-run-file ()
    "Execute the code of the current file."
    (interactive)
    (compile (format "dart %s" (buffer-file-name))))
  (with-eval-after-load "projectile"
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD")
    (add-to-list 'projectile-globally-ignored-file-suffixes "inject.dart")
    (add-to-list 'projectile-globally-ignored-file-suffixes "inject.summary")
    )
  :hook
  ((dart-mode . flutter-test-mode)
   (dart-mode . lsp-mode)
   )
  )

(provide 'init-flutter)
;;; init-flutter.el ends here
