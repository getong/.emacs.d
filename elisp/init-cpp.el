;;; -*- coding: utf-8; lexical-binding: t -*-

;; C/C++
(use-package cc-mode
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  ;; TODO: open these hooks.
  ;; Sadly I work with a dirty team, i will change the whole code base if
  ;; uncommend config below.
  ;; (before-save-hook . lsp-format-buffer)
  ;; (before-save-hook . lsp-organize-imports)
  :config
  (define-key c-mode-base-map (kbd "M-/") 'ff-find-related-file)
  ;; Open a header file in C++ mode by defaults
  ;;(add-auto-mode 'c++-mode "\\.h\\'")
  )

(use-package cmake-mode
  :init
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))


;; C++20 highlighting
(use-package modern-cpp-font-lock
  :diminish nil
  :hook
  (c++-mode . modern-c++-font-lock-mode)
  (modern-c++-font-lock-mode . (lambda () (diminish
                                           'modern-c++-font-lock-mode))))

;; C/C++/Objective-C
(use-package ccls
  :disabled t
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json" ".ccls")
                  projectile-project-root-files-top-down-recurring)))
  (with-no-warnings
    ;; FIXME: fail to call ccls.xref
    ;; @see https://github.com/emacs-lsp/emacs-ccls/issues/109
    (cl-defmethod my-lsp-execute-command
      ((_server (eql ccls)) (command (eql ccls.xref)) arguments)
      (when-let ((xrefs (lsp--locations-to-xref-items
                         (lsp--send-execute-command (symbol-name command) arguments))))
        (xref--show-xrefs xrefs nil)))
    (advice-add #'lsp-execute-command :override #'my-lsp-execute-command)))

(provide 'init-cpp)
;;; init-cpp ends here
