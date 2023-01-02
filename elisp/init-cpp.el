;; copy from https://quant67.com/post/emcas/init-config.html
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

(use-package diminish
  :ensure t)

;; C++20 highlighting
(use-package modern-cpp-font-lock
  :diminish nil
  :hook
  (c++-mode . modern-c++-font-lock-mode)
  (modern-c++-font-lock-mode . (lambda () (diminish
                                           'modern-c++-font-lock-mode))))

;; google cpplint
(use-package flycheck-google-cpplint
  :config
  (with-eval-after-load 'flycheck
    '(progn
       (require 'flycheck-google-cpplint)
       ;; Add Google C++ Style checker.
       ;; In default, syntax checked by Clang and Cppcheck.
       (flycheck-add-next-checker 'c/c++-cppcheck
                                  '(warning . c/c++-googlelint)))))

;; google style, but with 4 space indent.
(defun google-set-c-style-with-4-indent ()
  "Set current buffer to google style, but with 4 space indent."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "Google" google-c-style t)
  (setq tab-width 4
        c-indent-tabs-mode t
        c-indent-level 4
        c-basic-offset 4))

(use-package google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style-with-4-indent)

;; copy from https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(provide 'init-cpp)
