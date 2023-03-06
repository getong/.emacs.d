;;; -*- coding: utf-8; lexical-binding: t -*-

;; Flycheck is a general syntax highlighting framework which other packages hook into. It's an improvment on the built in flymake.
;; Setup is pretty simple - we just enable globally and turn on a custom eslint function, and also add a custom checker for proselint.
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (setq flycheck-indication-mode 'right-fringe)
  ;; only check on save
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-highlighting-mode 'lines)
  ;; Define fringe indicator / warning levels
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info))

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :commands flycheck-rust-setup
  :config
  :init (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(use-package consult-flycheck)

(use-package flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup))

;; Add flycheck support.
(use-package flycheck-irony
  :ensure t
  :hook (flycheck-mode . flycheck-irony-setup))

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

(use-package flycheck-phpstan
  :ensure t
  :hook ((php-mode) . flycheck-mode)
  :commands (flycheck-mode)
  :config
  (setq-default phpstan-executable 'docker)
  )

(use-package flycheck-posframe
  :custom-face
  (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
  (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'window-bottom-left-corner)
  (flycheck-posframe-border-width 3)
  ;; (flycheck-posframe-inhibit-functions '((lambda (&rest _) (bound-and-true-p company-backend))))
  )

(use-package flycheck-pos-tip
  :defines flycheck-pos-tip-timeout
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :custom (flycheck-pos-tip-timeout 30))

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(provide 'init-flycheck)
;;; init-flycheck ends here
