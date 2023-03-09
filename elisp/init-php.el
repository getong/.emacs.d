;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package php-mode
  ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
  :mode
  ;; (("[^.][^t][^p][^l]\\.php$" . php-mode))
  (("\\.php\\'" . php-mode)
   ("\\.inc\\'" . php-mode)
   ("\\.module\\'" . php-mode))
  :custom
  (indent-tabs-mode nil)
  (tab-width 2)
  (c-basic-offset 2)
  :hook (php-mode . flycheck-mode)
  :config
  (add-hook 'php-mode-hook
            #'(lambda ()
         ;;; PHP-mode settings:
                (setq indent-tabs-mode nil
                      c-basic-offset 4
                      php-template-compatibility nil)

                (php-enable-psr2-coding-style)

         ;;; PHP_CodeSniffer settings:
                ;; (use-package phpcbf
                ;; :init
                ;; (setq phpcbf-executable "~/.composer/vendor/squizlabs/php_codesniffer/scripts/phpcbf"
                ;; phpcbf-standard "PSR2"))

         ;;; Company-mode settings:
                ;; Using :with and company-sort-by-backend-importance makes
                ;; it so that company-lsp entries will always appear before
                ;; company-dabbrev-code.
                ;; TODO Add in support for company-gtags/capf

                (ac-php-core-eldoc-setup)
                ;; (setq-local company-dabbrev-char-regexp "\\\`$sw")
                ;; (setq-local company-dabbrev-code-everywhere t)
                ;; (setq-local company-transformers '(company-sort-by-backend-importance))
                ;; (set (make-local-variable 'company-backends)
                ;;      ;;'((company-ac-php-backend company-dabbrev-code)))
                ;;      ;;'((company-ac-php-backend company-dabbrev-code :separate)))
                ;;      '((company-dabbrev-code company-ac-php-backend)))
                ;;'((company-ac-php-backend :with company-dabbrev-code)))
                ;; '((company-lsp :with company-dabbrev-code)))

         ;;; LSP (Language Server Protocol) Settings:
                ;; (add-to-list 'load-path "~/.emacs.d/lsp-php")
                ;; (require 'lsp-php)
                ;; (custom-set-variables
                ;; Composer.json detection after Projectile.
                ;;  '(lsp-php-workspace-root-detectors (quote (lsp-php-root-projectile lsp-php-root-composer-json lsp-php-root-vcs)))
                ;; )
                ;; (lsp-php-enable)

         ;;; Flycheck Settings:
                (defvar-local flycheck-checker 'php-phpcs)
                (setq-local flycheck-check-syntax-automatically '(save))

         ;;; Key Bindings:
                ;; (dumb-jump-mode)
                ;; (ggtags-mode 1)
                ;; [J]ump to a function definition (at point)
                (local-set-key (kbd "C-c j") 'ac-php-find-symbol-at-point)
                ;; (local-set-key (kbd "C-c j") 'dumb-jump-go)
                ;; (local-set-key (kbd "C-c j") 'ggtags-find-definition)

                ;; Find [r]eferences (at point)
                ;; (local-set-key (kbd "C-c r") 'ggtags-find-reference)

                ;; Go [b]ack, after jumping
                ;; (local-set-key (kbd "C-c b") 'dumb-jump-back)
                (local-set-key (kbd "C-c b") 'ac-php-location-stack-back)
                ;; (local-set-key (kbd "C-c b") 'ggtags-prev-mark)

                ;; Go [f]orward
                (local-set-key (kbd "C-c f") 'ac-php-location-stack-forward)
                ;; (local-set-key (kbd "C-c f") 'ggtags-next-mark)

                ;; [S]how a function definition (at point)
                (local-set-key (kbd "C-c s") 'ac-php-show-tip)
                ;; (local-set-key (kbd "C-c q") 'dumb-jump-quick-look)

                ;; Re[m]ake the tags (after a source has changed)
                (local-set-key (kbd "C-c m") 'ac-php-remake-tags)

                ;; Show [p]roject info
                (local-set-key (kbd "C-c p") 'ac-php-show-cur-project-info)

                ;; Bring up [i]menu
                (local-set-key (kbd "C-c i") 'helm-imenu)))
  )


(provide 'init-php)
;;; init-php.el ends here
