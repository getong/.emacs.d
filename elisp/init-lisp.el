(use-package sly
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package ielm
  :ensure nil
  :config
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package elisp-mode
  :ensure nil
  :after org
  :bind (:map emacs-lisp-mode-map
              ("C-c C-b" . eval-buffer)
              ("C-c C-c" . eval-to-comment)
              :map lisp-interaction-mode-map
              ("C-c C-c" . eval-to-comment)
              :map org-mode-map
              ("C-c C-;" . eval-to-comment)
              )
  :init
  ;; for emacs-lisp org babel
  (add-to-list 'org-babel-default-header-args:emacs-lisp
               '(:results . "value pp"))
  :config
  (defconst eval-as-comment-prefix " ⇒ ")
  (defun eval-to-comment (&optional arg)
    (interactive "P")
    ;; (if (not (looking-back ";\\s*"))
    ;;     (call-interactively 'comment-dwim))
    (call-interactively 'comment-dwim)
    (progn
      (search-backward ";")
      (forward-char 1))
    (delete-region (point) (line-end-position))
    (save-excursion
      (let ((current-prefix-arg '(4)))
        (call-interactively 'eval-last-sexp)))
    (insert eval-as-comment-prefix)
    (end-of-line 1))
  )

(provide 'init-lisp)
;;; init-lisp.el ends here
