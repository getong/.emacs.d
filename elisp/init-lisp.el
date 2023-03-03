(use-package sly
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package suggest
  :ensure t
  :defer t)

(provide 'init-lisp)
