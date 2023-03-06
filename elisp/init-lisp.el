(use-package sly
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package ielm
  :ensure nil
  :config
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(provide 'init-lisp)
