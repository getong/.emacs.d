;;; init-python.el --- Summary python file -*- lexical-binding: t -*-

;;; Commentary:
;; python

;;; Code:

;; Python Mode
;; Install: pip install pyflakes autopep8
(use-package python
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  )

;; Live Coding in Python
(use-package live-py-mode
  :ensure t)

(use-package pyenv-mode
  :ensure t
  :config
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
  (add-hook 'python-mode-hook 'pyenv-mode))

(use-package jedi
  :ensure t)

(use-package eval-in-repl-python
  :ensure t)

(use-package ansible
  :ensure t
  :mode
  (("ansible/group_vars/.*" . yaml-mode)
   ("ansible/host_vars/.*"  . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook #'ansible))

(use-package jinja2-mode
  :ensure t
  :mode "\\.j2?\\'")

(use-package ansible-doc
  :ensure t
  :init
  (add-hook 'ansible::hook #'ansible-doc-mode))

(use-package ansible-vault
  :ensure t)

(provide 'init-python)
;;; init-python.el ends here
