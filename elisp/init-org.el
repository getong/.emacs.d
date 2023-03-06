;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package org-tree-slide
  :ensure t
  :defer t
  :config
  (defun my/org-tree-slide-setup ()
    (org-display-inline-images)
    (hide-mode-line-mode 1))

  (defun my/org-tree-slide-end ()
    (org-display-inline-images)
    (hide-mode-line-mode 0))
  :custom
  (org-image-actual-width nil)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  :hook ((org-tree-slide-play . my/org-tree-slide-setup)
         (org-tree-slide-stop . my/org-tree-slide-end))
  :bind (:map org-tree-slide-mode-map
              ("C-<" . org-tree-slide-move-previous-tree)
              ("C->" . org-tree-slide-move-next-tree)))


;; Beautify Org heading symbol
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))

;; Emoji Org tag
(use-package org-pretty-tags
  :diminish org-pretty-tags-mode
  :ensure t
  :config
  (setq org-pretty-tags-surrogate-strings
        '(
          ("work"  . "⚒")
          ("@pc" . "🖥")
          ("@ps5" . "🎮")
          ("@switch" . "🕹")
          ("script" . "📝")
          ))
  (org-pretty-tags-global-mode))

;; Org fancy Priorities
(use-package org-fancy-priorities
  :diminish
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))


(provide 'init-org)
;;; init-org ends here
