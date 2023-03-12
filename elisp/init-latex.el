;;; init-latex.el --- Summary latex file -*- lexical-binding: t -*-

;;; Commentary:
;; latex

;;; Code:

;; latex
(use-package auctex
  :ensure t
  :defer t
  :init
  (setq TeX-view-program-selection '((output-pdf "pdf-tools"))
        TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  )

(use-package latex-preview-pane
  :ensure t
  :defer t)

(use-package auctex-latexmk
  :ensure t
  :after tex
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package bibtex
  :ensure nil
  :init
  (setq-default bibtex-dialect 'biblatex))

(use-package biblio
  :ensure t)

(use-package reftex
  :ensure t
  :init
  (setq reftex-plug-into-AUCTeX t))

(provide 'init-latex)
;;; init-latex.el ends here
