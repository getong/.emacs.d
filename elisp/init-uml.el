;;; -*- coding: utf-8; lexical-binding: t -*-

;;; plantuml
;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :init
  ;; (setq plantuml-jar-path "d:/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  )

(use-package plantuml-emacs
  :straight (plantuml-emacs :type git :host github :repo "ginqi7/plantuml-emacs")
  :after plantuml
  :config
  (setq
   plantuml-jar-path (concat
                      (file-name-directory
                       (directory-file-name
                        (file-name-directory
                         (file-truename (executable-find "plantuml"))))) "libexec/plantuml.jar")
   plantuml-output-type "svg"
   plantuml-relative-path "~/images/"
   plantuml-theme "plain"
   plantuml-font "somefont"
   plantuml-add-index-number t
   plantuml-log-command t
   plantuml-mindmap-contains-org-content t
   plantuml-org-headline-bold t)
  )

(provide 'init-uml)
;;; init-uml.el ends here
