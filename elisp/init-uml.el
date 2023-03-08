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

(use-package gnuplot
  :ensure t
  :mode ("\\.gp$" . gnuplot-mode)
  :init
  (add-to-list 'org-src-lang-modes '("gnuplot" . gnuplot))
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((gnuplot . t))))
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))
  (setq org-babel-default-header-args:gnuplot
        '((:exports . "results")
          (:results . "file")))
  )

(use-package lilypond-mode
  :ensure nil
  :mode ("\\.i?ly\\'" . LilyPond-mode)
  :init
  (add-to-list 'org-src-lang-modes '("lilypond" . lilypond))
  ;; add support for org babel
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((lilypond . t))))
  ;; set lilypond binary directory
  (setq org-babel-lilypond-ly-command "/usr/local/bin/lilypond -dpreview")
  :config
  ;; trim extra space for generated image
  (defun my/trim-lilypond-png (orig-fun
                               &optional arg
                               info
                               param)
    (when (member (car (org-babel-get-src-block-info)) '("lilypond"))
      (let ((ly-file (alist-get :file (nth 2 (org-babel-get-src-block-info)))))
        (let ((ly-preview-file (replace-regexp-in-string "\\.png" ".preview.png" ly-file)))
          (when (file-exists-p ly-preview-file)
            (shell-command (concat "mv " ly-preview-file " " ly-file)))
          (org-redisplay-inline-images)))))
  (advice-add 'org-babel-execute-src-block :after #'my/trim-lilypond-png)
  (setq ob-lilypond-header-args
        '((:results . "file replace")
          (:exports . "results")
          ))
  )

(provide 'init-uml)
;;; init-uml.el ends here
