;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package ox
  :ensure nil
  :custom
  (org-export-with-toc t)
  (org-export-with-tags 'not-in-toc)
  (org-export-with-drawers nil)
  (org-export-with-priority t)
  (org-export-with-footnotes t)
  (org-export-with-smart-quotes t)
  (org-export-with-section-numbers t)
  (org-export-with-sub-superscripts '{})
  ;; `org-export-use-babel' set to nil will cause all source block header arguments to be ignored This means that code blocks with the argument :exports none or :exports results will end up in the export.
  ;; See:
  ;; https://stackoverflow.com/questions/29952543/how-do-i-prevent-org-mode-from-executing-all-of-the-babel-source-blocks
  (org-export-use-babel t)
  (org-export-headline-levels 9)
  (org-export-coding-system 'utf-8)
  (org-export-with-broken-links 'mark)
  (org-export-default-language "zh-CN") ; 默认是en
  ;; (org-ascii-text-width 72)
  )

;; export extra
(use-package ox-extra
  :ensure nil
  :config
  (ox-extras-activate '(ignore-headlines))
  )

(use-package ox-html
  :ensure nil
  :init
  ;; add support for video
  (defun org-video-link-export (path desc backend)
    (let ((ext (file-name-extension path)))
      (cond
       ((eq 'html backend)
        (format "<video width='800' preload='metadata' controls='controls'><source type='video/%s' src='%s' /></video>" ext path))
       ;; fall-through case for everything else
       (t
        path))))
  (org-link-set-parameters "video" :export 'org-video-link-export)
  :custom
  (org-html-doctype "html5")
  (org-html-html5-fancy t)
  (org-html-checkbox-type 'unicode)
  (org-html-validation-link nil))

(use-package htmlize
  :ensure t
  :custom
  (htmlize-pre-style t)
  (htmlize-output-type 'inline-css))

(use-package ox-latex
  :ensure nil
  :defer t
  :config
  (add-to-list 'org-latex-classes
               '("cn-article"
                 "\\documentclass[UTF8,a4paper]{article}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("cn-report"
                 "\\documentclass[11pt,a4paper]{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (setq org-latex-default-class "cn-article")
  (setq org-latex-image-default-height "0.9\\textheight"
        org-latex-image-default-width "\\linewidth")
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl auto"
          ))
  ;; 使用 Listings 宏包格式化源代码(只是把代码框用 listing 环境框起来，还需要额外的设置)
  (setq org-latex-listings t)
  ;; mapping jupyter-python to Python
  (add-to-list 'org-latex-listings-langs '(jupyter-python "Python"))
  ;; Options for \lset command（reference to listing Manual)
  (setq org-latex-listings-options
        '(
          ("basicstyle" "\\small\\ttfamily")       ; 源代码字体样式
          ("keywordstyle" "\\color{eminence}\\small")                 ; 关键词字体样式
          ;; ("identifierstyle" "\\color{doc}\\small")
          ("commentstyle" "\\color{commentgreen}\\small\\itshape")    ; 批注样式
          ("stringstyle" "\\color{red}\\small")                       ; 字符串样式
          ("showstringspaces" "false")                                ; 字符串空格显示
          ("numbers" "left")                                          ; 行号显示
          ("numberstyle" "\\color{preprocess}")                       ; 行号样式
          ("stepnumber" "1")                                          ; 行号递增
          ("xleftmargin" "2em")                                       ;
          ;; ("backgroundcolor" "\\color{background}")                   ; 代码框背景色
          ("tabsize" "4")                                             ; TAB 等效空格数
          ("captionpos" "t")                                          ; 标题位置 top or buttom(t|b)
          ("breaklines" "true")                                       ; 自动断行
          ("breakatwhitespace" "true")                                ; 只在空格分行
          ("showspaces" "false")                                      ; 显示空格
          ("columns" "flexible")                                      ; 列样式
          ("frame" "tb")                                              ; 代码框：single, or tb 上下线
          ("frameleftmargin" "1.5em")                                 ; frame 向右偏移
          ;; ("frameround" "tttt")                                       ; 代码框： 圆角
          ;; ("framesep" "0pt")
          ;; ("framerule" "1pt")                                         ; 框的线宽
          ;; ("rulecolor" "\\color{background}")                         ; 框颜色
          ;; ("fillcolor" "\\color{white}")
          ;; ("rulesepcolor" "\\color{comdil}")
          ("framexleftmargin" "5mm")                                  ; let line numer inside frame
          ))
  )

(use-package ox-reveal
  :ensure t
  :after ox
  :config
  (setq org-reveal-hlevel 1)
  ;; Avalable themes: night, black, white, league, beige, sky, serif, simple, solarized, blood, moon
  (setq org-reveal-theme "moon")
  ;; can also set root to a CDN cloud: https://cdn.jsdelivr.net/npm/reveal.js
  (setq org-reveal-root (expand-file-name "reveal.js" user-emacs-directory))
  (setq org-reveal-mathjax t)
  (setq org-reveal-ignore-speaker-notes t)
  ;; original title font size is TOO large!
  (setq org-reveal-title-slide "<h1><font size=\"8\">%t</font></h1><h2><font size=\"6\">%s</font></h2><p><font size=\"5\">%a</font><br/><font size=\"5\">%d</font></p>")
  ;; don't load highlight, use htmlize instead. If you want to add line-number, add -n in src block header
  (setq org-reveal-plugins '(markdown zoom notes search))
  (setq org-reveal-klipsify-src 'on)
  (setq org-reveal-extra-css (expand-file-name "reveal.js/css/extra.css" user-emacs-directory))
  )

(use-package ox-gfm
  :ensure t
  :after ox)

(use-package ox-pandoc
  :ensure t
  :custom
  ;; special extensions for markdown_github output
  (org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))
  (org-pandoc-command "/usr/local/bin/pandoc")
  )

(use-package ox-publish
  :ensure nil
  :commands (org-publish org-publish-all)
  :config
  (setq org-export-global-macros
        '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))

  ;; sitemap 生成函数
  (defun my/org-sitemap-date-entry-format (entry style project)
    "Format ENTRY in org-publish PROJECT Sitemap format ENTRY ENTRY STYLE format that includes date."
    (let ((filename (org-publish-find-title entry project)))
      (if (= (length filename) 0)
          (format "*%s*" entry)
        (format "{{{timestamp(%s)}}} [[file:%s][%s]]"
                (format-time-string "%Y-%m-%d"
                                    (org-publish-find-date entry project))
                entry
                filename))))

  ;; 设置 org-publish 的项目列表
  (setq org-publish-project-alist
        '(
          ;; 笔记部分
          ("org-notes"
           :base-directory "~/org/"
           :base-extension "org"
           :exclude "\\(tasks\\|test\\|scratch\\|diary\\|capture\\|mail\\|habits\\|resume\\|meetings\\|personal\\|org-beamer-example\\)\\.org\\|test\\|article\\|roam\\|hugo"
           :publishing-directory "~/public_html/"
           :recursive t                 ; include subdirectories if t
           :publishing-function org-html-publish-to-html
           :headline-levels 6
           :auto-preamble t
           :auto-sitemap t
           :sitemap-filename "sitemap.org"
           :sitemap-title "Sitemap"
           :sitemap-format-entry my/org-sitemap-date-entry-format)

          ;; 静态资源部分
          ("org-static"
           :base-directory "~/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|mov"
           :publishing-directory "~/public_html/"
           :recursive t
           :publishing-function org-publish-attachment)

          ;; 项目集合
          ("org"
           :components ("org-notes" "org-static"))
          ))
  )

(use-package ox-hugo
  :ensure t
  :config
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ; Prompt to enter the post title
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ":END:"
                     "%?\n")          ; Place the cursor here finally
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"                ; `org-capture' binding + h
                   "Hugo post"
                   entry
                   ;; It is assumed that below file is present in `org-directory'
                   ;; and that it has a "Blog Ideas" heading. It can even be a
                   ;; symlink pointing to the actual location of capture.org!
                   (file+olp "capture.org" "Notes")
                   (function org-hugo-new-subtree-post-capture-template))))
  )

(provide 'init-ox)
;;; init-ox.el ends here
