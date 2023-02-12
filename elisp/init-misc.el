;; -*- coding: utf-8; lexical-binding: t -*-

(setq inhibit-startup-screen t)
;; disable menu bar, tool-bar
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(global-superword-mode t)

(setq visible-bell t);;关闭出错时的提示声
                                        ;
(setq show-paren-style 'parenthesis)

(mouse-avoidance-mode 'animate) ;;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(setq appt-issue-message t)

(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)

(global-unset-key (kbd "C-SPC")) ;; 输入法快捷键冲突
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8

;; 对齐插入空格而不是tab
;; copy from http://stackoverflow.com/questions/22710040/emacs-align-regexp-with-spaces-instead-of-tabs
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;;如果有喜欢用鼠标选择emacs文本的同学, 可以试试加上这句配置:
(setq mouse-drag-copy-region t)


;; copy from [Emacs and symbolic links](https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links)
(setq vc-follow-symlinks t)

;; copy from https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; copy from https://blog.ginshio.org/2022/doom_emacs_configuration/#guix
(setq-default
 window-combination-resize t        ; 从其他窗口获取新窗口的大小
 x-stretch-cursor t                 ; 将光标拉伸到字形宽度
 )

(setq undo-limit 104857600         ; 重置撤销限制到 100 MiB
      truncate-string-ellipsis "…" ; Unicode 省略号相比 ascii 更好
      ;; 同时节省 /宝贵的/ 空间
      password-cache-expiry nil    ; 我能信任我的电脑 ... 或不能?
      ;; 不要让 `点' (光标) 跳来跳去
      scroll-margin 2              ; 适当保持一点点边距
      gc-cons-threshold 1073741824
      read-process-output-max 1048576
      )

(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

(global-unset-key (kbd "C-z"))      ; 关闭 "C-z" 最小化

(custom-set-variables '(delete-selection-mode t) ;; delete when you select region and modify
                      '(inhibit-compacting-font-caches t) ;; don’t compact font caches during GC.
                      )

;; Dark and transparent title bar in macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; copy from https://qiita.com/sanryuu/items/e2584d6163b5bf27c885
(setq max-specpdl-size 10000)
(setq max-lisp-eval-depth 10000)


(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(provide 'init-misc)
