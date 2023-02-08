;; -*- coding: utf-8; lexical-binding: t -*-

;; 这两段一定要在 init.el 的最上方
(require 'package)
;; 初始化包管理器
(package-initialize)

;; 设置软件源
(setq package-archives '(
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")
                         ("stable-melpa" . "http://mirrors.ustc.edu.cn/elpa/stable-melpa/")
                         ("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("gnu-devel" . "http://mirrors.ustc.edu.cn/elpa/gnu-devel/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ("nongnu-devel" . "http://mirrors.ustc.edu.cn/elpa/nongnu-devel/")
                         ;; ("marmalade" . "http://mirrors.cloud.tencent.com/elpa/marmalade/")
                         ;; ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
                         ;; ("org" . "http://mirrors.cloud.tencent.com/elpa/org/")
                         ;; ("stable-melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa-stable/")
                         ;; ("gnu" . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ;; ("gnu-devel" . "http://mirrors.ustc.edu.cn/elpa/gnu-devel/")
                         ;; ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ;; ("nongnu-devel" . "http://mirrors.ustc.edu.cn/elpa/nongnu-devel/")
                         ))

;; 设置软件源优先级
(setq package-archive-priorities '(("melpa"  . 10)
                                   ("org"  . 5)
                                   ("stable-melpa"  . 5)
                                   ;; ("marmalade"  . 5)
                                   ("gnu"  . 3)
                                   ("gnu-devel"  . 3)
                                   ("nongnu"    . 1)
                                   ("nongnu-devel" . 1)))


;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; copy from [Configuring Emacs for Rust development](https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/)
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  (package-install 'no-littering))


;;; copy from [How to automatically install Emacs packages by specifying a list of package names?](https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name)
;;; list the packages you want
;;(setq package-list '(indent-guide pangu-spacing spinner undo-tree highlight-thing markdown-mode switch-window
;;                                  protobuf-mode tide dart-mode dart-server mix csharp-mode omnisharp lua-mode flycheck-rust rust-mode
;;                                  swift-mode lsp-mode which-key use-package rustic magit openwith
;;                                  color-theme-sanityinc-tomorrow olivetti aggressive-indent dap-mode vterm multiple-cursors))

;; install the missing packages
;;(dolist (package package-list)
;;  (unless (package-installed-p package)
;;    (package-install package)))

;; this i only needed once
(eval-when-compile
  (require 'use-package))

;个别时候会出现签名校验失败
(setq package-check-signature nil)

;; copy from https://ianyepan.github.io/posts/setting-up-use-package/
;; Setting use-package-always-ensure to t (meaning “true”) saves us the trouble of having to specify :ensure t in any future packages we’d like to declare and install.
(eval-and-compile
  ;; 不用每个包都手动添加:ensure t关键字
  (setq use-package-always-ensure t)
  ;; 默认都是延迟加载，不用每个包都手动添加:defer t
  ;; (setq use-package-always-defer t)
  ;; (setq use-package-always-demand nil)
  (setq use-package-expand-minimally t)
  (setq use-package-verbose t))

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

;; 启用 straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; copy from https://www.reddit.com/r/emacs/comments/siuvpu/isnt_there_a_better_way_to_set_utf8/
(set-locale-environment "zh_CN.UTF-8")
