;; 这两段一定要在 init.el 的最上方
(require 'package)
;; 初始化包管理器
(package-initialize)

;; 设置软件源
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("gnu-devel" . "http://mirrors.ustc.edu.cn/elpa/gnu-devel/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ("nongnu-devel" . "http://mirrors.ustc.edu.cn/elpa/nongnu-devel/")
                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")
                         ("stable-melpa" . "http://mirrors.ustc.edu.cn/elpa/stable-melpa/")
                         ))


;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; copy from [Configuring Emacs for Rust development](https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;;; copy from [How to automatically install Emacs packages by specifying a list of package names?](https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name)
;;; list the packages you want
(setq package-list '(indent-guide pangu-spacing spinner undo-tree highlight-thing markdown-mode switch-window
                                  protobuf-mode tide dart-mode dart-server mix csharp-mode omnisharp lua-mode flycheck-rust rust-mode
                                  swift-mode lsp-mode which-key use-package rustic magit openwith corfu orderless
                                  color-theme-sanityinc-tomorrow olivetti aggressive-indent dap-mode vterm multiple-cursors))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-package)
