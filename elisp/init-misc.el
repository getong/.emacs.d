;; -*- coding: utf-8; lexical-binding: t -*-


(setq inhibit-startup-screen t)
;; disable menu bar, tool-bar
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; (setq-default indent-tabs-mode nil)
;; (setq default-tab-width 4)

;; (setq dart-server-sdk-path "~/flutter/bin/cache/dart-sdk/")
;; (setq dart-server-enable-analysis-server t)
;; (add-hook 'dart-server-hook 'flycheck-mode)

(global-superword-mode t)
;; (global-hl-line-mode t)

;; copy from [hl-line-mode hide background, how to avoid this?](https://emacs.stackexchange.com/questions/10445/hl-line-mode-hide-background-how-to-avoid-this)
;; (defun my-hl-line-range-function ()
;;   (cons (line-end-position) (line-beginning-position 2)))
;; (setq hl-line-range-function #'my-hl-line-range-function)

;; (when window-system
;;   (require 'hl-line)
;;   (set-face-attribute 'hl-line nil :inherit nil :background "light yellow")
;;   (setq global-hl-line-sticky-flag t)
;;   (global-hl-line-mode 1))

;;(setq-default indent-tabs-mode nil)
(ido-mode 1)
;; (setq column-number-mode t)

(setq inhibit-startup-message t) ;; 关闭起动时LOGO
(setq visible-bell t);;关闭出错时的提示声
(global-font-lock-mode t);语法高亮
;; (show-paren-mode t) ;; 显示括号匹配
(setq show-paren-style 'parenthesis)
;;(setq mouse-yank-at-point t);;支持中键粘贴
(mouse-avoidance-mode 'animate) ;;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(setq appt-issue-message t)

;; (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

(setq backup-directory-alist (quote (("." . "~/.backups"))))
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-by-copying t)


(setq initial-scratch-message nil)

(global-unset-key (kbd "C-SPC")) ;; 输入法快捷键冲突
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; (icomplete-mode 99)


;; (spinner-start 'vertical-breathing 10)
;; (spinner-start 'minibox)
;; (spinner-start 'moon)
;; (spinner-start 'triangle)

;; 高亮显示选中区域
;; (transient-mark-mode t)
;; 高亮选中区域颜色
;; (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")


;; (defun switch-to-frame (frame-name)
;;   (interactive "sFrame name:")
;;   (let ((frames (frame-list)))
;;     (catch 'break
;;       (while frames (let ((frame (car frames)))
;;                       (if (equal (frame-parameter frame 'name) frame-name)
;;                           (throw 'break (select-frame-set-input-focus frame))
;;                         (setq frames (cdr frames))))))))

;; (global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

;; tabs 底下显示 bar
;; (setq x-underline-at-descent-line t)

;;文本解码设置默认为 UTF-8
;; (setq locale-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (set-language-environment 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-file-name-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

;; Emacs 自动加载外部修改过的文件
;; (global-auto-revert-mode 1)

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8

;; 对齐插入空格而不是tab
;; copy from http://stackoverflow.com/questions/22710040/emacs-align-regexp-with-spaces-instead-of-tabs
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;;如果有喜欢用鼠标选择emacs文本的同学, 可以试试加上这句配置:
(setq mouse-drag-copy-region t)

;; (autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; (autoload 'gfm-mode "markdown-mode"
;;   "Major mode for editing GitHub Flavored Markdown files" t)
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; (setq debug-on-error nil)

;; copy from [Undo Tree](https://www.emacswiki.org/emacs/UndoTree)
;;(global-undo-tree-mode)

;; (savehist-mode 1)

;; (setq savehist-file "~/.emacs.d/var/savehist")

;; copy from https://github.com/dimitri/switch-window
;;(require 'switch-window)
;;(global-set-key (kbd "C-x o") 'switch-window)
;;(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
;;(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
;;(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
;;(global-set-key (kbd "C-x 0") 'switch-window-then-delete)
;;
;;(global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
;;(global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
;;(global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
;;(global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)
;;
;;(global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
;;(global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)
;;
;;(global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)
;;
;;(setq switch-window-shortcut-style 'qwerty)
;;(setq switch-window-qwerty-shortcuts
;;      '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;;(add-hook 'typescript-mode-hook #'setup-tide-modeµ)

;; (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))



;; copy from [Emacs and symbolic links](https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links)
(setq vc-follow-symlinks t)



;; (defun insert-todo-comment ()
;;   (interactive)
;;   (indent-for-tab-command)
;;   (insert "TODO: ")
;;   (back-to-indentation)
;;   (set-mark-command nil)
;;   (move-end-of-line nil)
;;   (comment-dwim nil))
;; (defun todo-comment-on-next-line ()
;;   "Insert a TODO comment on the next line at the proper indentation"
;;   (interactive)
;;   (move-end-of-line nil)
;;   (newline)
;;   (insert-todo-comment))
;; (add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "") #'todo-comment-on-next-line)))

;; copy from https://xenodium.com/emacs-create-a-swift-packageproject/
;; (defun ar/swift-package-init ()
;;   "Execute `swift package init', with optional name and completing type."
;;   (interactive)
;;   (let* ((name (read-string "name (default): "))
;;          (type (completing-read
;;                 "project type: "
;;                 ;; Splits "--type empty|library|executable|system-module|manifest"
;;                 (split-string
;;                  (nth 1 (split-string
;;                          (string-trim
;;                           (seq-find
;;                            (lambda (line)
;;                              (string-match "--type" line))
;;                            (process-lines "swift" "package" "init" "--help")))
;;                          "   "))
;;                  "|")))
;;          (command (format "swift package init --type %s" type)))
;;     (unless (string-empty-p name)
;;       (append command "--name " name))
;;     (shell-command command))
;;   (dired default-directory)
;;   (revert-buffer))

;; (defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
;;   "Start a program in a subprocess.  Return the process object for it. Similar to `start-process-shell-command', but calls `start-file-process'."
;;   ;; On remote hosts, the local `shell-file-name' might be useless.
;;   (let ((command (mapconcat 'identity args " ")))
;;     (funcall start-file-process-shell-command name buffer command)))
;; (advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)

;; copy from http://xahlee.info/emacs/emacs/emacs_set_default_font_size.html
;; (defun xah-set-default-font-size ()
;;   "Set default font globally.
;; Note, this command change font size only for current session, not in init file.
;; This command useful for making font large when you want to do video livestream.
;; URL `http://xahlee.info/emacs/emacs/emacs_set_default_font_size.html'
;; Version: 2021-07-26 2021-08-21 2022-08-05"
;;   (interactive)
;;   (let (($fSize (read-string "size:" "16" nil "16")))
;;     (if (> (string-to-number $fSize) 51)
;;         (user-error "Max font size allowed is 51. You gave %s " $fSize)
;;       (progn
;;         (set-frame-font
;;          (cond
;;           ((string-equal system-type "windows-nt")
;;            (if (member "Consolas" (font-family-list)) (format "Consolas-%s" $fSize) nil))
;;           ((string-equal system-type "darwin")
;;            ;; (if (member "LXGW WenKai Mono" (font-family-list)) "LXGW WenKai Mono" nil))
;;            (if (member "EB Garamond" (font-family-list)) (format "EB Garamond 12 Italic %s" $fSize) nil))
;;           ((string-equal system-type "gnu/linux")
;;            (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono" nil))
;;           (t nil))
;;          t t)
;;         (set-face-attribute 'default nil :font  (format "EB Garamond 12 Italic %s" $fSize))
;;         (set-fontset-font "fontset-default"
;;                           'han (font-spec :family "LXGW WenKai Mono"
;;                                           :size (string-to-number $fSize)))
;;         (set-fontset-font "fontset-default"
;;                           'unicode (font-spec :family "LXGW WenKai Mono"
;;                                               :size (string-to-number $fSize)))
;;         (set-fontset-font "fontset-default"
;;                           'unicode-bmp (font-spec :family "LXGW WenKai Mono"
;;                                                   :size (string-to-number $fSize)))
;;         )
;;       ))
;;   )
;; copy from https://www.emacswiki.org/emacs/SystemTrash
;; (setq delete-by-moving-to-trash t)
;; (defun system-move-file-to-trash (file)
;;   "Use \"trash\" to move FILE to the system trash.
;; When using Homebrew, install it using \"brew install trash-cli\"."
;;   (call-process (executable-find "trash-put")
;; 		        nil 0 nil
;; 		        file))



;; copy from https://github.com/NapoleonWils0n/ubuntu-dotfiles/blob/f061fa1b05c5ccba8f6b4a2d165660ab8ab3c56b/.config/emacs/init.el#L170
;; openwth
;; (require 'mm-util)
;; (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)



(setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows '(bar . 2))
(setq-default blink-cursor-blinks 50)
(setq-default blink-cursor-interval nil) ; 0.75 would be my choice
(setq-default blink-cursor-delay 0.2)
(blink-cursor-mode -1)
;; (define-minor-mode prot/cursor-type-mode
;;   "Toggle between static block and pulsing bar cursor."
;;   :init-value nil
;;   :global t
;;   (if prot/cursor-type-mode
;;       (progn
;;         (setq-local blink-cursor-interval 0.75
;;                     cursor-type '(bar . 2)
;;                     cursor-in-non-selected-windows 'hollow)
;;         (blink-cursor-mode 1))
;;     (dolist (local '(blink-cursor-interval
;;                      cursor-type
;;                      cursor-in-non-selected-windows))
;;       (kill-local-variable `,local))
;;     (blink-cursor-mode -1)))
;; copy from https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (defalias 'yes-or-no-p 'y-or-n-p)

;; (setq user-emacs-directory (file-truename "~/.emacs.d/"))

;; copy from https://blog.ginshio.org/2022/doom_emacs_configuration/#guix
(setq-default
 window-combination-resize t        ; 从其他窗口获取新窗口的大小
 x-stretch-cursor t                 ; 将光标拉伸到字形宽度
 )

(setq undo-limit 104857600         ; 重置撤销限制到 100 MiB
      ;; auto-save-default t          ; 没有人喜欢丢失工作，我也是如此
      truncate-string-ellipsis "…" ; Unicode 省略号相比 ascii 更好
      ;; 同时节省 /宝贵的/ 空间
      password-cache-expiry nil    ; 我能信任我的电脑 ... 或不能?
      ;; scroll-preserve-screen-position 'always
      ;; 不要让 `点' (光标) 跳来跳去
      scroll-margin 2              ; 适当保持一点点边距
      gc-cons-threshold 1073741824
      read-process-output-max 1048576
      )

(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
;; 设置最大化启动
;;(display-time-mode t)             ; 开启时间状态栏
;; (require 'battery)
;; (when (and battery-status-function
;;            (not (string-match-p "N/A"
;;                                 (battery-format "%B"
;;                                                 (funcall battery-status-function)))))
;;   (display-battery-mode 1))         ; 知道还剩多少 ⚡️ 很重要

;; (global-subword-mode 1)             ; 识别驼峰，而不是傻瓜前进
(global-unset-key (kbd "C-z"))      ; 关闭 "C-z" 最小化
;;(define-key global-map "C-s" #'+default/search-buffer)
;;(map (:leader (:desc "load a saved workspace" :g "wr" #'+workspace/load))) ;; workspace load keybind


(custom-set-variables '(delete-selection-mode t) ;; delete when you select region and modify
                      ;; '(delete-by-moving-to-trash t) ;; delete && move to transh
                      '(inhibit-compacting-font-caches t) ;; don’t compact font caches during GC.
                      '(gc-cons-percentage 1))

;; Dark and transparent title bar in macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))


;; (set-face-attribute 'hl-line nil :inherit nil :background "light yellow")
;; (set-face-attribute 'line-number nil :inherit nil :foreground "light green")
;; (set-face-attribute 'line-number-current-line nil :inherit nil :foreground "white")
;; copy from https://qiita.com/sanryuu/items/e2584d6163b5bf27c885
(setq max-specpdl-size 10000)
(setq max-lisp-eval-depth 10000)

;; https://emacs.stackexchange.com/questions/18677/prevent-auto-save-list-directory-to-be-created
(setq auto-save-list-file-prefix nil)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(provide 'init-misc)
