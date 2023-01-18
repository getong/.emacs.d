;; -*- coding: utf-8; lexical-binding: t -*-

(setq inhibit-startup-screen t)
;; disable menu bar, tool-bar
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

(setq dart-server-sdk-path "~/flutter/bin/cache/dart-sdk/")
(setq dart-server-enable-analysis-server t)
(add-hook 'dart-server-hook 'flycheck-mode)

(global-superword-mode t)
(global-hl-line-mode t)

;; copy from [hl-line-mode hide background, how to avoid this?](https://emacs.stackexchange.com/questions/10445/hl-line-mode-hide-background-how-to-avoid-this)
(defun my-hl-line-range-function ()
  (cons (line-end-position) (line-beginning-position 2)))
(setq hl-line-range-function #'my-hl-line-range-function)

(when window-system
  (require 'hl-line)
  (set-face-attribute 'hl-line nil :inherit nil :background "light yellow")
  (setq global-hl-line-sticky-flag t)
  (global-hl-line-mode 1))

;;(setq-default indent-tabs-mode nil)
(ido-mode 1)
(setq column-number-mode t)

(setq inhibit-startup-message t) ;; 关闭起动时LOGO
(setq visible-bell t);;关闭出错时的提示声
(global-font-lock-mode t);语法高亮
(show-paren-mode t) ;; 显示括号匹配
(setq show-paren-style 'parenthesis)
;;(setq mouse-yank-at-point t);;支持中键粘贴
(mouse-avoidance-mode 'animate) ;;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(setq appt-issue-message t)

(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))


(setq backup-directory-alist (quote (("." . "~/.backups"))))
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-by-copying t)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
  See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
  See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

(setq initial-scratch-message nil)

(global-unset-key (kbd "C-SPC")) ;; 输入法快捷键冲突
(global-set-key (kbd "M-SPC") 'set-mark-command)

(icomplete-mode 99)

(defun split-window-4()
  "Splite window into 4 sub-window"
  (interactive)
  (if (= 1 (length (window-list)))
      (progn (split-window-vertically)
             (split-window-horizontally)
             (other-window 2)
             (split-window-horizontally)
             )
    )
  )

(defun change-split-type (split-fn &optional arg)
  "Change 3 window style from horizontal to vertical and vice-versa"
  (let ((bufList (mapcar 'window-buffer (window-list))))
    (select-window (get-largest-window))
    (funcall split-fn arg)
    (mapcar* 'set-window-buffer (window-list) bufList)))


(defun change-split-type-2 (&optional arg)
  "Changes splitting from vertical to horizontal and vice-versa"
  (interactive "P")
  (let ((split-type (lambda (&optional arg)
                      (delete-other-windows-internal)
                      (if arg (split-window-vertically)
                        (split-window-horizontally)))))
    (change-split-type split-type arg)))


(defun change-split-type-3-v (&optional arg)
  "change 3 window style from horizon to vertical"
  (interactive "P")
  (change-split-type 'split-window-3-horizontally arg))

(defun change-split-type-3-h (&optional arg)
  "change 3 window style from vertical to horizon"
  (interactive "P")
  (change-split-type 'split-window-3-vertically arg))

(defun split-window-3-horizontally (&optional arg)
  "Split window into 3 while largest one is in horizon"
  ;;  (interactive "P")
  (delete-other-windows)
  (split-window-horizontally)
  (if arg (other-window 1))
  (split-window-vertically))

(defun split-window-3-vertically (&optional arg)
  "Split window into 3 while largest one is in vertical"
  ;; (interactive "P")
  (delete-other-windows)
  (split-window-vertically)
  (if arg (other-window 1))
  (split-window-horizontally))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(spinner-start 'vertical-breathing 10)
(spinner-start 'minibox)
(spinner-start 'moon)
(spinner-start 'triangle)

;; 高亮显示选中区域
(transient-mark-mode t)
;; 高亮选中区域颜色
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")



(defun switch-to-frame (frame-name)
  (interactive "sFrame name:")
  (let ((frames (frame-list)))
    (catch 'break
      (while frames (let ((frame (car frames)))
                      (if (equal (frame-parameter frame 'name) frame-name)
                          (throw 'break (select-frame-set-input-focus frame))
                        (setq frames (cdr frames))))))))

(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

;; 关闭所有的buffer
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(setq x-underline-at-descent-line t)

;;文本解码设置默认为 UTF-8
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Emacs 自动加载外部修改过的文件
(global-auto-revert-mode 1)

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8

;; 对齐插入空格而不是tab
;; copy from http://stackoverflow.com/questions/22710040/emacs-align-regexp-with-spaces-instead-of-tabs
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;;如果有喜欢用鼠标选择emacs文本的同学, 可以试试加上这句配置:
(setq mouse-drag-copy-region t)


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(setq debug-on-error nil)

;; copy from [Undo Tree](https://www.emacswiki.org/emacs/UndoTree)
;;(global-undo-tree-mode)

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/var/savehist")

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
(add-hook 'before-save-hook 'tide-format-before-save)

;;(add-hook 'typescript-mode-hook #'setup-tide-modeµ)

(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; copy from [Aligning columns in Emacs](https://blog.lambda.cx/posts/emacs-align-columns/)
(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

;; copy from [Emacs and symbolic links](https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links)
(setq vc-follow-symlinks t)

;; copy from [How to configure dired to update instantly when files/folders change?](https://www.reddit.com/r/emacs/comments/1acg6q/how_to_configure_dired_to_update_instantly_when/)
(setq global-auto-revert-non-file-buffers t)

;; copy from https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
(setq delete-by-moving-to-trash t)
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (setq trash-directory "/backup/.Trash-1000/files")  ;; fallback for `move-file-to-trash'
    ))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (setq trash-directory (expand-file-name "~/.Trash"))  ;; fallback for `move-file-to-trash'
    ))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (setq trash-directory "/backup/.Trash-1000/files")  ;; fallback for `move-file-to-trash'
    )))

(when (memq window-system '(mac ns))
  (defun system-move-file-to-trash (path)
    "Moves file at PATH to the macOS Trash according to `move-file-to-trash' convention.
Relies on the command-line utility 'trash' to be installed.
Get it from:  <http://hasseg.org/trash/>"
    (shell-command (concat "trash -vF \"" path "\""
                           "| sed -e 's/^/Trashed: /'")
                   nil ;; Name of output buffer
                   "*Trash Error Buffer*")))


(defun insert-todo-comment ()
  (interactive)
  (indent-for-tab-command)
  (insert "TODO: ")
  (back-to-indentation)
  (set-mark-command nil)
  (move-end-of-line nil)
  (comment-dwim nil))
(defun todo-comment-on-next-line ()
  "Insert a TODO comment on the next line at the proper indentation"
  (interactive)
  (move-end-of-line nil)
  (newline)
  (insert-todo-comment))
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "") #'todo-comment-on-next-line)))

;; copy from https://xenodium.com/emacs-create-a-swift-packageproject/
(defun ar/swift-package-init ()
  "Execute `swift package init', with optional name and completing type."
  (interactive)
  (let* ((name (read-string "name (default): "))
         (type (completing-read
                "project type: "
                ;; Splits "--type empty|library|executable|system-module|manifest"
                (split-string
                 (nth 1 (split-string
                         (string-trim
                          (seq-find
                           (lambda (line)
                             (string-match "--type" line))
                           (process-lines "swift" "package" "init" "--help")))
                         "   "))
                 "|")))
         (command (format "swift package init --type %s" type)))
    (unless (string-empty-p name)
      (append command "--name " name))
    (shell-command command))
  (dired default-directory)
  (revert-buffer))

(defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it. Similar to `start-process-shell-command', but calls `start-file-process'."
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (let ((command (mapconcat 'identity args " ")))
    (funcall start-file-process-shell-command name buffer command)))
(advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)

;; copy from http://xahlee.info/emacs/emacs/emacs_set_default_font_size.html
(defun xah-set-default-font-size ()
  "Set default font globally.
Note, this command change font size only for current session, not in init file.
This command useful for making font large when you want to do video livestream.
URL `http://xahlee.info/emacs/emacs/emacs_set_default_font_size.html'
Version: 2021-07-26 2021-08-21 2022-08-05"
  (interactive)
  (let (($fSize (read-string "size:" "16" nil "16")))
    (if (> (string-to-number $fSize) 51)
        (user-error "Max font size allowed is 51. You gave %s " $fSize)
      (progn
        (set-frame-font
         (cond
          ((string-equal system-type "windows-nt")
           (if (member "Consolas" (font-family-list)) (format "Consolas-%s" $fSize) nil))
          ((string-equal system-type "darwin")
           ;; (if (member "LXGW WenKai Mono" (font-family-list)) "LXGW WenKai Mono" nil))
           (if (member "EB Garamond" (font-family-list)) (format "EB Garamond 12 Italic %s" $fSize) nil))
          ((string-equal system-type "gnu/linux")
           (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono" nil))
          (t nil))
         t t)
        (set-face-attribute 'default nil :font  (format "EB Garamond 12 Italic %s" $fSize))
        (set-fontset-font "fontset-default"
                          'han (font-spec :family "LXGW WenKai Mono"
                                          :size (string-to-number $fSize)))
        (set-fontset-font "fontset-default"
                          'unicode (font-spec :family "LXGW WenKai Mono"
                                              :size (string-to-number $fSize)))
        (set-fontset-font "fontset-default"
                          'unicode-bmp (font-spec :family "LXGW WenKai Mono"
                                                  :size (string-to-number $fSize)))
        )
      )))



;; copy from https://www.emacswiki.org/emacs/SystemTrash
(setq delete-by-moving-to-trash t)
(defun system-move-file-to-trash (file)
  "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash-cli\"."
  (call-process (executable-find "trash-put")
		        nil 0 nil
		        file))

;; copy from https://github.com/d12frosted/homebrew-emacs-plus/issues/383
(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))
(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

;; copy from https://github.com/NapoleonWils0n/ubuntu-dotfiles/blob/f061fa1b05c5ccba8f6b4a2d165660ab8ab3c56b/.config/emacs/init.el#L170
;; openwth
(require 'mm-util)
(add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)

;; copy from https://emacs-china.org/t/magit-emacs-terminal-proxy/16942/2
(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost")
        socks-server '("Default server" "127.0.0.1" 10808 5))
  (setenv "all_proxy" "socks5://127.0.0.1:10808")
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (setenv "all_proxy" "")
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (require 'socks)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))


(setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows '(bar . 2))
(setq-default blink-cursor-blinks 50)
(setq-default blink-cursor-interval nil) ; 0.75 would be my choice
(setq-default blink-cursor-delay 0.2)
(blink-cursor-mode -1)
(define-minor-mode prot/cursor-type-mode
  "Toggle between static block and pulsing bar cursor."
  :init-value nil
  :global t
  (if prot/cursor-type-mode
      (progn
        (setq-local blink-cursor-interval 0.75
                    cursor-type '(bar . 2)
                    cursor-in-non-selected-windows 'hollow)
        (blink-cursor-mode 1))
    (dolist (local '(blink-cursor-interval
                     cursor-type
                     cursor-in-non-selected-windows))
      (kill-local-variable `,local))
    (blink-cursor-mode -1)))
;; copy from https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)
(defalias 'yes-or-no-p 'y-or-n-p)


(setq user-emacs-directory (file-truename "~/.emacs.d/"))


;; copy from https://blog.ginshio.org/2022/doom_emacs_configuration/#guix
(setq-default
 window-combination-resize t        ; 从其他窗口获取新窗口的大小
 x-stretch-cursor t                 ; 将光标拉伸到字形宽度
 )

(setq undo-limit 104857600         ; 重置撤销限制到 100 MiB
      auto-save-default t          ; 没有人喜欢丢失工作，我也是如此
      truncate-string-ellipsis "…" ; Unicode 省略号相比 ascii 更好
                                        ; 同时节省 /宝贵的/ 空间
      password-cache-expiry nil    ; 我能信任我的电脑 ... 或不能?
                                        ; scroll-preserve-screen-position 'always
                                        ; 不要让 `点' (光标) 跳来跳去
      scroll-margin 2              ; 适当保持一点点边距
      gc-cons-threshold 1073741824
      read-process-output-max 1048576
      )

(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
                                        ; 设置最大化启动
;;(display-time-mode t)             ; 开启时间状态栏
(require 'battery)
(when (and battery-status-function
           (not (string-match-p "N/A"
                                (battery-format "%B"
                                                (funcall battery-status-function)))))
  (display-battery-mode 1))         ; 知道还剩多少 ⚡️ 很重要

(global-subword-mode 1)             ; 识别驼峰，而不是傻瓜前进
(global-unset-key (kbd "C-z"))      ; 关闭 "C-z" 最小化
;;(define-key global-map "C-s" #'+default/search-buffer)
;;(map (:leader (:desc "load a saved workspace" :g "wr" #'+workspace/load))) ;; workspace load keybind


(custom-set-variables '(delete-selection-mode t) ;; delete when you select region and modify
                      '(delete-by-moving-to-trash t) ;; delete && move to transh
                      '(inhibit-compacting-font-caches t) ;; don’t compact font caches during GC.
                      '(gc-cons-percentage 1))

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace 1))) ; 编程模式下让结尾的空白符亮起

;; Show a marker when the line has empty characters at the end
(setq-default show-trailing-whitespace t)

;; Dark and transparent title bar in macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; copy from https://emacs.stackexchange.com/questions/6065/how-to-display-time-in-seconds-in-the-mode-line
(setq display-time-format "%Y-%m-%d %H:%M:%S")
(setq display-time-interval 1)
;; copy from https://codeantenna.com/a/ng3kV0ML9U
(display-time-mode 1) ;; 常显
(setq display-time-24hr-format t) ;;格式
(setq display-time-day-and-date t) ;;显示时间、星期、日期
;; copy from https://www.reddit.com/r/emacs/comments/kf3tsq/what_is_this_number_after_the_time_in_the_modeline/
(setq display-time-default-load-average nil)


;; copy from [launch love2d app from Emacs](https://gist.github.com/legumbre/38ef323645f17a3c8033)
(defvar love2d-program "/usr/local/bin/love")

(defun love2d-launch-current ()
  (interactive)
  (let ((app-root (locate-dominating-file (buffer-file-name) "main.lua")))
    (if app-root
        (shell-command (format "%s %s &" love2d-program app-root))
      (error "main.lua not found"))))


;; copy from [Emacs + Company-Mode 配置多个补全后端](https://manateelazycat.github.io/emacs/2021/06/30/company-multiple-backends.html)
;; Customize company backends.
(setq company-backends
      '(
        (company-tabnine company-dabbrev company-keywords company-files company-capf)
        ))

;; Add yasnippet support for all company backends.
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; Add `company-elisp' backend for elisp.
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (require 'company-elisp)
              (push 'company-elisp company-backends)))

;; Remove duplicate candidate.
(add-to-list 'company-transformers #'delete-dups)

(provide 'init-misc)
