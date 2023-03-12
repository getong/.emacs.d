;;; init-config-packages.el --- Summary config files -*- lexical-binding: t -*-

;;; Commentary:
;; config packages

;;; Code:

;;auto-compile
(use-package auto-compile
  :ensure t
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq load-prefer-newer t)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :ensure t)

;; async给emacs提供了elisp层面的异步支持, 避免长时间等待
(use-package async
  :config
  (setq async-bytecomp-allowed-packages '(all))
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  )
(use-package async-bytecomp
  :ensure async
  :custom
  (async-bytecomp-allowed-packages '(all))
  (byte-compile-verbose        nil)
  (byte-compile-warnings       '(not free-vars unresolved noruntime lexical make-local cl-functions))
  (async-byte-compile-log-file (no-littering-expand-var-file-name "async-bytecomp.log")))

;; ensure straight install all the need packages
(use-package f :ensure t)


;; hydra 更接近于「功能菜单」：弹出一个「常用功能列表」.
;; 你可以用连续击键来连续触发若干个函数。
(use-package use-package-hydra
  :ensure t
  :after hydra)

(use-package pretty-hydra
  :bind ("<f6>" . toggles-hydra/body)
  :hook (emacs-lisp-mode . (lambda ()
                             (add-to-list
                              'imenu-generic-expression
                              '("Hydras"
                                "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)"
                                2))))
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icon-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (with-no-warnings
    (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on" :v-adjust -0.1)
                                               :color amaranth :quit-key ("q" "C-g"))
      ("Basic"
       (("n" (cond ((fboundp 'display-line-numbers-mode)
                    (display-line-numbers-mode (if display-line-numbers-mode -1 1)))
                   ((fboundp 'gblobal-linum-mode)
                    (global-linum-mode (if global-linum-mode -1 1))))
         "line number"
         :toggle (or (bound-and-true-p display-line-numbers-mode)
                     (bound-and-true-p global-linum-mode)))
        ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
        ("d" global-hungry-delete-mode "hungry delete" :toggle t)
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("l" global-page-break-lines-mode "page break lines" :toggle t)
        ("b" display-battery-mode "battery" :toggle t)
        ("i" display-time-mode "time" :toggle t)
        ("m" doom-modeline-mode "modern mode-line" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line" :toggle t)
        ("h p" show-paren-mode "paren" :toggle t)
        ("h s" symbol-overlay-mode "symbol" :toggle t)
        ("h r" rainbow-mode "rainbow" :toggle t)
        ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h i" highlight-indent-guides-mode "indent" :toggle t)
        ("h t" global-hl-todo-mode "todo" :toggle t))
       "Program"
       (("f" flycheck-mode "flycheck" :toggle t)
        ("F" flymake-mode "flymake" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
        ("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       "Theme"
       (("t a" (centaur-load-theme 'auto) "auto"
         :toggle (eq centaur-theme 'auto) :exit t)
        ("t m" (centaur-load-theme 'random) "random"
         :toggle (eq centaur-theme 'random) :exit t)
        ("t s" (centaur-load-theme 'system) "system"
         :toggle (eq centaur-theme 'system) :exit t)
        ("t d" (centaur-load-theme 'default) "default"
         :toggle (centaur-theme-enable-p 'default) :exit t)
        ("t p" (centaur-load-theme 'pro) "pro"
         :toggle (centaur-theme-enable-p 'pro) :exit t)
        ("t k" (centaur-load-theme 'dark) "dark"
         :toggle (centaur-theme-enable-p 'dark) :exit t)
        ("t l" (centaur-load-theme 'light) "light"
         :toggle (centaur-theme-enable-p 'light) :exit t)
        ("t w" (centaur-load-theme 'warm) "warm"
         :toggle (centaur-theme-enable-p 'warm) :exit t)
        ("t c" (centaur-load-theme 'cold) "cold"
         :toggle (centaur-theme-enable-p 'cold) :exit t)
        ("t y" (centaur-load-theme 'day) "day"
         :toggle (centaur-theme-enable-p 'day) :exit t)
        ("t n" (centaur-load-theme 'night) "night"
         :toggle (centaur-theme-enable-p 'night) :exit t)
        ("t o" (ivy-read "Load custom theme: "
                         (all-completions "doom" (custom-available-themes))
                         :action (lambda (theme)
                                   (centaur-set-variable
                                    'centaur-theme
                                    (let ((x (intern theme)))
                                      (or (car (rassoc x centaur-theme-alist)) x)))
                                   (counsel-load-theme-action theme))
                         :caller 'counsel-load-theme)
         "others"
         :toggle (not (or (rassoc (car custom-enabled-themes) centaur-theme-alist)
                          (rassoc (cadr custom-enabled-themes) centaur-theme-alist)))
         :exit t))
       "Package Archive"
       (("p m" (centaur-set-package-archives 'melpa t)
         "melpa" :toggle (eq centaur-package-archives 'melpa) :exit t)
        ("p c" (centaur-set-package-archives 'emacs-cn t)
         "emacs-cn" :toggle (eq centaur-package-archives 'emacs-cn) :exit t)
        ("p b" (centaur-set-package-archives 'bfsu t)
         "bfsu" :toggle (eq centaur-package-archives 'bfsu) :exit t)
        ("p n" (centaur-set-package-archives 'netease t)
         "netease" :toggle (eq centaur-package-archives 'netease) :exit t)
        ("p s" (centaur-set-package-archives 'sjtu t)
         "sjtu" :toggle (eq centaur-package-archives 'sjtu) :exit t)
        ("p t" (centaur-set-package-archives 'tuna t)
         "tuna" :toggle (eq centaur-package-archives 'tuna) :exit t)
        ("p u" (centaur-set-package-archives 'ustc t)
         "ustc" :toggle (eq centaur-package-archives 'ustc) :exit t)
        ("p T" (centaur-test-package-archives) "speed test" :exit t))))))

;; benchmark-init records startup time by package so we can debug. It only records things after it's initialised, so put as early in config as possible.
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; (add-hook 'after-init-hook
;;           (lambda () (message "loaded in %s" (emacs-init-time))))

(use-package async-await
  :config
  (promise-rejection-tracking-enable '((all-rejections . t))))

;; 让 .emacs.d 更干净
;; no littering, keep .emacs.d clean
(use-package no-littering
  :ensure t
  :config
  (unless (file-directory-p (no-littering-expand-var-file-name "docsets/"))
    (make-directory (no-littering-expand-var-file-name "docsets/"))
    )
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (no-littering-expand-var-file-name "eln-cache"))))
  ;; (setq recentf-max-menu-items 5)
  )

;; copy from [Error when running magit-status: run-hooks: Wrong number of arguments](https://github.com/magit/magit/issues/3837)
(use-package transient
  :init
  ;; (setq transient-history nil)
  (setq transient-history-file  (no-littering-expand-var-file-name "transient/history.el"))
  :config
  ;; 执行 Mx outline-navigate 时，会出现一个菜单，你可以通过 p 和 n 键在 outline-mode（包括 org-mode！）中前进和后退，这个状态会一直持续到你停止它随着 Cg. 增加。
  (transient-define-prefix outline-navigate ()
    :transient-suffix     'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    [("p" "previous visible heading" outline-previous-visible-heading)
     ("n" "next visible heading" outline-next-visible-heading)])
  )

;; 显示行号
(use-package display-line-numbers
  :ensure t
  :config
  (global-display-line-numbers-mode 1)
  ;;(setq-default display-line-numbers-width 4)
  :custom-face
  (line-number ((t (:foreground "light green"))))
  (line-number-current-line ((t (:foreground "blue"))))
  :custom
  ;; Calculate max number to prevent shaking.
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t))

(use-package ispell
  :config
  (setq-default ispell-program-name "aspell")
  (ispell-change-dictionary "american" t))

;; https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/
;; Emacs次要模式，可提供良好的写作环境
(use-package olivetti
  :ensure t
  :hook
  (olivetti-mode . olivetti-mode-setup))

(use-package undohist
  :ensure t
  :config
  (setq undohist-ignored-files '("\\.git/COMMIT_EDITMSG$"))
  (undohist-initialize)
  )

;; Vundo exposes a visual tree of all the available undo paths
(use-package vundo
  :bind (("C-x u" . 'vundo)
         :map vundo-mode-map
         ("C-f" . vundo-forward)
         ("C-b" . vundo-backward)
         ("C-n" . vundo-next)
         ("C-p" . vundo-previous)
         ("C-a" . vundo-stem-root)
         ("C-e" . vundo-stem-end))
  :init (setq vundo-compact-display t)
  :config
  ;; Better contrasting highlight.
  ;; (custom-set-faces
  ;;  '(vundo-node ((t (:foreground "#808080"))))
  ;;  '(vundo-stem ((t (:foreground "#808080"))))
  ;;  '(vundo-highlight ((t (:foreground "#FFFF00")))))
  (setq vundo--window-max-height 5)
  ;; 是否需要回车确认
  (setq vundo-roll-back-on-quit t)
  ;; 头部显示
  ;; (setq  vundo-window-side 'top)
  (setq undohist-directory (no-littering-expand-var-file-name "undohist"))
  (undohist-initialize)
  (defun my/vundo-diff ()
    (interactive)
    (let* ((orig vundo--orig-buffer)
           (source (vundo--current-node vundo--prev-mod-list))
           (dest (vundo-m-parent source)))
      (if (or (not dest) (eq source dest))
          (message "vundo diff not available.")
	      (let ((buf (make-temp-name (concat (buffer-name orig) "-vundo-diff"))))
          (vundo--move-to-node source dest orig vundo--prev-mod-list)
          (with-current-buffer (get-buffer-create buf)
	          (insert-buffer orig))
          (vundo--refresh-buffer orig (current-buffer) 'incremental)
          (vundo--move-to-node dest source orig vundo--prev-mod-list)
          (vundo--refresh-buffer orig (current-buffer) 'incremental)
          (diff-buffers buf orig)
          (kill-buffer buf)))))
  (keymap-set vundo-mode-map "d" #'my/vundo-diff)
  )

;;
;; Multiple cursors
;;
;; Support for multiple cursor selection and editing.
;;
(use-package multiple-cursors
  :ensure t
  :bind (
         ;; 全选光标所在单词并在下一个单词增加一个光标。通常用来启动一个流程
         ("C->" . mc/mark-next-like-this-symbol)
         ;; 跳过当前单词并跳到下一个单词，和上面在同一个流程里。
         ("C-M->" . mc/skip-to-next-like-this)
         ;; 同样是开启一个多光标流程，但是是「向上找」而不是向下找。
         ("C-<" . mc/mark-previous-like-this-symbol)
         ;; 跳过当前单词并跳到上一个单词，和上面在同一个流程里。
         ("C-M-<" . mc/skip-to-previous-like-this)
         ;; 直接多选本 buffer 所有这个单词
         ("C-c C-. ."   . mc/mark-all-dwim)
         ("C-c C-. C-." . mc/mark-all-like-this-dwim)
         ("C-c C-. n"   . mc/mark-next-symbol-like-this)
         ("C-c C-. P"   . mc/mark-previous-symbol-like-this)
         ("C-c C-. A"   . mc/mark-all-symbols-like-this)
         ("C-c C-. f"   . mc/mark-all-like-this-in-defun)
         ;; 每行一个光标
         ("C-c C-. l"   . mc/edit-lines)
         ("C-c C-. e"   . mc/edit-ends-of-lines)
         ("C-c C-<" . mc/mark-all-like-this)
         ;; ("C->"     . mc/mark-next-like-this)
         ;; ("C-<"     . mc/mark-previous-like-this)
         ))

(use-package emacs
  ;; :commands prot/hidden-mode-line-mode

  ;; C-c l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.
  ;; :bind ("C-c L" . prot/scroll-centre-cursor-mode)

  :init
  ;; use super-save mode
  (setq auto-save-default nil)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; life is too short
  ;; (defalias 'yes-or-no-p 'y-or-n-p)
  ;; no tabs
  ;; (setq indent-tabs-mode nil)
  (setq-default indent-tabs-mode nil)
  ;; always open large size files
  (setq large-file-warning-threshold nil)
  ;; keep everything under vc
  ;; (setq make-backup-files nil)
  ;; no need to create lockfiles
  ;; (setq create-lockfiles nil)
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (set-charset-priority 'unicode) ;; utf8 in every nook and cranny
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  ;; (set-terminal-coding-system 'utf-8)
  ;; (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  ;; (prefer-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  ;; (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8)
  ;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything
  (global-unset-key (kbd "C-SPC")) ;; 输入法快捷键冲突
  (global-set-key (kbd "M-SPC") 'set-mark-command)
  (global-unset-key (kbd "C-z"))      ; 关闭 "C-z" 最小化

  ;; Don't persist a custom file
  ;; (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  ;; (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings
  ;; 禁止自动缩放窗口先
  (setq frame-inhibit-implied-resize t)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  ;; FIXME currently using tempel in org-mode triggers this warning
  ;; (setq warning-suppress-types (append warning-suppress-types '((org-element-cache))))

  ;; 显示括号匹配
  ;; (show-paren-mode t)
;;; copy from https://blog.ginshio.org/2022/doom_emacs_configuration/#guix
  (setq-default
   window-combination-resize t        ; 从其他窗口获取新窗口的大小
   x-stretch-cursor t                 ; 将光标拉伸到字形宽度
   )

  (setq
   undo-limit 104857600         ; 重置撤销限制到 100 MiB
   truncate-string-ellipsis "…" ; Unicode 省略号相比 ascii 更好
   ;; 同时节省 /宝贵的/ 空间
   password-cache-expiry nil    ; 我能信任我的电脑 ... 或不能?
   ;; 不要让 `点' (光标) 跳来跳去
   scroll-margin 2              ; 适当保持一点点边距
   gc-cons-threshold 1073741824
   read-process-output-max 1048576
   delete-selection-mode t  ;; delete when you select region and modify
   inhibit-compacting-font-caches t  ;; don’t compact font caches during GC.
   max-specpdl-size 10000
   max-lisp-eval-depth 10000
   ;; gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
   inhibit-startup-screen t
   visible-bell t;;关闭出错时的提示声
   show-paren-style 'parenthesis
   appt-issue-message t
   version-control t
   kept-old-versions 2
   kept-new-versions 5
   delete-old-versions t
   vc-follow-symlinks t
   ;; make cursor the width of the character it is under
   ;; i.e. full width of a TAB
   x-stretch-cursor t
   )

  ;; Hide commands in M-x which don't work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (call-interactively  #'toggle-frame-fullscreen)
  (defun highlight-all-buffers (keyword)
    "Highlight KEYWORD in all open buffers and remove previous highlights."
    (interactive "sKeyword: ")
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (unhighlight-regexp t)
        (highlight-regexp keyword))))
  ;; disable menu bar, tool-bar
  (push '(menu-bar-lines . 0)   default-frame-alist)
  (push '(tool-bar-lines . 0)   default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)

  (global-superword-mode t)
  ;;自动折行设置
  (toggle-truncate-lines 1)
  (mouse-avoidance-mode 'animate) ;;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。


  ;; 对齐插入空格而不是tab
  ;; copy from http://stackoverflow.com/questions/22710040/emacs-align-regexp-with-spaces-instead-of-tabs
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it))

  ;; copy from https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
  ;; start the initial frame maximized
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; start every frame maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'auto-fill-mode)
  (add-hook 'window-setup-hook #'toggle-frame-fullscreen)

  ;; Dark and transparent title bar in macOS
  (when (memq window-system '(mac ns))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  ;; copy from https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
  ;; 添加时间戳到message消息
  (defun sh/current-time-microseconds ()
	  "Return the current time formatted to include microseconds."
	  (let* ((nowtime (current-time))
           (now-ms (nth 2 nowtime)))
      (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

  (defun sh/ad-timestamp-message (FORMAT-STRING &rest args)
	  "Advice to run before `message' that prepends a timestamp to each message.
Activate this advice with:
(advice-add 'message :before 'sh/ad-timestamp-message)"
	  (unless (string-equal FORMAT-STRING "%s%s")
      (let ((deactivate-mark nil)
			      (inhibit-read-only t))
		    (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (if (not (bolp))
			        (newline))
          (insert (sh/current-time-microseconds) " ")))))

  (advice-add 'message :before 'sh/ad-timestamp-message)

  (defun my-reload-emacs-configuration ()
    (interactive)
    (load-file (expand-file-name "early-init.el" user-emacs-directory))
    (load-file (expand-file-name "init.el" user-emacs-directory))
    )
  ;; By default emacs will not delete selection text when typing on it, let's fix it
  (delete-selection-mode t)
  ;; 默认情况下，Emacs 为每个打开的文件创建一些临时的文件，这会搞乱我们的目录，不需要它。
  ;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
  ;; as a buffer is unsaved, backups create copies once, when the file is first
  ;; written, and never again until it is killed and reopened. This is better
  ;; suited to version control, and I don't want world-readable copies of
  ;; potentially sensitive material floating around our filesystem.
  (setq
   ;; But in case the user does enable it, some sensible defaults:
   version-control t     ; number each backup file
   backup-by-copying t   ; instead of renaming current file (clobbers links)
   delete-old-versions t ; clean up after itself
   kept-old-versions 5
   kept-new-versions 5)
  ;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
  ;; Use `recover-file' or `recover-session' to recover them.
  ;; copy from https://stackoverflow.com/questions/15302973/emacs-auto-save-why-are-files-not-stored-in-the-correct-folder
  ;; (defvar my-auto-save-folder "~/.emacs.d/var/auto-save/"); folder for auto-saves
  ;; (setq auto-save-default t
  ;;       ;; Don't auto-disable auto-save after deleting big chunks. This defeats
  ;;       ;; the purpose of a failsafe. This adds the risk of losing the data we
  ;;       ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
  ;;       auto-save-include-big-deletions t
  ;;       auto-save-file-name-transforms
  ;;       (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
  ;;                   ;; Prefix tramp autosaves to prevent conflicts with local ones
  ;;                   (concat auto-save-list-file-prefix "tramp-\\2") t)
  ;;             (list ".*" auto-save-list-file-prefix t)))
  (setq mode-line-percent-position '(-3 "%p"))
  (setq mode-line-defining-kbd-macro
        (propertize " Macro" 'face 'mode-line-emphasis))
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "  "
                  mode-line-position
                  (vc-mode vc-mode)
                  " "
                  mode-line-modes
                  " "
                  mode-line-misc-info
                  mode-line-end-spaces))
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  ;; copy from [Emacs on Mac OS X - To Alt or Command?](https://apple.stackexchange.com/questions/12087/emacs-on-mac-os-x-to-alt-or-command)
  ;; copy from [emacs-mac-port的command key能不能改回系统默认的command功能？](https://emacs-china.org/t/emacs-mac-port-command-key-command/8845)
  ;; check OS type
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    (progn
      (message "Microsoft Windows")))
   ((string-equal system-type "darwin") ; Mac OS X
    (progn
      (setq mac-option-modifier 'meta)
      (setq mac-command-modifier 'super)
      (setq ns-option-modifier 'meta)
      (setq ns-command-modifier 'super)
      ;; Here are some Nextstep-like bindings for command key sequences.
      (define-key global-map [?\s-,] 'customize)
      (define-key global-map [?\s-'] 'next-window-any-frame)
      (define-key global-map [?\s-`] 'other-frame)
      (define-key global-map [?\s-~] 'ns-prev-frame)
      (define-key global-map [?\s--] 'center-line)
      (define-key global-map [?\s-:] 'ispell)
      (define-key global-map [?\s-?] 'info)
      (define-key global-map [?\s-^] 'kill-some-buffers)
      (define-key global-map [?\s-&] 'kill-current-buffer)
      (define-key global-map [?\s-C] 'ns-popup-color-panel)
      (define-key global-map [?\s-D] 'dired)
      (define-key global-map [?\s-E] 'edit-abbrevs)
      (define-key global-map [?\s-L] 'shell-command)
      (define-key global-map [?\s-M] 'manual-entry)
      (define-key global-map [?\s-S] 'ns-write-file-using-panel)
      (define-key global-map [?\s-a] 'mark-whole-buffer)
      (define-key global-map [?\s-c] 'ns-copy-including-secondary)
      (define-key global-map [?\s-d] 'isearch-repeat-backward)
      (define-key global-map [?\s-e] 'isearch-yank-kill)
      (define-key global-map [?\s-f] 'isearch-forward)
      (define-key global-map [?\s-g] 'isearch-repeat-forward)
      (define-key global-map [?\s-h] 'ns-do-hide-emacs)
      (define-key global-map [?\s-H] 'ns-do-hide-others)
      (define-key global-map [?\M-\s-h] 'ns-do-hide-others)
      (define-key global-map [?\s-j] 'exchange-point-and-mark)
      (define-key global-map [?\s-k] 'kill-current-buffer)
      (define-key global-map [?\s-l] 'goto-line)
      (define-key global-map [?\s-m] 'iconify-frame)
      (define-key global-map [?\s-n] 'make-frame)
      (define-key global-map [?\s-o] 'ns-open-file-using-panel)
      (define-key global-map [?\s-p] 'ns-print-buffer)
      (define-key global-map [?\s-q] 'save-buffers-kill-emacs)
      (define-key global-map [?\s-s] 'save-buffer)
      (define-key global-map [?\s-t] 'ns-popup-font-panel)
      (define-key global-map [?\s-u] 'revert-buffer)
      (define-key global-map [?\s-v] 'yank)
      (define-key global-map [?\s-w] 'delete-frame)
      (define-key global-map [?\s-x] 'kill-region)
      (define-key global-map [?\s-y] 'ns-paste-secondary)
      (define-key global-map [?\s-z] 'undo)
      (define-key global-map [?\s-+] 'text-scale-adjust)
      (define-key global-map [?\s-=] 'text-scale-adjust)
      (define-key global-map [?\s--] 'text-scale-adjust)
      (define-key global-map [?\s-0] 'text-scale-adjust)
      (define-key global-map [?\s-|] 'shell-command-on-region)
      (define-key global-map [s-kp-bar] 'shell-command-on-region)
      (define-key global-map [?\C-\s- ] 'ns-do-show-character-palette)
      (message "Mac OS X")))
   ((string-equal system-type "gnu/linux") ; linux
    (progn
      (message "Linux"))))

  ;; Autoindent open-*-lines
  (defvar newline-and-indent t
    "Modify the behavior of the open-*-line functions to cause them to autoindent.")

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

  (global-set-key (kbd "C-c o") 'open-next-line)
  (global-set-key (kbd "C-c z") 'toggle-frame-fullscreen)

  ;; Behave like vi's O command
  (defun open-previous-line (arg)
    "Open a new line before the current one.
  See also `newline-and-indent'."
    (interactive "p")
    (beginning-of-line)
    (open-line arg)
    (when newline-and-indent
      (indent-according-to-mode)))
  (global-set-key (kbd "C-c O") 'open-previous-line)

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

  ;; 根据条件删除行尾的空白
  (when newline-and-indent
    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    ;; 编程模式下让结尾的空白符亮起
    ;; (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace 1)))

    ;; Show a marker when the line has empty characters at the end
    ;; (setq-default show-trailing-whitespace t)
    )

  ;; 关闭所有的buffer
  (defun close-all-buffers ()
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

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

  ;; copy from http://xahlee.info/emacs/emacs/emacs_set_default_font_size.html
  (defun xah-set-default-font-size ()
    "Set default font globally.
Note, this command change font size only for current session, not in init file.
This command useful for making font large when you want to do video livestream.
URL `http://xahlee.info/emacs/emacs/emacs_set_default_font_size.html'
Version: 2021-07-26 2021-08-21 2022-08-05"
    (interactive)
    (let (($fSize (read-string "size:" "12" nil "12")))
      (if (> (string-to-number $fSize) 51)
          (user-error "Max font size allowed is 51. You gave %s " $fSize)
        (progn
          (set-frame-font
           (cond
            ((string-equal system-type "windows-nt")
             (if (member "Consolas" (font-family-list)) (format "Consolas-%s" $fSize) nil))
            ((string-equal system-type "darwin")
             ;; (if (member "LXGW WenKai Mono" (font-family-list)) "LXGW WenKai Mono" nil))
             ;; (if (member "EB Garamond" (font-family-list)) (format "EB Garamond 12 Italic %s" $fSize) nil))
             (if (member "JetBrains Mono" (font-family-list)) (format "JetBrains Mono %s" $fSize) nil))
            ((string-equal system-type "gnu/linux")
             (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono" nil))
            (t nil))
           t t)
          ;; (set-face-attribute 'default nil :font  (format "EB Garamond 12 Italic %s" $fSize))
          (set-face-attribute 'default nil :font  (format "JetBrains Mono %s" $fSize))
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

  ;; copy from [Aligning columns in Emacs](https://blog.lambda.cx/posts/emacs-align-columns/)
  (defun align-non-space (BEG END)
    "Align non-space columns in region BEG END."
    (interactive "r")
    (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

  (defun xah-convert-fullwidth-chars (Begin End &optional ToDirection)
    "Convert ASCII chars to/from Unicode fullwidth version.
Works on current line or text selection.

The conversion direction is determined like this: if the command has been repeated, then toggle. Else, always do to-Unicode direction.

If `universal-argument' is called first:

 no C-u → Automatic.
 C-u → to ASCII
 C-u 1 → to ASCII
 C-u 2 → to Unicode

When called in lisp code, Begin End are region begin/end positions. ToDirection must be any of the following values: 「\"unicode\"」, 「\"ascii\"」, 「\"auto\"」.

URL `http://xahlee.info/emacs/emacs/elisp_convert_chinese_punctuation.html'
Version: 2018-08-02 2022-05-18"
    (interactive
     (let ($p1 $p2)
       (if (use-region-p)
           (setq $p1 (region-beginning) $p2 (region-end))
         (setq $p1 (line-beginning-position) $p2 (line-end-position)))
       (list $p1 $p2
             (cond
              ((equal current-prefix-arg nil) "auto")
              ((equal current-prefix-arg '(4)) "ascii")
              ((equal current-prefix-arg 1) "ascii")
              ((equal current-prefix-arg 2) "unicode")
              (t "unicode")))))
    (let* (($ascii-unicode-map
            [
             ["0" "０"] ["1" "１"] ["2" "２"] ["3" "３"] ["4" "４"] ["5" "５"] ["6" "６"] ["7" "７"] ["8" "８"] ["9" "９"]
             ["A" "Ａ"] ["B" "Ｂ"] ["C" "Ｃ"] ["D" "Ｄ"] ["E" "Ｅ"] ["F" "Ｆ"] ["G" "Ｇ"] ["H" "Ｈ"] ["I" "Ｉ"] ["J" "Ｊ"] ["K" "Ｋ"] ["L" "Ｌ"] ["M" "Ｍ"] ["N" "Ｎ"] ["O" "Ｏ"] ["P" "Ｐ"] ["Q" "Ｑ"] ["R" "Ｒ"] ["S" "Ｓ"] ["T" "Ｔ"] ["U" "Ｕ"] ["V" "Ｖ"] ["W" "Ｗ"] ["X" "Ｘ"] ["Y" "Ｙ"] ["Z" "Ｚ"]
             ["a" "ａ"] ["b" "ｂ"] ["c" "ｃ"] ["d" "ｄ"] ["e" "ｅ"] ["f" "ｆ"] ["g" "ｇ"] ["h" "ｈ"] ["i" "ｉ"] ["j" "ｊ"] ["k" "ｋ"] ["l" "ｌ"] ["m" "ｍ"] ["n" "ｎ"] ["o" "ｏ"] ["p" "ｐ"] ["q" "ｑ"] ["r" "ｒ"] ["s" "ｓ"] ["t" "ｔ"] ["u" "ｕ"] ["v" "ｖ"] ["w" "ｗ"] ["x" "ｘ"] ["y" "ｙ"] ["z" "ｚ"]
             ["," "，"] ["." "．"] [":" "："] [";" "；"] ["!" "！"] ["?" "？"] ["\"" "＂"] ["'" "＇"] ["`" "｀"] ["^" "＾"] ["~" "～"] ["¯" "￣"] ["_" "＿"]
             [" " "　"]
             ["&" "＆"] ["@" "＠"] ["#" "＃"] ["%" "％"] ["+" "＋"] ["-" "－"] ["*" "＊"] ["=" "＝"] ["<" "＜"] [">" "＞"] ["(" "（"] [")" "）"] ["[" "［"] ["]" "］"] ["{" "｛"] ["}" "｝"] ["(" "｟"] [")" "｠"] ["|" "｜"] ["¦" "￤"] ["/" "／"] ["\\" "＼"] ["¬" "￢"] ["$" "＄"] ["£" "￡"] ["¢" "￠"] ["₩" "￦"] ["¥" "￥"]
             ]
            )
           ($reverse-map
            (mapcar
             (lambda (x) (vector (elt x 1) (elt x 0)))
             $ascii-unicode-map))

           ($stateBefore
            (if (get 'xah-convert-fullwidth-chars 'state)
                (get 'xah-convert-fullwidth-chars 'state)
              (progn
                (put 'xah-convert-fullwidth-chars 'state 0)
                0
                )))
           ($stateAfter (if (eq $stateBefore 0) 1 0)))

                                        ;"０\\|１\\|２\\|３\\|４\\|５\\|６\\|７\\|８\\|９\\|Ａ\\|Ｂ\\|Ｃ\\|Ｄ\\|Ｅ\\|Ｆ\\|Ｇ\\|Ｈ\\|Ｉ\\|Ｊ\\|Ｋ\\|Ｌ\\|Ｍ\\|Ｎ\\|Ｏ\\|Ｐ\\|Ｑ\\|Ｒ\\|Ｓ\\|Ｔ\\|Ｕ\\|Ｖ\\|Ｗ\\|Ｘ\\|Ｙ\\|Ｚ\\|ａ\\|ｂ\\|ｃ\\|ｄ\\|ｅ\\|ｆ\\|ｇ\\|ｈ\\|ｉ\\|ｊ\\|ｋ\\|ｌ\\|ｍ\\|ｎ\\|ｏ\\|ｐ\\|ｑ\\|ｒ\\|ｓ\\|ｔ\\|ｕ\\|ｖ\\|ｗ\\|ｘ\\|ｙ\\|ｚ"

      (let ((case-fold-search nil))
        (xah-replace-pairs-region
         Begin End
         (cond
          ((string-equal ToDirection "unicode") $ascii-unicode-map)
          ((string-equal ToDirection "ascii") $reverse-map)
          ((string-equal ToDirection "auto")
           (if (eq $stateBefore 0)
               $reverse-map
             $ascii-unicode-map))
          (t (user-error "Your 3rd argument 「%s」 isn't valid" ToDirection)))
         t t))
      (put 'xah-convert-fullwidth-chars 'state $stateAfter)))

  ;; 全角转半角 M-x translate-region chunyang-fullwidth-to-halfwidth
  ;; 半角转全角 M-x translate-region chunyang-halfwidth-to-fullwidth
  (define-translation-table 'chunyang-fullwidth-to-halfwidth
    (let ((table (make-char-table 'translation-table)))
      (cl-loop for fullwidth from #xFF01 to #xFF5E
               for halfwidth from #x21 to #x7E
               do (aset table fullwidth halfwidth))
      table))

  (define-translation-table 'chunyang-halfwidth-to-fullwidth
    (let ((table (make-char-table 'translation-table)))
      (cl-loop for fullwidth from #xFF01 to #xFF5E
               for halfwidth from #x21 to #x7E
               do (aset table halfwidth fullwidth))
      table))
  )



(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4" "m4v"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv" "webm"))
               "VLC"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg" "jpg" "webp"))
               "nsxiv -a"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "zathura"
               '(file)))))

(use-package pangu-spacing
  :config
  (global-pangu-spacing-mode 1)
  )


(use-package highlight-thing
  :config
  (global-highlight-thing-mode)
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-delay-seconds 0.1)
  (setq highlight-thing-limit-to-defun t)
  (setq highlight-thing-case-sensitive-p t)
  )


(use-package fennel-mode
  :ensure t)

(use-package reformatter
  :ensure t
  :config
  (reformatter-define lua-format
    :program "stylua"
    :args '("-")
    :group 'lua)

  (reformatter-define js-format
    :program "npx"
    :args '("prettier" "--stdin-filepath" "a.js"))
  ;; SQL formatter
  (reformatter-define sql-format
    :program "pg_format")

  ;; XML formatter
  (reformatter-define xml-format
    :program "xmlformat"
    :group 'xml)
  ;; Dart format
  (reformatter-define dart-format
    :program "dart"
    :args '("format")
    :group 'dart)
  )


;;  rainbow-delimiters 可以将对称的括号用同一种颜色标记出来。
;; parens
(use-package smartparens
  :ensure t
  :diminish
  smartparens-mode
  :hook
  (after-prog-mode . smartparens-mode))

(use-package smartparens-config
  :diminish nil
  :ensure smartparens
  :hook
  (prog-mode-hook . turn-on-smartparens-strict-mode)
  :config (progn (show-smartparens-global-mode t)))

(use-package paren
  :config
  (setq show-paren-delay 0.1
        show-paren-when-point-in-periphery t))


;; copy from https://se30.xyz/conf.html
(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :commands (rainbow-delimiters-mode rainbow-delimiters-mode-enable)
  :config
  (defun alexm/set-faces-by-spec (&rest specs)
    "Maps SPECS through face-spec-set."
    (mapc #'(lambda (f) (apply #'face-spec-set f)) specs))
  (alexm/set-faces-by-spec
   '(rainbow-delimiters-depth-1-face ((t (:foreground "green" :weight extra-bold))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "forestgreen" :weight bold))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "lightseagreen" :weight bold))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "lightskyblue" :weight bold))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "cyan" :weight bold))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "steelblue" :weight bold))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "orchid" :weight bold))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "purple" :weight bold))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "hotpink" :weight bold))))
   '(rainbow-delimiters-unmatched-face ((t (:background "green" :foreground "blue" :weight bold)))))
  :hook
  ((css-mode . rainbow-delimiters-mode)
   (sass-mode . rainbow-delimiters-mode)
   (scss-mode . rainbow-delimiters-mode)
   (rust-mode . rainbow-delimiters-mode)
   (prog-mode . rainbow-delimiters-mode)))

(use-package highlight-indent-guides
  :ensure t
  :custom (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (elisp-mode . highlight-indent-guides-mode)
  (rust-mode . highlight-indent-guides-mode)
  (rustic-mode . highlight-indent-guides-mode)
  )


(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode)
  )

;; enable ob-tmux by see [org-mode + vterm + tmux == ❤️❤️❤️](https://www.reddit.com/r/emacs/comments/xyo2fo/orgmode_vterm_tmux/)
(use-package ob-tmux
  ;; Install package automatically (optional)
  :ensure t
  :custom
  (org-babel-default-header-args:tmux
   '((:results . "silent")	;
     (:session . "default")	; The default tmux session to send code to
     (:socket  . nil)))		; The default tmux socket to communicate with
  ;; The tmux sessions are prefixed with the following string.
  ;; You can customize this if you like.
  (org-babel-tmux-session-prefix "ob-")
  ;; The terminal that will be used.
  ;; You can also customize the options passed to the terminal.
  ;; The default terminal is "gnome-terminal" with options "--".
  (org-babel-tmux-terminal "/Applications/iTerm.app/Contents/MacOS/iTerm2")
  ;; (org-babel-tmux-terminal "alacritty")
  (org-babel-tmux-terminal-opts '("-t" "ob-tmux" "-e"))
  ;; Finally, if your tmux is not in your $PATH for whatever reason, you
  ;; may set the path to the tmux binary as follows:
  (org-babel-tmux-location (executable-find "tmux")))

;; copy from https://emacs.stackexchange.com/questions/31872/how-to-update-packages-installed-with-use-package
;; With that setup, packages will be updated every 4 days, and the old packages will be removed.
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4
        auto-package-update-hide-results t
        )
  (auto-package-update-maybe))


;; copy from https://quant67.com/post/emcas/init-config.html
;; 默认的 mode-line 不是很好看，用 doom-modeline 好一些。
;; (use-package all-the-icons
;;   :if (display-graphic-p)
;;   ;; (set-fontset-font t 'symbol "Apple Color Emoji")
;;   ;; (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
;;   ;; (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
;;   ;; (set-fontset-font t 'symbol "Symbola" nil 'append)
;;   :config
;;   (when (string-equal system-type "gnu/linux");
;;     (setq all-the-icons-fonts-subdirectory (no-littering-expand-var-file-name "all-the-icons")))
;;   ;;(unless (file-exists-p all-the-icons-fonts-subdirectory)
;;   ;;    (call-interactively #'all-the-icons-install-fonts))
;;   )

;; (use-package all-the-icons-completion
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))


;; https://www.emacswiki.org/emacs/KeyCast
;; copy from https://book.emacs-china.org/#org737719a
;; ;;modeline上显示我的所有的按键和执行的命令
;; https://sqrtminusone.xyz/configs/emacs/
(use-package keycast
  :ensure t
  :config
  (keycast-mode)
  (define-minor-mode keycast-mode
    "Keycast mode"
    :global t
    (if keycast-mode
	      (progn
	        (add-to-list 'global-mode-string '("" keycast-mode-line " "))
	        (add-hook 'pre-command-hook 'keycast--update t) )
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string)))))
;; 在后面，上面在前面
;; (use-package keycast
;;   :commands bg/toggle-keycast
;;   :config
;;   (defun bg/toggle-keycast()
;;     (interactive)
;;     (if (member '("" keycast-mode-line " ") global-mode-string)
;;         (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
;;                (remove-hook 'pre-command-hook 'keycast--update)
;;                (message "Keycast OFF"))
;;       (add-to-list 'global-mode-string '("" keycast-mode-line " "))
;;       (add-hook 'pre-command-hook 'keycast--update t)
;;       (message "Keycast ON"))))

(use-package shrink-path
  :ensure t)

;; 这里的执行顺序非常重要，doom-modeline-mode 的激活时机一定要在设置global-mode-string 之后‘
(use-package doom-modeline
  :straight (doom-modeline :type git :host github :repo "seagle0128/doom-modeline")
  :ensure t
  :defer t
  :init
  (doom-modeline-mode 1)
  (setq doom-moeline-time nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-height 15)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-buffer-file-name-style 'auto)
  :config
  (setq doom-modeline-battery nil)
  ;; (doom-modeline-mode 1)
  (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "orange")
  )

;; M-x all-the-icons-install-fonts
;; copy from [Why do I have Chinese/Mandarin characters in my mode-line and e-shell out of the blue? How do I fix this?](https://emacs.stackexchange.com/questions/73397/why-do-i-have-chinese-mandarin-characters-in-my-mode-line-and-e-shell-out-of-the)

;; copy from https://quant67.com/post/emcas/init-config.html
;; 让 Emacs 识别文件在项目里
;;projectile 提供了这个功能。 C-c c-p 会列举它的快捷键，其中包括在项目中搜索，切换项目等。
(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-keymap-prefix (kbd "C-c C-p"))
  :config
  (projectile-mode +1)
  ;; M-x projectile-purge-file-from-cache
  ;; M-x projectile-purge-dir-from-cache
  (progn
    (setq
     projectile-enable-caching t
     projectile-sort-order 'recently-active
     projectile-completion-system 'default
     ))
  (setq-default projectile-mode-line-prefix " Proj")
  (projectile-global-mode))


;; copy from [Highlight current active window](https://stackoverflow.com/questions/33195122/highlight-current-active-window)
;; auto dim / dimmer
(use-package auto-dim-other-buffers
  :ensure t
  :diminish auto-dim-other-buffers-mode
  :init (add-hook 'after-init-hook (lambda ()
                                     (when (fboundp 'auto-dim-other-buffers-mode)
                                       (auto-dim-other-buffers-mode t))))
  :config
  (set-face-attribute 'auto-dim-other-buffers-face nil :background "#42444C"))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

;; Share clipoard with OS
(use-package pbcopy
  :ensure t)



(with-eval-after-load 'nxml-mode
  (define-key nxml-mode-map (kbd "C-c C-f") 'xml-format-buffer))

;; Incremental code parsing for better syntax highlighting
(use-package tree-sitter
  :ensure t
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; Org tree slide
(use-package hide-mode-line
  :ensure t)

;; Cursor 光标彩虹效果
(use-package beacon
  :ensure t
  :custom
  (beacon-color "blue")
  :config
  (setq-default beacon-size 15)
  (add-hook 'after-init-hook 'beacon-mode)
  (beacon-mode 1))

;; C-x C-f /method:user@host:path/to/file
;; /sshx:vagrant@192.168.31.92:/etc/hosts
;; C-x C-f /sshx:bird@bastion|ssh:admin@production:/path
;; C-x C-f /sshx:you@remotehost|sudo::/path RET
;; /ssh:username@hostname#port:/path/to/file
;; /ssh:tarou@remotehost#10022:~/
;; /ssh:username@hostname|sudo:usename:/path/to/file
;; Tramp should default to the sshx mode.
(use-package tramp
  ;; :commands tramp
  :config
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                     "direct-async-process" t))
  ;; Tips to speed up connections
  (setq tramp-verbose 6
        tramp-chunksize 2000
        tramp-use-ssh-controlmaster-options nil
        tramp-default-method "sshx"
        ;; Skip looking for dir-local on remote system to speed up tramp.
        enable-remote-dir-locals nil
        ;; Preserve PATH on remote host.
        tramp-remote-path (delete 'tramp-default-remote-path tramp-remote-path)
        remote-file-name-inhibit-cache 120
        tramp-auto-save-directory (no-littering-expand-var-file-name "tramp-autosave")
        tramp-persistency-file-name (no-littering-expand-var-file-name "tramp")
        vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules")
        )
  )

(use-package s)

(use-package dash
  :defer t)


;; copy from https://emacs-china.org/t/purcell-emacs-emacs/17511/13
(use-package desktop
  :commands restart-emacs-without-desktop
  :init (desktop-save-mode)
  :config
  ;; inhibit no-loaded prompt
  (setq desktop-file-modtime (file-attribute-modification-time
                              (file-attributes
                               (desktop-full-file-name)))
        desktop-lazy-verbose nil
        desktop-load-locked-desktop t
        desktop-restore-eager 1
        desktop-restore-frames nil
        desktop-save t)

  (defun restart-emacs-without-desktop (&optional args)
    "Restart emacs without desktop."
    (interactive)
    (restart-emacs (cons "--no-desktop" args))))

;; copy from https://emacs-china.org/t/zoom/22957
;; zoom: 窗口管理插件，自动调整窗口布局
(use-package zoom
  :config
  (custom-set-variables
   '(zoom-mode t))
  (defun size-callback ()
    (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
          (t                            '(0.5 . 0.5))))
  (custom-set-variables
   '(zoom-size 'size-callback))
  (custom-set-variables
   '(zoom-ignored-major-modes '(dired-mode markdown-mode))
   '(zoom-ignored-buffer-names '("zoom.el" "init.el"))
   '(zoom-ignored-buffer-name-regexps '("^*calc"))
   '(zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20)))))
  )

;; zoom-window provides window zoom like tmux zoom and unzoom.
;; C-x C-z 可以把当前窗口最大化
(use-package zoom-window
  :config
  (require 'zoom-window)
  (global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
  (custom-set-variables
   '(zoom-window-mode-line-color "DarkGreen"))
  )

;; copy from https://emacs-china.org/t/emacs-builtin-mode/11937
;; winner-mode 是一个全局的 minor mode。它的主要功能是记录窗体的变动。
;; 例如当前有2 个窗口，然后你关了一个，这时可以通过 winner-undo 来恢复。
;; 还可以再 winner-redo 来撤销刚才的 undo.
;; (C-c <Left>) winner-undo
;; (C-c <Right>) winner-redo
(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode))

;; 它也可以应用在 ediff 上，恢复由 ediff 导致的窗体变动。
(use-package eiff
  :ensure nil
  :hook (ediff-quit . winner-undo))

;;  saveplace 记录了上次打开文件时 cursor 停留在第几行、第几列。
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; 高亮当前行。
(use-package hl-line
  :ensure t
  ;; :hook (after-init . global-hl-line-mode)
  :config
  (global-hl-line-mode 1)
  (setq global-hl-line-sticky-flag t)
  ;; copy from [hl-line-mode hide background, how to avoid this?](https://emacs.stackexchange.com/questions/10445/hl-line-mode-hide-background-how-to-avoid-this)
  (defun my-hl-line-range-function ()
    (cons (line-end-position) (line-beginning-position 2)))
  (setq hl-line-range-function #'my-hl-line-range-function)

  :custom-face
  (hl-line ((nil (:background "light green"))))
  :hook
  ((after-init . global-hl-line-mode)
   ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
    (lambda () (setq-local global-hl-line-mode nil))))
  )

;; 隐藏、显示结构化数据，如 { } 里的内容。对于单函数较长的情况比较有用。
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
              ("C-c TAB" . hs-toggle-hiding)
              ("M-+" . hs-show-all))
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-special-modes-alist
   (mapcar 'purecopy
           '((c-mode "{" "}" "/[*/]" nil nil)
             (c++-mode "{" "}" "/[*/]" nil nil)
             (rust-mode "{" "}" "/[*/]" nil nil)))))

;; 当打开一个具有长行的文件时，它会自动检测并将一些可能导致严重性能的 mode 关闭， 如 syntax highlight。
;; Solong mode
;; improves performance in large files by disabling some modes
(use-package so-long
  :hook (prog-mode . global-so-long-mode)
  :config
  (setq so-long-threshold 400)
  ;; don't disable font-lock-mode, line-number-mode and don't make buffer read only
  (delq 'font-lock-mode so-long-minor-modes)
  (delq 'display-line-numbers-mode so-long-minor-modes)
  (delq 'buffer-read-only so-long-variable-overrides)
  ;; reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; disable save-place in large files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; disable these
  (append so-long-minor-modes
          '(eldoc-mode
            auto-composition-mode
            undo-tree-mode
            hl-fill-column-mode)))

;; 有时候Emacs里打开的文件可能被外部修改，启用autorevert的话可以自动更新对应的 buffer.
(use-package autorevert
  :ensure t
  :hook (after-init . global-auto-revert-mode))

;; 来显示如 10/100 这种状态。
;; 在搜索中删除字符会回退搜索结果，而不是停在当前位置将最后一个搜 索字符删除。这里可以通过remap isearch-delete-char来实现。
;; 还可以将搜索结果保持在高亮状态以方便肉眼识别。这个是通过设置 lazy-highlight-cleanup为nil实现的。
;; 去除高亮状态需要人工M-x调用 lazy-highlight-cleanup。
;; (use-package isearch
;;   :ensure nil
;;   :bind (:map isearch-mode-map
;;               ([remap isearch-delete-char] . isearch-del-char))
;;   :custom
;;   (isearch-lazy-count t)
;;   (lazy-count-prefix-format "%s/%s ")
;;   (lazy-highlight-cleanup nil))

;; 打开这个 mode 以后就能正确地处理驼峰命名中的单词了。
;; (use-package subword
;;   :hook (after-init . global-subword-mode))

;; 如果你想要一个足够简单的注释与反注释功能，那么自带的newcomment就可以做到。
;; 当用户选中区间时，在对应区间上注释或者反注释
;; 如果当前行是空的，那么会插入一个注释并且将它对齐 (偷懒，直接调用了comment-dwim)
;; 其他情况则对当前行注释或者反注释
(use-package newcomment
  :ensure nil
  :bind
  ([remap comment-dwim] . #'comment-or-uncomment)
  ("C-x C-;" . comment-dwim)
  :config
  (defun comment-or-uncomment ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (call-interactively 'comment-dwim)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  (comment-auto-fill-only-comments t))


;; typescript
;; copy from https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; copy from [极简Emacs开发环境配置](https://huadeyu.tech/tools/emacs-setup-notes.html)
;; Json
(use-package json-mode)

;; Yaml
(use-package yaml-mode)

;; Dockfile
(use-package dockerfile-mode)

;; Protobuf
(use-package protobuf-mode)

;; (use-package yasnippet
;;   :ensure
;;   :config
;;   (yas-reload-all)
;;   (add-hook 'prog-mode-hook 'yas-minor-mode)
;;   (add-hook 'text-mode-hook 'yas-minor-mode))





(use-package toml-mode
  :ensure t
  :mode (("\\.toml\\'" . toml-mode)
         ("/Pipfile\\'" . toml-mode)))

;; setting up debugging support with dap-mode
;; (when (executable-find "lldb-mi")
(use-package dap-mode
  :commands rgr/dap-debug
  :custom
  (dap-auto-configure-features '(locals  tooltip))
  :config
  (setq dap-ui-buffer-configurations
        `((,"*Dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.50))) ;; changed this to 0.50
          (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.50)))
          (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.50)))
          (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
          (,"*debug-window*" . ((side . bottom) (slot . 3) (window-width . 0.20)))))
  (defun rgr/dap-debug()
    (interactive)
    (if current-prefix-arg
        (call-interactively 'dap-debug)
      (dap-debug-last)))
  (require 'dap-cpptools)
  (require 'dap-chrome)
  (dap-chrome-setup)
  :hook
  ((dap-stopped . (lambda (arg)
                    ;; Automatically trigger the built-in dap-mode hydra when the debugger
                    ;; hits a breakpoint.
                    (call-interactively #'dap-hydra)))
   (lsp-mode . (lambda ()
                 ;; Automatically configure dap-mode with default settings
                 (dap-auto-configure-mode 1)))
   ;; Require programming language specific DAP setup.
   ((js-mode js2-mode web-mode) . (lambda ()
                                    (require 'dap-chrome)
                                    (dap-chrome-setup))))
  :config
  (require 'dap-chrome)
  :bind
  (:map lsp-mode-map
        ("C-<f9>" . 'rgr/dap-debug))
  (:map dap-mode-map
        ("<f8>" . dap-continue)
        ("C-S-<f8>" . dap-delete-session)
        ("<f9>" . dap-hydra)
        ("<f10>" . dap-next)
        ("<f11>" . dap-step-in)
        ("S-<f11>" . dap-step-out)
        ))

(use-package dap-lldb
  :ensure nil
  :after dap-mode
  :config
  ;; (setq dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
  (setq dap-lldb-debug-program '((executable-find "lldb-vscode")))
  ;; ask user for executable to debug if not specified explicitly (c++)
  (setq dap-lldb-debugged-program-function
	      (lambda () (read-file-name "Select file to debug: "))))


;; copy from https://zenn.dev/yukit/articles/25a88b33a35633
;; (cond
;;  ((string-equal system-type "windows-nt") ; Microsoft Windows
;;   (progn
;;     (add-to-list 'exec-path (expand-file-name  "~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin"))
;;     (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
;;     ))
;;  ((string-equal system-type "darwin") ; Mac OS X
;;   (progn
;;     (add-to-list 'exec-path (expand-file-name  "~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin"))
;;     (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
;;     (setq rustic-analyzer-command '("~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/rust-analyzer"))
;;     (setq lsp-rust-analyzer-server-command '("~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/rust-analyzer"))
;;     ))
;;  ((string-equal system-type "gnu/linux") ; linux
;;   (progn
;;     (add-to-list 'exec-path (expand-file-name "/backup/backup/rust_installation/cargo/bin"))
;;     (add-to-list 'exec-path (expand-file-name "/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-apple-darwin/bin"))
;;     (setq rustic-analyzer-command '("/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))
;;     (setq lsp-rust-analyzer-server-command '("/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))
;;     )))


;; 正在从ivy、swiper、counsel、hydra转向vertico、consult、embark、orderless。
;; 增强 minibuffer 补全：vertico 和 Orderless, 垂直补全
(use-package vertico
  :hook
  (after-init . vertico-mode)
  (vertico-mode . vertico-multiform-mode)
  ;; Tidy shadowed file names
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  ;; More convenient directory navigation commands
  :bind ((:map vertico-map
               ("RET" . vertico-directory-enter)
               ("M-RET" . vertico-exit-input)
               ("DEL" . vertico-directory-delete-char)
               ("M-DEL" . vertico-directory-delete-word)))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background nil :weight bold :foreground "yellow"))))
  :init
  (setq vertico-resize nil
        vertico-scroll-margin 0
        vertico-count 12
        vertico-cycle t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        vertico-preselect 'directory)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)
        ;; Sort directories before files (vertico-multiform-mode)
        vertico-multiform-categories
        '((file (vertico-sort-function . sort-directories-first)))
        ;; use vertico as the interface for completion-at-point
        completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))
        vterm-timer-delay 0.01
        vertico-buffer-display-action '(display-buffer-below-selected (window-height . 13))
        )
  (vertico-buffer-mode 1)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files))))

(use-package posframe :ensure t)

;; (use-package vertico-posframe
;;   :config
;;   (setq vertico-posframe-min-width 60
;;         vertico-posframe-truncate-lines nil
;;         ;; vertico-posframe-poshandler 'posframe-poshandler-window-top-center
;;         ;; vertico-posframe-poshandler #'posframe-poshandler-frame-top-left-corner
;;         ;; vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
;;         ;; vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center
;;         ;; vertico-posframe-poshandler #'posframe-poshandler-frame-center ;
;;         ;; 光标下面弹出
;;         vertico-posframe-poshandler 'posframe-poshandler-point-window-center
;;         ;; vertico-posframe-width (frame-width)
;;         vertico-posframe-width (window-total-width)
;;         )

;;   (defun vertico-posframe-set-cursor (&rest args)
;;     (with-current-buffer vertico-posframe--buffer
;;       (setq-local cursor-type 'bar)
;;       (setq-local cursor-in-non-selected-windows 'bar)))
;;   (advice-add 'vertico-posframe--show :after 'vertico-posframe-set-cursor)
;;   (vertico-posframe-mode 1))

;; minibuffer 模糊匹配
(use-package orderless
  :ensure t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
	    (?! . orderless-without-literal)
	    (?`. orderless-initialism)
	    (?= . orderless-literal)
	    (?~ . orderless-flex)))
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
	    `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
	     ;; Completing filename or eshell
	     (or minibuffer-completing-file-name
	         (derived-mode-p 'eshell-mode))
	     ;; File extension
	     (string-match-p "\\`\\.." pattern))
	    `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
	        (cons (cdr x) (substring pattern 1))
	      (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
	        (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
	    ;;; Enable partial-completion for files.
	    ;;; Either give orderless precedence or partial-completion.
	    ;;; Note that completion-category-overrides is not really an override,
	    ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles . (partial-completion))) ;; partial-completion is tried first
					                              ;; enable initialism by default for symbols
					                              (command (styles +orderless-with-initialism))
					                              (variable (styles +orderless-with-initialism))
					                              (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch)))

;;配置 Marginalia 增强 minubuffer 的 annotation
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :hook (vertico-mode . marginalia-mode)
  :config
  (setq marginalia-align 'center)
  ;; show mode on/off
  (defun marginalia-annotate-command (cand)
    "Annotate command CAND with its documentation string.
Similar to `marginalia-annotate-symbol', but does not show symbol class."
    (when-let* ((sym (intern-soft cand)))
      (concat
       (let ((mode (if (and sym (boundp sym))
                       sym
                     nil)))
         (when (and sym (boundp sym))
           (if (and (boundp mode) (symbol-value mode))
               (propertize " [On]" 'face 'marginalia-on)
             (propertize " [Off]" 'face 'marginalia-off))))
       (marginalia-annotate-binding cand)
       (marginalia--documentation (marginalia--function-doc sym))))))

(use-package consult
  :straight (consult :type git :host github :repo "minad/consult")
  ;; :demand
  :bind
  (;; C-c bindings (mode-specific-map)
   ;; 重新映射命令
   ;; ([remap goto-line] . consult-goto-line)
   ;; ([remap isearch-forward] . consult-line)
   ;; ([remap isearch-forward-regexp] . consult-line)
   ;; ([remap switch-to-buffer] . consult-buffer)
   ;; C-c bindings (mode-specific-map)
   ("C-s" . consult-line)
   ("C-r" . consult-line)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)

   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)

   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ("<help> a" . consult-apropos)            ;; orig. apropos-command
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flycheck)               ;; Alternative: consult-flymake
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)

   ;; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)

   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("C-s" . consult-line)
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch

   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :init
  ;; Set narrow key
  (setq consult-narrow-key (kbd "<")
        consult-widen-key (kbd ">"))

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref)
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  (defun start-with-current-line (lines)
    (cl-loop
     with current = (line-number-at-pos (point) consult-line-numbers-widen)
     for (line . after) on (cdr lines)
     while (< (cdr (get-text-property 0 'consult-location line)) current)
     collect line into before
     finally (return (cons (car lines) (append after before)))))

  (advice-add 'consult--line-candidates :filter-return #'start-with-current-line)
  (setq
   register-preview-delay 0.5
   register-preview-function #'consult-register-format
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref
   ;; Optionally configure the narrowing key.m
   consult-line-numbers-widen t
   consult-async-min-input 2
   consult-async-refresh-delay  0.15
   consult-async-input-throttle 0.2
   consult-async-input-debounce 0.1
   consult-project-root-function #'projectile-project-root)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.2 any)
   )

  ;; custom functions
  (defun zw/consult-line-multi ()
    (interactive)
    (consult-line-multi t)))

;; custom consult packages
;; (use-package consult-yasnippet
;;   :commands consult-yasnippet)
(use-package consult-dir
  :commands consult-dir)
(use-package consult-tramp
  :commands consult-tramp
  :straight (consult-tramp :type git :host github :repo "Ladicle/consult-tramp")
  :init (setq consult-tramp-method "sshx"))
(use-package consult-flyspell
  :commands consult-flyspell)

;; 保存光标历史，记住上个命令
;; copy from https://book.emacs-china.org/#orga142e60
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq
	 history-length 1000
	 savehist-additional-variables '(mark-ring
					                         global-mark-ring
					                         search-ring
					                         regexp-search-ring
					                         extended-command-history)
	 savehist-autosave-interval 300)
  :config
  ;; (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (setq savehist-file (no-littering-expand-var-file-name "savehist")
        savehist-save-minibuffer-history 1
        )
  )

;; 显示文件列
(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode)
  :init
  ;; 高亮显示选中区域
  ;; (transient-mark-mode t)
  ;; 高亮选中区域颜色
  ;; (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
  (custom-set-faces
   '(region
     ;; ((nil (:background "#666" :foreground "#ffffff")))
     ((nil (:background "purple" :foreground "black")))
     ))
  ;; (progn
  ;;   (setq column-number-mode t)
  ;;   )
  )

;; copy from https://blog.sumtypeofway.com/posts/emacs-config.html
(use-package recentf
  :init
  ;; (add-to-list 'recentf-exclude "\\elpa")
  ;; (add-to-list 'recentf-exclude "private/tmp")
  ;; 2000 files ought to be enough.
  (setq recentf-max-saved-items 2000
        ;;(setq recentf-max-menu-items 5000)
        recentf-auto-cleanup 'never
        recentf-exclude '(no-littering-var-directory
                          no-littering-etc-directory
                          (expand-file-name "elpa" user-emacs-directory)
                          (expand-file-name "straight" user-emacs-directory)
                          "/tmp"
                          "sync-recentf-marker"
                          `(,tramp-file-name-regexp
                            "COMMIT_EDITMSG")
                          tramp-auto-save-directory temporary-file-directory
                          backup-directory-alist (list (cons tramp-file-name-regexp nil))
                          )
        ;; (setq recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"  "~$" "^/ftp:" "^/ssh:" "sync-recentf-marker" (expand-file-name "var/undohist/*" user-emacs-directory)))
        ;; (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
        recentf-save-file (no-littering-expand-var-file-name "recentf"))
  ;; (bind-key "C-c っ" 'helm-recentf)
  ;; (bind-key "C-c t" 'helm-recentf)
  ;; :config
  ;; (recentf-mode 1)
  ;;(run-at-time nil (* 5 60) 'recentf-save-list)
  )

(use-package sync-recentf
  :ensure t
  :config
  (setq recentf-auto-cleanup 60)
  ;; (recentf-mode 1)
  :init
  ;; defvar 必须在init修改，才起效
  (setq sync-recentf-marker (no-littering-expand-var-file-name "sync-recentf-marker"))
  )


;; (use-package yasnippet-snippets
;;   :disabled)


(use-package deadgrep
  :commands (deadgrep)
  :config
  (defun my/deadgrep-fix-buffer-advice (fun &rest args)
    (let ((buf (apply fun args)))
      (with-current-buffer buf
        (toggle-truncate-lines 1))
      buf))
  (advice-add #'deadgrep--buffer :around #'my/deadgrep-fix-buffer-advice))



(use-package dashboard
  :ensure t
  :diminish
  (dashboard-mode page-break-lines-mode)
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名，随读者喜好设置
  (setq dashboard-projects-backend 'projectile) ;; 读者可以暂时注释掉这一行，等安装了 projectile 后再使用
  (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 20)   ;; 显示多少个最近文件
			                    (bookmarks . 10)  ;; 显示多少个最近书签
			                    (projects . 12) ;; 显示多少个最近项目
                          (agenda . 5)
                          ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  ;;跳过远端项目，不要持久化保存
  ;; copy from https://zhuanlan.zhihu.com/p/488366338
  (defun my/project-remember-advice (fn pr &optional no-write)
    (let* ((remote? (file-remote-p (project-root pr)))
           (no-write (if remote? t no-write)))
      (funcall fn pr no-write)))

  (advice-add 'project-remember-project :around
              'my/project-remember-advice)
  )


;; C-c / t 触发 google-this，
(use-package google-this
  :ensure t
  :init
  (google-this-mode))

(use-package tiny
  :ensure t
  ;; 可选绑定快捷键，笔者个人感觉不绑定快捷键也无妨
  :bind
  ("C-;" . tiny-expand))




;; Make rectangular region marking easier.
(use-package rect-mark
  :ensure nil
  :bind (("C-x r C-SPC" . rm-set-mark)
         ("C-x r C-x" . rm-exchange-point-and-mark)
         ("C-x r C-k" . rm-kill-region)
         ("C-x r M-w" . rm-kill-ring-save)))

;; copy from https://github.com/jwiegley/use-package/issues/320
;; Make buffer names unique, handy when opening files with similar names
(use-package uniquify
  :ensure nil
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*")))



;; ANSI & XTERM 256 color support
(use-package xterm-color
  ;;   :defines (compilation-environment
  ;;             eshell-preoutput-filter-functions
  ;;             eshell-output-filter-functions)
  ;;   :functions (compilation-filter my-advice-compilation-filter)
  :init
  ;; For shell and interpreters
  (setenv "TERM" "xterm-256color")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled
              (make-local-variable 'font-lock-function)
              (setq font-lock-function #'ignore)))

  ;; For eshell
  (with-eval-after-load 'esh-mode
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  ;; For compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my-advice-compilation-filter (f proc string)
    (funcall f proc
             (if (eq major-mode 'rg-mode) ; compatible with `rg'
                 string
               (xterm-color-filter string))))
  (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
  (advice-add 'gud-filter :around #'my-advice-compilation-filter))


;; 自动保存
(use-package super-save
  :ensure t
  :defer 1
  :hook (after-init . super-save-mode)
  :diminish super-save-mode
  :config
  ;; (super-save-mode +1)
  ;; Emacs空闲是否自动保存，这里不设置
  (setq super-save-auto-save-when-idle nil)
  ;; add integration with ace-window
  ;; 切换窗口自动保存
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  ;; 查找文件时自动保存
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  ;; 远程文件编辑不自动保存
  (setq super-save-remote-files nil)
  ;;  exclude specific files from super-save, not save gpg files
  ;; 特定后缀名的文件不自动保存
  (setq super-save-exclude '(".gpg"))
  ;; predicates must not take arguments and return nil, when current buffer shouldn't save.
  (add-to-list 'super-save-predicates
               (lambda ()
                 (not (eq major-mode 'markdown-mode))))
  ;; 自动保存时，保存所有缓冲区
  (defun super-save/save-all-buffers ()
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when (and buffer-file-name
                   (buffer-modified-p (current-buffer))
                   (file-writable-p buffer-file-name)
                   (if (file-remote-p buffer-file-name) super-save-remote-files t))
          (save-buffer)))))
  (advice-add 'super-save-command :override 'super-save/save-all-buffers)
  )


(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-nord t)
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package dsvn)

;; copy from https://www.lucacambiaghi.com/vanilla-emacs/readme.html
;; 安装后可以通过 M-x restart-emacs 重启 emacs
(use-package restart-emacs
  :bind ("C-c x r" . restart-emacs))

;; copy from https://codeberg.org/ideasman42/emacs-elisp-autofmt
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;; Will automated download images for the first time
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode)
  :custom
  (emojify-emoji-set "emojione-v2.2.6")
  (emojify-emojis-dir (no-littering-expand-var-file-name "emojify"))
  (emojify-display-style 'image)
  (emojify-download-emojis-p t)
  :config
  (defun --set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (if (eq system-type 'darwin)
	      ;; For NS/Cocoa
	      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
	    ;; For Linux
	    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

  ;; For when Emacs is started in GUI mode:
  (--set-emoji-font nil)
  ;; Hook for when a frame is created with emacsclient
  ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
  (add-hook 'after-make-frame-functions '--set-emoji-font))

;; Suggest next keys to me based on currently entered key combination.
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.7)
  (which-key-add-column-padding 1)
  :config
  (which-key-add-key-based-replacements
    "C-c !" "flycheck"
    "C-c @" "hideshow"
    "C-c i" "ispell"
    "C-c t" "hl-todo"
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x t" "tab")
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 2
        which-key-show-early-on-C-h t
        which-key-idle-secondary-delay 0.05)
  :diminish
  which-key-mode)

;; How to rename or delete file and buffer
(use-package crux
  :ensure t
  :bind
  (
   ("C-c r" . crux-rename-file-and-buffer)
   ("C-c D" . crux-delete-file-and-buffer)
   ;; 优化版的回到行首
   ("C-a" . crux-move-beginning-of-line)
   ;; 快速连接两行等
   ("C-c ^" . crux-top-join-line)
   ;; 快速打开Emacs配置文件
   ("C-x ," . crux-find-user-init-file)
   ("C-c k" . crux-smart-kill-line)
   ("C-x 4 t" . crux-transpose-windows)
   ("C-x K" . crux-kill-other-buffers)
   )
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer)
  )

;; How to delete consecutive space at once
(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks))

;; M-<up> M-<down> to move line up and down
;; 上下移动行/块
(use-package drag-stuff
  :ensure t
  :bind (("<M-up>". drag-stuff-up)
         ("<M-down>" . drag-stuff-down))
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))





;; Nyan Cat is lovely, it can live on mode line
(use-package nyan-mode
  :ensure t
  :init
  (setq nyan-animate-nyancat nil)
  (setq nyan-wavy-trail t)
  (setq nyan-minimum-window-width 80)
  (setq nyan-bar-length 20)
  (nyan-mode))

;; Format all
(use-package format-all
  :ensure t)

;; fzf is a fuzzy file finder which is very quick.
(use-package fzf
  :ensure t)

;; dumb-jump attempts to support many languages by simple searching.
;; It's quite effective even with dynamic libraries like JS and Python.
(use-package dumb-jump
  :ensure t
  :diminish dumb-jump-mode
  :pretty-hydra
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "anchor")
           :color blue :quit-key ("q" "C-g"))
   ("Jump"
    (("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Go other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
    "Other"
    (("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))))
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("C-M-j" . dumb-jump-hydra/body))
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg)
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  )


;; Color Identifier
(use-package color-identifiers-mode
  :ensure t
  :commands color-identifiers-mode)

;; elixir
(use-package elixir-mode
  :ensure t)

;; disable company
;; (use-package alchemist
;; :ensure t)

;; Emacs has a great built in C/C++ mode, but we can improve on it with irony-mode for code completion via libclang.
(use-package irony
  :ensure t
  :hook (c-mode . irony-mode))



;; Web mode handles html/css/js.
(use-package web-mode
  :ensure t
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.phtml\\'"  . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.html\\.twig\\'" . web-mode)
   )
  :custom
  (indent-tabs-mode nil)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  :config
  (setq web-mode-markup-indent-offset 2)
  ;; Set PHP as the embedded language for phtml/tpl.php files.
  (add-to-list 'web-mode-engines-alist
               '(("php" . "\\.phtml\\'")
                 ("php" . "\\.tpl\\.php\\'")))

  ;; Set Django as the embedded language for Twig files.
  (add-to-list 'web-mode-engines-alist
               '("django" . "\\.html\\.twig\\'"))
  )


;; Web beautify prettifies html / css / js using js-beautify - install with npm install -g js-beautify.
(use-package web-beautify
  :ensure t
  :bind (:map web-mode-map
              ("C-c b" . web-beautify-html)
              :map js2-mode-map
              ("C-c b" . web-beautify-js)))

;; HTML preview
(use-package impatient-mode
  :ensure t)

;; Emmet mode
;; use C-j to expand it
(use-package emmet-mode
  :ensure t)

;; Solidity
(use-package solidity-mode
  :ensure t)

;; Colorful todo stags
(use-package hl-todo
  :ensure t
  :hook ((prog-mode org-mode) . teddy-ma/hl-todo-init)
  :init
  (defun teddy-ma/hl-todo-init ()
    (setq-local hl-todo-keyword-faces '(("TODO" . "#ff9977")
                                        ("DOING" . "#FF00BC")
                                        ("DONE" . "#44bc44")
                                        ("BLOCKED" . "#003366")
                                        ))
    (hl-todo-mode))
  )

;; English Chinese Dictionary
(use-package youdao-dictionary
  :ensure t
  :config
  (setq url-automatic-caching t))

;; Disk Usage
(use-package disk-usage
  :ensure t)

;; google style, but with 4 space indent.
;; (defun google-set-c-style-with-4-indent ()
;;   "Set current buffer to google style, but with 4 space indent."
;;   (interactive)
;;   (make-local-variable 'c-tab-always-indent)
;;   (setq c-tab-always-indent t)
;;   (c-add-style "Google" google-c-style t)
;;   (setq tab-width 4
;;         c-indent-tabs-mode t
;;         c-indent-level 4
;;         c-basic-offset 4))

;; (use-package google-c-style)
;; (add-hook 'c-mode-common-hook 'google-set-c-style-with-4-indent)

;; ;; copy from https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)
;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       ;; treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       lsp-idle-delay 0.1)
;; clangd is fast

;;以前是ivy用户，现在则是仅使用vertico, embark和consult了

(use-package ag
  :ensure t
  :ensure-system-package (ag . "brew install ag")
  )


(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(use-package whitespace-cleanup-mode
  :demand t
  :hook
  (special-mode     . me/hide-trailing-whitespace)
  (comint-mode      . me/hide-trailing-whitespace)
  (compilation-mode . me/hide-trailing-whitespace)
  (term-mode        . me/hide-trailing-whitespace)
  (vterm-mode       . me/hide-trailing-whitespace)
  (shell-mode       . me/hide-trailing-whitespace)
  (minibuffer-setup . me/hide-trailing-whitespace)
  :custom
  (show-trailing-whitespace t)
  :config
  (defun me/hide-trailing-whitespace ()
    (setq show-trailing-whitespace nil))
  (global-whitespace-cleanup-mode 1))


;; keyfreq to analyze the key using situation
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line
          org-self-insert-command
          org-delete-backward-char
          org-return
          mwheel-scroll
          dap-tooltip-mouse-motion
          gud-tooltip-mouse-motion))
  )


;; copy from https://tech.toryanderson.com/2020/11/13/migrating-to-a-custom-file-less-setup/
;; With this I turn off customization-file-saving.
(use-package cus-edit
  :ensure nil
  :custom
  (custom-file null-device "Don't store customizations"))


;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-x ;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-x ;" . iedit-mode-from-isearch)
         :map esc-map ("C-x ;" . iedit-execute-last-modification)
         :map help-map ("C-x ;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Dimmer (for dimming inactive buffers)
(use-package dimmer
  :disabled
  :custom
  (dimmer-fraction 0.5)
  (dimmer-exclusion-regexp-list
   '(".*Minibuf.*"
	   ".*which-key.*"
	   ".*NeoTree.*"
	   ".*Messages.*"
	   ".*Async.*"
	   ".*Warnings.*"
	   ".*LV.*"
	   ".*Ilist.*"))
  :config
  (dimmer-mode t))

;; 查看二进制文件
(use-package nhexl-mode
  :ensure t
  :defer t)

;; eldoc-overlay则是将信息显示于sideline
(use-package eldoc-overlay
  :ensure t
  :after quick-look
  :custom
  ((quick-peek-add-spacer nil)
   (quick-peek-position 'above)))

(use-package diminish
  :ensure t)


;; 高亮当前字符
(use-package idle-highlight-mode
  :pin melpa
  :ensure t)

(use-package keychain-environment
  :config (keychain-refresh-environment))

(use-package rg
  :config (rg-enable-menu)
  ;; :init (setq ripgrep-arguments "--ignore-case")
  )

(use-package wgrep
  :after (embark-consult ripgrep)
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t)
  :bind (:map wgrep-mode-map
		          ;; Added keybinding to echo Magit behavior
		          ("C-c C-c" . save-buffer)
		          :map grep-mode-map
		          ("e" . wgrep-change-to-wgrep-mode)
		          :map ripgrep-search-mode-map
		          ("e" . wgrep-change-to-wgrep-mode)))


(use-package kind-icon
  :ensure t
  :custom
  (kind-icon-use-icons t)
  ;; (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  :config
  (add-hook 'my-completion-ui-mode-hook
   	        (lambda ()
   	          (setq completion-in-region-function
   		              (kind-icon-enhance-completion
   		               completion-in-region-function)))))

(use-package pulsar
  :config

  ;; pulse on change
  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)

  ;; configure
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)

  ;; enable globally
  (pulsar-global-mode 1)

  ;; integration with the `consult' package:
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)

  ;; integration with the built-in `imenu':
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry))
(use-package goto-line-preview
  :bind (("M-g g" . goto-line-preview)
         ("M-g M-g" . goto-line-preview)))

;;  Sometimes it's useful to step to the last changes in a buffer.
(use-package goto-last-change
  :bind (("C-x ;" . goto-last-change)))





;; (require 'bind-key)
;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; ;; [j]ump to definition
;; (bind-key "C-c j" 'lsp-ui-peek-find-definitions)
;; ;; jump [f]oward
;; (bind-key "C-c f" 'lsp-ui-peek-jump-forward)
;; ;; jump [b]ack
;; (bind-key "C-c b" 'lsp-ui-peek-jump-backward)
;; ;; find all [r]eferences
;; (bind-key "C-c r" 'lsp-ui-peek-find-references)
;; ;; [r]ename
;; (bind-key "C-c r" 'lsp-rename)
;; ;; [d]escribe thing at point
;; (bind-key "C-c d" 'lsp-describe-thing-at-point)
;; ;; show documentation [u]nder point
;; (bind-key "C-c u" 'lsp-info-under-point)
;; ;; [h]ighlight all relevant references to the symbol under point
;; (bind-key "C-c h" 'lsp-symbol-highlight))))




(use-package edebug-x
  :ensure t
  :defer t
  :commands (edebug-x-mode)
  ;; :init (setq edebug-x-stop-point-overlay t)
  :config
  ;; 取消 edebug-x-mode-map 所有的绑定key
  (setcdr edebug-x-mode-map nil)
  (set-face-attribute 'hi-edebug-x-stop nil
                      :reverse-video nil :foreground nil :overline nil
                      :background (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('light "orange")
                                    ('dark "DarkMagenta")))
  (set-face-attribute 'hi-edebug-x-debug-line nil
                      :reverse-video nil :foreground nil :underline nil
                      :background (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('light "pink")
                                    ('dark "DarkSlateGray")))

  (add-to-list 'display-buffer-alist
               '("^\\*Instrumented Functions\\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*Edebug Breakpoints\\*" (display-buffer-below-selected))))

(use-package bug-hunter
  :ensure t
  :defer t
  :commands (bug-hunter-file bug-hunter-init-file))

;; tempel
;; corfu capf
;; eglot or lsp-bridge
;; project

;; #f(advice-wrapper :after command-error-default-function help-command-error-confusable-suggestions)((quit) "" nil)

;; lentic-mode.el --- minor mode for lentic buffers
(use-package lentic
  :ensure t
  :config (global-lentic-mode))


(use-package focus
  :ensure t
  :config
  (progn
    (setq focus-mode-to-thing '((prog-mode . defun)
                                (text-mode . line)))))

(use-package fontaine
  :ensure t
  :commands (fontaine-store-latest-preset)
  :hook (kill-emacs-hook fontaine-store-latest-preset))

;;smart-hungry-delete 删除多个空格
;;-------------------------------
(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
		     ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks)
  )

;;行首行尾优化(备选:smart C-a and C-e)
;;-----------------------------------------------
(use-package mwim
  :defer t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; 支持驼峰书写的光标移动
;;--------------------------------------------------------
(use-package syntax-subword
  :ensure t
  :defer t
  :diminish syntax-subword-mode
  :config
  (global-syntax-subword-mode)
  :init
  (setq syntax-subword-skip-spaces nil)
  :hook
  (prog-mode . syntax-subword-mode)
  (org-mode . syntax-subword-mode)
  (text-mode . syntax-subword-mode)
  )

(use-package mmm-mode
  :ensure t
  :hook (
         ;; (mmm-mode . company-mode)
         (mmm-mdoe . flycheck-mode)
         (mmm-mode . eldoc-mode))
  :config
  ;; (set-face-background 'mmm-default-submode-face "gray13")
  (setq indent-tab-mode nil)
  (setq mmm-submode-decoration-level 2)
  (setq tab-width 2))

;; One-frame-per-action GIF recording for optimal quality/size ratio
;; https://gitlab.com/ambrevar/emacs-gif-screencast
(use-package gif-screencast
  :ensure t
  :bind (("M-g r" . gif-screencast-start-or-stop)
         ("M-g p" . gif-screencast-pause)
         ("M-g s" . gif-screencast-stop)))

;;; [ realgud ] -- A modular GNU Emacs front-end for interacting with external debuggers.

(use-package realgud
  :ensure t
  :defer t)

;;; [ realgud-lldb ] -- realgud front-end to lldb.

(use-package realgud-lldb
  :ensure t
  :defer t)

;;; [ GDB ]

(use-package gdb-mi
  :ensure t
  :custom
  (gud-chdir-before-run nil)
  ;;(setq gdb-use-separate-io-buffer t)
  ;;(setq gud-tooltip-echo-area t)
  :defer t
  :init
  (setq gdb-many-windows t
        gdb-show-main t)
  :hook
  (gdb-mode . gud-tooltip-mode))


;; 将不同类型的buffer分组切换
(use-package centaur-tabs
  :ensure t
  :config (setq centaur-tabs-set-bar 'over
		            centaur-tabs-set-icons t
		            centaur-tabs-gray-out-icons 'buffer
		            centaur-tabs-set-modified-marker t
		            centaur-tabs-modifier-marker ".")
  (centaur-tabs-mode t))

;; Bookmark
(use-package bookmark
  :ensure nil
  :config
  (with-no-warnings
    ;; Display icons for bookmarks
    (defun my-bookmark-bmenu--revert ()
      "Re-populate `tabulated-list-entries'."
      (let (entries)
        (dolist (full-record (bookmark-maybe-sort-alist))
          (let* ((name       (bookmark-name-from-full-record full-record))
                 (annotation (bookmark-get-annotation full-record))
                 (location   (bookmark-location full-record))
                 (file       (file-name-nondirectory location))
                 (type       (let ((fmt "%-8.8s"))
                               (cond ((null location)
                                      (propertize (format fmt "NOFILE") 'face 'warning))
                                     ((file-remote-p location)
                                      (propertize (format fmt "REMOTE") 'face 'mode-line-buffer-id))
                                     ((not (file-exists-p location))
                                      (propertize (format fmt "NOTFOUND") 'face 'error))
                                     ((file-directory-p location)
                                      (propertize (format fmt "DIRED") 'face 'warning))
                                     (t (propertize (format fmt "FILE") 'face 'success)))))
                 (icon       (if (icon-displayable-p)
                                 (cond
                                  ((file-remote-p location)
                                   (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust 0.0))
                                  ((file-directory-p location)
                                   (all-the-icons-icon-for-dir location :height 0.9 :v-adjust 0.01))
                                  ((not (string-empty-p file))
                                   (all-the-icons-icon-for-file file :height 0.9 :v-adjust 0.0)))
                               "")))
            (push (list
                   full-record
                   `[,(if (and annotation (not (string-equal annotation "")))
                          "*" "")
                     ,icon
                     ,(if (display-mouse-p)
                          (propertize name
                                      'font-lock-face 'bookmark-menu-bookmark
                                      'mouse-face 'highlight
                                      'follow-link t
                                      'help-echo "mouse-2: go to this bookmark in other window")
                        name)
                     ,type
                     ,@(if bookmark-bmenu-toggle-filenames
                           (list (propertize location 'face 'completions-annotations)))])
                  entries)))
        (tabulated-list-init-header)
        (setq tabulated-list-entries entries))
      (tabulated-list-print t))
    (advice-add #'bookmark-bmenu--revert :override #'my-bookmark-bmenu--revert)

    (defun my-bookmark-bmenu-list ()
      "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
      (interactive)
      (bookmark-maybe-load-default-file)
      (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
        (if (called-interactively-p 'interactive)
            (pop-to-buffer buf)
          (set-buffer buf)))
      (bookmark-bmenu-mode)
      (bookmark-bmenu--revert))
    (advice-add #'bookmark-bmenu-list :override #'my-bookmark-bmenu-list)

    (define-derived-mode bookmark-bmenu-mode tabulated-list-mode "Bookmark Menu"
      (setq truncate-lines t)
      (setq buffer-read-only t)
      (setq tabulated-list-format
            `[("" 1) ;; Space to add "*" for bookmark with annotation
              ("" ,(if (icon-displayable-p) 2 0)) ;; Icons
              ("Bookmark" ,bookmark-bmenu-file-column bookmark-bmenu--name-predicate)
              ("Type" 9)
              ,@(if bookmark-bmenu-toggle-filenames
                    '(("File" 0 bookmark-bmenu--file-predicate)))])
      (setq tabulated-list-padding bookmark-bmenu-marks-width)
      (setq tabulated-list-sort-key '("Bookmark" . nil))
      (add-hook 'tabulated-list-revert-hook #'bookmark-bmenu--revert nil t)'
      (setq revert-buffer-function #'bookmark-bmenu--revert)
      (tabulated-list-init-header))))

(use-package time
  :config
  (setq display-time-default-load-average nil
        ;; display-time-use-mail-icon t
        ;; display-time-24hr-format t
        )
  ;; (display-time-mode t)
  ;; copy from https://emacs.stackexchange.com/questions/6065/how-to-display-time-in-seconds-in-the-mode-line
  (setq display-time-format "%Y-%m-%d %H:%M:%S")
                                        ;(setq display-time-interval 1)
  ;; copy from https://codeantenna.com/a/ng3kV0ML9U
  (display-time-mode 1) ;; 常显
  (setq display-time-24hr-format t) ;;格式
  (setq display-time-day-and-date t) ;;显示时间、星期、日期
  ;; copy from https://www.reddit.com/r/emacs/comments/kf3tsq/what_is_this_number_after_the_time_in_the_modeline/
  (setq display-time-default-load-average nil)
  )

;; emmet is a minor-mode that generates html using shorthand, rather than from a pre-existing
;; template.
(use-package emmet-mode
  :hook web-mode)

;; transpose spit windows
(use-package transpose-frame
  :bind ("C-x %". #'transpose-frame))


;; *Gcmh* does garbage collection (GC) when the user is idle.
(use-package gcmh
  :diminish 'gcmh-mode
  :init
  (setq gcmh-idle-delay 5
	      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  :config (gcmh-mode))

(use-package recursion-indicator
  :config
  (recursion-indicator-mode))


(use-package consult-projectile
  :commands (consult-projectile)
  :bind ("C-c C-t" . consult-projectile)
  ("C-x C-p" . consult-projectile))

;; 可视范围内跳转
;; avy 是一个光标移动插件，能快速将光标移动到屏幕上的任意位置，非常强大！
;; copy from https://remacs.cc/posts/%E9%9D%A2%E5%90%91%E4%BA%A7%E5%93%81%E7%BB%8F%E7%90%86%E7%9A%84emacs%E6%95%99%E7%A8%8B10.-emacs%E7%BC%96%E8%BE%91%E8%BF%9B%E9%98%B6/
(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-timer) ;; Control + 单引号
         ;; 复用上一次搜索
         ("C-c C-j" . avy-resume)
         :map isearch-mode-map
         ("C-'" . avy-isearch)
         )
  :config
  (setq avy-background t ;; 打关键字时给匹配结果加一个灰背景，更醒目
        avy-all-windows t ;; 搜索所有 window，即所有「可视范围」
        avy-timeout-seconds 0.3) ;; 「关键字输入完毕」信号的触发时间
  ;; Make `avy-goto-char-timer' support pinyin, refer to:
  ;; https://emacs-china.org/t/avy-avy-goto-char-timer/20900/2
  (defun my/avy-goto-char-timer (&optional arg)
    "Make avy-goto-char-timer support pinyin"
    (interactive "P")
    (let ((avy-all-windows (if arg
                               (not avy-all-windows)
                             avy-all-windows)))
      (avy-with avy-goto-char-timer
        (setq avy--old-cands (avy--read-candidates
                              'pinyinlib-build-regexp-string))
        (avy-process avy--old-cands))))

  (defun avy-action-kill-whole-line (pt)
    "avy action: kill the whole line where avy selection is"
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    "avy action: copy the whole line where avy selection is"
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    "avy action: copy the line where avy selection is and paste to current point"
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    "avy action: kill the line where avy selection is and paste to current point"
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-helpful (pt)
    "avy action: get helpful information at point"
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    t)

  (defun avy-action-mark-to-char (pt)
    "avy action: mark from current point to avy selection"
    (activate-mark)
    (goto-char pt))

  (defun avy-action-flyspell (pt)
    "avy action: flyspell the word where avy selection is"
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-correct-wrapper))))

  (defun avy-action-define (pt)
    "avy action: define the word in dictionary where avy selection is"
    (save-excursion
      (goto-char pt)
      (fanyi-dwim2)))

  (defun avy-action-embark (pt)
    "avy action: embark where avy selection is"
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-google (pt)
    "avy action: google the avy selection when it is a word or browse it when it is a link"
    (save-excursion
      (goto-char pt)
      (my/search-or-browse)))

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?H avy-dispatch-alist) 'avy-action-helpful
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell
        (alist-get ?= avy-dispatch-alist) 'avy-action-define
        (alist-get ?o avy-dispatch-alist) 'avy-action-embark
        (alist-get ?G avy-dispatch-alist) 'avy-action-google
        )

  :custom
  (avy-timeout-seconds 1.0)
  (avy-all-windows t)
  (avy-background t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?q ?e ?r ?u ?i ?p ?n))
  )

;; buffer 内正则替换
;; 渐进式可视化
(use-package anzu
  :diminish anzu-mode

  :custom-face
  (anzu-mode-line ((t (:foreground "yellow"))))
  (anzu-mode-line-no-match ((t (:foreground "red"))))

  :config
  (global-anzu-mode 1)
  )
;; 我都是手动调用它的，因为使用场景不多，但又不能没有……
;; M-x anzu-query-replace-regexp

;; Workspaces
;; 保存和呼出窗口布局。
(use-package eyebrowse
  :ensure t
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "C-x m 1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd  "C-x m 2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "C-x m 3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "C-x m 4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))


;; git clone https://github.com/manateelazycat/color-rg
(use-package color-rg
  :defer nil
  ;; :load-path (lambda () (expand-file-name "elpa/color-rg" user-emacs-directory))
  :straight '(color-rg :type git
		                   :host github
		                   :repo "manateelazycat/color-rg")
  :commands (color-rg-search-input)
  :if (executable-find "rg")
  :bind
  ;; (:map isearch-mode-map
  ("M-s M-s" . isearch-toggle-color-rg)
  ("C-M-s" . color-rg-search-input)
  ;; )
  )

;; (use-package explain-pause-mode
;;   :ensure t
;;   :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
;;   :defer 2
;;   :config
;;   (setq explain-pause-top-auto-refresh-interval 0.1)
;;   (explain-pause-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window))
  :init
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))
  ;;cewindowd窗口提示大小
  :custom-face
  (aw-leading-char-face ((t (:foreground "green" :weight normal :height 4.5))))
  ;; (mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
  ;; (mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil))))
  )

;; kubernetes
(use-package kele
  :ensure t
  ;; :straight t
  :disabled t
  :config
  (kele-mode 1))

;; paredit-mode 启用自动补全括号
;; https://mumble.net/~campbell/emacs/paredit.html
;; 再一次paredit-mode 禁用自动补全括号
;; (use-package paredit
;;   :ensure t
;;   :config
;;   (define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp)
;;   (define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp)
;;   (define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
;;   (define-key paredit-mode-map (kbd "M-[") 'paredit-backward-slurp-sexp)
;;   (add-hook 'cider-repl-mode-hook 'paredit-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;;   (add-hook 'clojure-mode-hook 'paredit-mode)
;;   (add-hook 'nrepl-mode-hook 'paredit-mode)
;;   )

;; nameframe
(use-package nameframe
  :ensure t
  :config
  (global-set-key (kbd "C-c :") 'nameframe-create-frame)
  (global-set-key (kbd "C-c ;") 'nameframe-switch-frame))


;; nameframe-projectile.el --- Nameframe integration with Projectile
(use-package nameframe-projectile
  :after (nameframe projectile)
  :config (nameframe-projectile-mode t))

;;; ue.el
;; https://gitlab.com/unrealemacs/ue.el
(use-package ue
  :init
  (ue-global-mode +1)
  :config
  (define-key ue-mode-map (kbd "C-c u") 'ue-command-map)
  )


;; https://github.com/Wilfred/helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :after pretty-hydra
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Helpful"
    (("f" helpful-callable "callable")
     ("v" helpful-variable "variable")
     ("s" helpful-symbol "symbol")
     ("S" describe-syntax "syntax")
     ("m" describe-mode "mode")
     ("F" describe-face "face")
     ("k" helpful-key "key")
     ("c" helpful-command "command")
     ("d" helpful-at-point "thing at point"))))
  :bind
  ("M-C-h" . helpful-hydra/body)
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


;; Highlight matching parens
(use-package paren
  ;; :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren nil
              show-paren-when-point-in-periphery nil)
  :config
  (with-no-warnings
    ;; Display matching line for off-screen paren.
    (defun display-line-overlay (pos str &optional face)
      "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
      (let ((ol (save-excursion
                  (goto-char pos)
                  (make-overlay (line-beginning-position)
                                (line-end-position)))))
        (overlay-put ol 'display str)
        (overlay-put ol 'face
                     (or face '(:inherit highlight)))
        ol))

    (defvar-local show-paren--off-screen-overlay nil)
    (defun show-paren-off-screen (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp show-paren--off-screen-overlay)
        (delete-overlay show-paren--off-screen-overlay))
      ;; Check if it's appropriate to show match info,
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; Rebind `minibuffer-message' called by `blink-matching-open'
        ;; to handle the overlay display.
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq show-paren--off-screen-overlay
                             (display-line-overlay
                              (window-start) msg ))))))
          (blink-matching-open))))
    (advice-add #'show-paren-function :after #'show-paren-off-screen)))

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package watch-other-window
  ;; :quelpa (watch-other-window :fetcher github
  ;; 		                      :repo "manateelazycat/watch-other-window"
  ;; 		                      :files ("*.el"))
  :straight (watch-other-window :type git :host github :repo "manateelazycat/watch-other-window")
  ;; :init
  ;; (require 'watch-other-window)
  :bind
  ;; (:map prog-mode-map
  ;; 隔壁窗口向下翻一行
  ("C-x 6 d" . watch-other-window-up-line)
  ;; 隔壁窗口向上翻一行
  ("C-x 6 u" . watch-other-window-down-line)
  ;; 隔壁窗口向上翻一屏
  ("C-x 7 d" . watch-other-window-up)
  ;; 隔壁窗口向下翻一屏
  ("C-x 7 u" . watch-other-window-down)
  )

(use-package vc-svn
  :ensure    dsvn
  :init      (progn
               (autoload 'svn-status "dsvn" "Run `svn status'." t)
               (autoload 'svn-update "dsvn" "Run `svn update'." t)))

;; Asynchronous Fuzzy Finder for Emacs
(use-package affe
  :after (consult)
  :bind (("M-s M-f" . affe-find)
	       ("M-s f"   . affe-find)
	       ;; ("M-s M-g" . affe-grep)
	       ("M-s g"   . affe-grep)
         )
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler
        affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-find affe-grep :preview-key (kbd "C-x ."))
  )

(use-package which-key-posframe
  :custom
  (which-key-idle-delay 2)
  (which-key-idle-secondary-delay 0)
  (which-key-posframe-border-width 2)
  (which-key-posframe-parameters '((left-fringe . 5) (right-fringe . 5)))
  :custom-face
  (which-key-posframe ((t (:background "black" ))))
  (which-key-posframe-border ((t (:background "green"))))
  :hook
  (dashboard-after-initialize . which-key-posframe-mode)
  (dashboard-after-initialize . which-key-mode))


;; AwesomePairPac
(use-package awesome-pair
  :straight (awesome-pair
             :host github
             :repo "manateelazycat/awesome-pair")
  :defer t
  :bind
  (:map prog-mode-map
        (
         ("(" . awesome-pair-open-round)
         ("[" . awesome-pair-open-bracket)
         ("{" . awesome-pair-open-curly)
         (")" . awesome-pair-close-round)
         ("]" . awesome-pair-close-bracket)
         ("}" . awesome-pair-close-curly)
         ("=" . awesome-pair-equal)

         ("%" . awesome-pair-match-paren)
         ("\"" . awesome-pair-double-quote)

         ("SPC" . awesome-pair-space)

         ("M-o" . awesome-pair-backward-delete)
         ("C-d" . awesome-pair-forward-delete)
         ("C-k" . awesome-pair-kill)

         ("M-\"" . awesome-pair-wrap-double-quote)
         ("M-[" . awesome-pair-wrap-bracket)
         ("M-{" . awesome-pair-wrap-curly)
         ("M-(" . awesome-pair-wrap-round)
         ("M-)" . awesome-pair-unwrap)

         ("M-F" . awesome-pair-jump-right)
         ("M-B" . awesome-pair-jump-left)
         ("M-:" . awesome-pair-jump-out-pair-and-newline)
         ))
  :config
  (defun setup-awesome-pair-mode ()
    (require 'awesome-pair)
    (awesome-pair-mode 1)
    (show-paren-mode 1)
    )
  :hook
  (prog-mode . setup-awesome-pair-mode)
  (c-mode . setup-awesome-pair-mode)
  (c++-mode . setup-awesome-pair-mode)
  (emacs-lisp-mode . setup-awesome-pair-mode)
  (lisp-interaction-mode . setup-awesome-pair-mode)
  (lisp-mode . setup-awesome-pair-mode)
  (sh-mode . setup-awesome-pair-mode)
  (php-mode . setup-awesome-pair-mode)
  (typescript-mode . setup-awesome-pair-mode)
  (go-mode . setup-awesome-pair-mode)
  (lua-mode . setup-awesome-pair-mode)
  (swift-mode . setup-awesome-pair-mode)
  (json-mode . setup-awesome-pair-mode)
  )


(use-package better-defaults
  :init
  (setq-default cursor-type 'bar)
  (set-default 'indicate-empty-lines t)
  (setq-default truncate-lines t)
  (if (boundp 'buffer-file-coding-system)
      (setq-default buffer-file-coding-system 'utf-8)
    (setf buffer-file-coding-system 'utf-8))
  ;; coloca los archivos de respaldo hechos por Emacs en /tmp
  ;; (setq backup-directory-alist
  ;;       `((".*" . ,temporary-file-directory)))
  (setq
   backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  ;; (setq auto-save-file-name-transforms
  ;;       `((".*" "~/.cache/emacs/saves/" t)))
  ;; apaga creación de lockfiles
  (setq create-lockfiles nil)
  :custom
  (blink-cursor-blinks 1)
  (blink-cursor-interval 1)
  ;; tabs 底下显示 bar
  (x-underline-at-descent-line t)
  (save-interprogram-paste-before-kill t)
  (bookmark-save-flag 1)
  ;; 关闭起动时LOGO
  (inhibit-startup-message t )
  (initial-scratch-message nil)
  (line-spacing 0)
  (make-backup-files nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (echo-keystrokes 0.1)
  (shift-select-mode nil)
  (fill-column 240)
  (blink-matching-paren t)
  (history-length 1000)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (enable-recursive-minibuffers t)
  (gc-cons-percentage 0.125 )
  (ediff-diff-options "-w")
  (ediff-split-window-function 'split-window-horizontally)
  ;; (buffer-file-coding-system 'utf-8)
  ;; (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  :config
  ;; activa transient-mark-mode
  (transient-mark-mode 1)
  (blink-cursor-mode)
  (ido-mode nil)
  (tooltip-mode -1)
  (recentf-mode 1)
  (savehist-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-auto-revert-mode 1)
  (column-number-mode 1)
  ;; (global-subword-mode 1)
  (global-font-lock-mode 1)
  (delete-selection-mode 1)
  (global-hi-lock-mode -1)
  (setq-default bidi-display-reordering nil)
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (add-to-list 'default-frame-alist '(tty-color-mode . -1)))


;;;; Buffers and Tabs
;; https://github.com/alphapapa/bufler.el
;;
;; Why this instead of Centaur Tabs?  `bufler' integrates with `tab-bar-mode'
;; and `tab-lines-mode'.  Why is this important?  Because `centaur-tabs-mode'
;; hack the buffer to add the tabs; the impact was that popped buffers would
;; have sizing issues.
(use-package bufler
  :straight t
  :diminish
  :hook (after-init . (bufler-mode))
  :custom (bufler-columns '("Name" "VC" "Path"))
  :config
  (defun jf/bufler/tab-configuration ()
    (bufler-tabs-mode 1)
    (tab-bar-mode -1)
    (bufler-workspace-tabs))
  (setq tab-line-switch-cycling t)
  (defun jf/bufler-workspace-mode-lighter ()
    "Return the lighter string mode line."
    "Bflr")
  (advice-add #'bufler-workspace-mode-lighter
	            :override #'jf/bufler-workspace-mode-lighter
	            '((name . "wrapper")))
  ;; Ensuring that when I make a selection, it closes the bufler buffer.
  (defun jf/bufler-list-buffer-switch (&rest args)
    (kill-buffer "*Bufler*"))
  (advice-add 'bufler-list-buffer-switch :after 'jf/bufler-list-buffer-switch)

  :bind (:map bufler-list-mode-map
	            ("s-3" . quit-window)
	            ("s-\\" . quit-window))
  :bind
  ;;(("s-3" . bufler-switch-buffer)
  ;;	 ("s-\\" . bufler-sidebar)
  ;; ("s-\\" . jf/tab-bar-switch-prompt-for-tab)
  ;; ("s-]" . tab-line-switch-to-next-tab)
  ;; ("s-}" . tab-line-switch-to-next-tab)
  ;; ("s-[" . tab-line-switch-to-prev-tab)
  ;; ("s-{" . tab-line-switch-to-prev-tab)
  ;;	 )
  )

(use-package color-rg
  :straight (:host github :repo "manateelazycat/color-rg")
  :commands (color-rg-search-input color-rg-search-project color-rg-search-symbol-in-project)

  :custom
  (color-rg-search-no-ignore-file nil)
  (color-rg-mac-load-path-from-shell nil)
  :config
  (custom-set-faces
   `(color-rg-font-lock-header-line-text ((t (:foreground ,(doom-color 'base7)))))
   `(color-rg-font-lock-header-line-keyword ((t (:foreground ,(doom-color 'red)))))
   `(color-rg-font-lock-header-line-directory ((t (:foreground ,(doom-color 'blue)))))
   `(color-rg-font-lock-header-line-edit-mode ((t (:foreground ,(doom-color 'magenta)))))
   `(color-rg-font-lock-command ((t (:background ,(doom-color 'modeline-bg) :foreground ,(doom-color 'comments)))))
   `(color-rg-font-lock-file ((t (:foreground ,(doom-color 'blue)))))
   `(color-rg-font-lock-line-number ((t (:foreground ,(doom-color 'comments)))))
   `(color-rg-font-lock-column-number ((t (:foreground ,(doom-color 'comments)))))
   `(color-rg-font-lock-match ((t (:foreground ,(doom-color 'red))))))
  :if (executable-find "rg")
  :bind ("C-M-s" . color-rg-search-input)
  )

(use-package wgrep-ag :ensure t :defer t)

;; docker tramp
(use-package docker-tramp
  :if (executable-find "docker")
  :ensure t)

(use-package vagrant
  :if (executable-find "vagrant")
  :ensure t)
(use-package vagrant-tramp
  :if (executable-find "vagrant")
  :ensure t)

(use-package sql
  :mode (("\\.sql\\'" . sql-mode))
  )

(use-package sqlformat
  :custom
  (sqlformat-args '("-g"))
  (sqlformat-command 'pgformatter))


;; Needed for `:after char-fold' to work
(use-package char-fold
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :ensure t ; install `reverse-im' using package.el
  :demand t ; always load it
  :after char-fold ; but only after `char-fold' is loaded
  :bind
  ("M-T" . reverse-im-translate-word) ; fix a word in wrong layout
  :custom
  (reverse-im-char-fold t) ; use lax matching
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  (reverse-im-input-methods '("ukrainian-computer")) ; translate these methods
  :config
  (reverse-im-mode t)) ; turn the mode on

;; Rectangle
(use-package rect
  :ensure nil
  :bind (:map text-mode-map
              ("<C-return>" . rect-hydra/body)
              :map prog-mode-map
              ("<C-return>" . rect-hydra/body))
  :init
  (with-eval-after-load 'org
    (bind-key "<s-return>" #'rect-hydra/body org-mode-map))
  (with-eval-after-load 'wgrep
    (bind-key "<C-return>" #'rect-hydra/body wgrep-mode-map))
  (with-eval-after-load 'wdired
    (bind-key "<C-return>" #'rect-hydra/body wdired-mode-map))
  :pretty-hydra
  ((:title (pretty-hydra-title "Rectangle" 'material "border_all" :height 1.2 :v-adjust -0.225)
           :color amaranth :body-pre (rectangle-mark-mode) :post (deactivate-mark) :quit-key ("q" "C-g"))
   ("Move"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→"))
    "Action"
    (("w" copy-rectangle-as-kill "copy") ; C-x r M-w
     ("y" yank-rectangle "yank")         ; C-x r y
     ("t" string-rectangle "string")     ; C-x r t
     ("d" kill-rectangle "kill")         ; C-x r d
     ("c" clear-rectangle "clear")       ; C-x r c
     ("o" open-rectangle "open"))        ; C-x r o
    "Misc"
    (("N" rectangle-number-lines "number lines")        ; C-x r N
     ("e" rectangle-exchange-point-and-mark "exchange") ; C-x C-x
     ("u" undo "undo")
     ("r" (if (region-active-p)
              (deactivate-mark)
            (rectangle-mark-mode 1))
      "reset")))))

;; Docker allows for interaction with the Docker distribution
(use-package docker
  :defer t
  :diminish t
  :commands (docker-containers
             docker-volumes
             docker-networks
             docker-build
             docker-build-buffer
             hydra-docker/body)
  :bind ("C-c d" . docker-containers)
  :hydra (hydra-docker (:columns 5 :color blue)
                       "Docker"
                       ("c" docker-containers "Containers")
                       ("v" docker-volumes "Volumes")
                       ("i" docker-images "Images")
                       ("n" docker-networks "Networks")
                       ("b" dockerfile-build-buffer "Build Buffer")
                       ("q" nil "Quit")))

;; NOTE: hydra and posframe are required
(use-package hydra-posframe
  :ensure t
  :straight (hydra-posframe :type git :host github :repo "Ladicle/hydra-posframe")
  :hook (after-init . hydra-posframe-mode))

;; Goggles highlights the modified region using pulse.
;; Currently the commands undo, yank, kill and delete are supported.
(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq goggles-pulse-delay 0.5)
  (setq-default goggles-pulse t) ;; set to nil to disable pulsing
  )

(use-package which-function-mode
  :ensure nil
  :hook ((prog-mode . which-function-mode)
         (org-mode . which-function-mode))
  :init
  (setq which-func-unknown "")
  (add-hook 'which-function-mode-hook
            #'(lambda ()
                (add-to-list 'which-func-functions
                             #'(lambda ()
                                 (when (eq major-mode 'org-mode)
                                   (mapconcat 'identity (org-get-outline-path t)
                                              " > "))))))
  (add-hook 'prog-mode-hook #'(lambda () (setq header-line-format
                                               '((which-func-mode ("" which-func-format))))))
  (add-hook 'org-mode-hook #'(lambda () (setq header-line-format
                                              '((which-func-mode ("" which-func-format))))))
  )


(use-package maple-preview
  :ensure t
  ;; :quelpa (:fetcher github :repo "honmaple/emacs-maple-preview" :files ("*.el" "index.html" "static"))
  :straight (maple-preview :type git :host github :repo "honmaple/emacs-maple-preview"
                           :files ("*.el" "index.html" "static"))
  :commands (maple-preview-mode))


(use-package go-translate
  :ensure t
  :straight (:type git :host github :repo "lorniu/go-translate")
  :commands (go-translate go-translate-popup)
  :bind (("C-c t g" . gts-do-translate)
         ("C-c t p" . go-translate-at-point)
         ("C-c t s" . go-translate-save-kill-ring))
  :config
  (setq gts-translate-list '(("en" "zh")))
  ;; 配置默认的 translator
  ;; 这些配置将被 gts-do-translate 命令使用
  (setq gts-default-translator
        (gts-translator

         :picker ; 用于拾取初始文本、from、to，只能配置一个

         ;;(gts-noprompt-picker)
         ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))
         (gts-prompt-picker)
         ;;(gts-prompt-picker :single t)
         ;;(gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)

         :engines ; 翻译引擎，可以配置多个。另外可以传入不同的 Parser 从而使用不同样式的输出

         (list
          (gts-bing-engine)
          ;;(gts-google-engine)
          ;;(gts-google-rpc-engine)
          ;;(gts-deepl-engine :auth-key [YOUR_AUTH_KEY] :pro nil)
          ;; (gts-google-engine :parser (gts-google-summary-parser))
          ;;(gts-google-engine :parser (gts-google-parser))
          ;;(gts-google-rpc-engine :parser (gts-google-rpc-summary-parser))
          ;; (gts-google-rpc-engine :parser (gts-google-rpc-parser))
          ;;(gts-youdao-dict-engine)
          ;;(gts-stardict-engine)
          )

         :render ; 渲染器，只能一个，用于输出结果到指定目标。如果使用 childframe 版本的，需自行安装 posframe

         (gts-buffer-render)
         ;;(gts-posframe-pop-render)
         ;;(gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")
         ;;(gts-posframe-pin-render)
         ;;(gts-posframe-pin-render :position (cons 1200 20))
         ;;(gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")
         ;;(gts-kill-ring-render)

         :splitter ; 分割器，可选。如果设置了，将会分段按照提供的规则分段进行翻译。可以选择定制 Render 混合输出分段翻译的结果

         (gts-paragraph-splitter)
         ))

  ;; Pick directly and use Google RPC API to translate
  (defun go-translate-at-point ()
    (interactive)
    (gts-translate (gts-translator
                    :picker (gts-noprompt-picker)
                    :engines (gts-google-rpc-engine)
                    :render (gts-buffer-render))))

  ;; Pick directly and add the results into kill-ring
  (defun go-translate-save-kill-ring ()
    (interactive)
    (gts-translate (gts-translator
                    :picker (gts-noprompt-picker)
                    :engines (gts-google-engine
                              :parser (gts-google-summary-parser))
                    :render (gts-kill-ring-render))))

  )

(use-package pdf-tools
  :straight (pdf-tools :type git :host github :repo "vedang/pdf-tools")
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (pdf-tools-install :no-query)
  :bind (:map pdf-view-mode-map
              ;; Swiper doesn't work in pdf-tools.
              ("C-s" . 'isearch-forward))
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  )

;; save pdf reading page
(use-package saveplace-pdf-view
  :straight (saveplace-pdf-view :type git :host github :repo "nicolaisingh/saveplace-pdf-view")
  :config
  (save-place-mode 1)
  )

(use-package ink
  :ensure t
  :straight (ink :type git :host github :repo "foxfriday/ink")
  )

(use-package query-replace-many
  :ensure t
  :straight (query-replace-many :type git :host github :repo "slotThe/query-replace-many")
  :bind ("C-M-%" . query-replace-many))


;; Epub reader
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . my-nov-setup)
  :init
  (defun my-nov-setup ()
    "Setup `nov-mode' for better reading experience."
    (visual-line-mode 1)
    (centaur-read-mode)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5))
  :config
  (with-no-warnings
    ;; WORKAROUND: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))
  )


;; Ctags IDE on the True Editor
;; @see https://github.com/universal-ctags/citre#quick-start
(use-package citre
  :diminish
  :commands citre-jump-back
  :functions (projectile-project-root xref-go-back)
  :bind (:map prog-mode-map
              ("C-x c j" . citre-jump+)
              ("C-x c k" . citre-jump-back+)
              ("C-x c p" . citre-peek)
              ("C-x c a" . citre-ace-peek)
              ("C-x c u" . citre-update-this-tags-file))
  :init
  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  (with-eval-after-load 'projectile
    (setq citre-project-root-function #'projectile-project-root))

  (defun citre-jump+ ()
    "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))

  (defun citre-jump-back+ ()
    "Go back to the position before last `citre-jump'.
Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error (if (fboundp #'xref-go-back)
                 (call-interactively #'xref-go-back)
               (call-interactively #'xref-pop-marker-stack)))))
  :config
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
  (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog))

  (with-no-warnings
    ;; Use Citre xref backend as a fallback
    (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
      (let ((fetcher (apply fn args))
            (citre-fetcher
             (let ((xref-backend-functions '(citre-xref-backend t)))
               (ignore xref-backend-functions)
               (apply fn args))))
        (lambda ()
          (or (with-demoted-errors "%s, fallback to citre"
                (funcall fetcher))
              (funcall citre-fetcher)))))

    ;; Combine completions from Citre and lsp
    (defun lsp-citre-capf-function ()
      "A capf backend that tries lsp first, then Citre."
      (let ((lsp-result (cond
                         ((bound-and-true-p lsp-mode)
                          (and (fboundp #'lsp-completion-at-point)
                               (lsp-completion-at-point)))
                         ((bound-and-true-p eglot--managed-mode)
                          (and (fboundp #'eglot-completion-at-point)
                               (eglot-completion-at-point))))))
        (if (and lsp-result
                 (try-completion
                  (buffer-substring (nth 0 lsp-result)
                                    (nth 1 lsp-result))
                  (nth 2 lsp-result)))
            lsp-result
          (citre-completion-at-point))))

    (defun enable-lsp-citre-capf-backend ()
      "Enable the lsp + Citre capf backend in current buffer."
      (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))

    (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend)))

;; we recommend using use-package to organize your init.el
;; (use-package codeium
;;   ;; if you use straight
;;   :straight (codeium :type git :host github :repo "Exafunction/codeium.el")
;;   ;; otherwise, make sure that the codeium.el file is on load-path

;;   :init
;;   ;; use globally
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;   ;; or on a hook
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

;;   ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local completion-at-point-functions
;;   ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
;;   ;; an async company-backend is coming soon!

;;   ;; codeium-completion-at-point is autoloaded, but you can
;;   ;; optionally set a timer, which might speed up things as the
;;   ;; codeium local language server takes ~0.2s to start up
;;   ;; (add-hook 'emacs-startup-hook
;;   ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;   ;; :defer t ;; lazy loading, if you want
;;   :config
;;   ;; (setq codeium-directory (no-littering-expand-var-file-name "codeium"))
;;   (setq use-dialog-box nil) ;; do not use popup boxes

;;   ;; if you don't want to use customize to save the api-key
;;   ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;   ;; get codeium status in the modeline
;;   (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;   ;; alternatively for a more extensive mode-line
;;   ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;   (setq codeium-api-enabled
;;         (lambda (api)
;;           (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
;;   ;; you can also set a config for a single buffer like this:
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local codeium/editor_options/tab_size 4)))

;;   ;; You can overwrite all the codeium configs!
;;   ;; for example, we recommend limiting the string sent to codeium for better performance
;;   (defun my-codeium/document/text ()
;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;   ;; if you change the text, you should also change the cursor_offset
;;   ;; warning: this is measured by UTF-8 encoded bytes
;;   (defun my-codeium/document/cursor_offset ()
;;     (codeium-utf8-byte-length
;;      (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;   (setq codeium/document/text 'my-codeium/document/text)
;;   (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;; Trim spaces from end of line on save
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C-." . er/contract-region)))

;; run app from desktop without emulator
(use-package hover
  :ensure t)

(use-package marqueeo
  :ensure t
  ;; if you use straight
  :straight (marqueeo :type git :host github :repo "progfolio/marqueeo"
                      :files (:defaults "*"))
  :demand t
  :diminish
  :config
  (its-a-him-marqueeo)
  )

;; in Dired mode,C-x C-q (dired-toggle-read-only), query-replace,type C-c C-c (wdired-finish-edit)
(use-package wdired
  :defer t
  :custom
  ((dired-listing-switches "-alh --group-directories-first")
   (wdired-allow-to-change-permissions t)
   (wdired-create-parent-directories t)
   )
  )

(use-package run-command
  :after vterm
  :ensure-system-package (grip . "pip install grip")
  :preface
  (defun run-command-recipe ()
    (list
     ;; Run a simple command
     (list :command-name "say-hello"
           :command-line "echo Hello, World!")

     ;; Do something with current file if it's a README
     ;; (uses https://github.com/joeyespo/grip)
     (when (equal (buffer-name) "README.md")
       (list :command-name "preview-github-readme"
             :command-line "grip --browser --norefresh"
             :display "Preview GitHub README"))

     ;; Do something with current file if it's executable
     (let ((buffer-file (buffer-file-name)))
       (when (and buffer-file
                  (file-executable-p buffer-file))
         (list :command-name "run-buffer-file"
               :command-line buffer-file
               :display "Run this buffer's file")))
     )
    )
  :custom (run-command-selector #'run-command-selector-completing-read)
  (run-command-default-runner #'run-command-runner-vterm)
  (run-command-recipes '(run-command-recipe))
  )

;; 如果你用emacs打开一个文件，却编辑不了它（提示该buffer是read-only的），那么用Ctrl-x，Ctrl-q就可以解除锁定。
(use-package zone
  :ensure nil
  :defer 5
  :config
  ;; (zone-when-idle 600) ; in seconds
  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone)))

  (defvar total-count 0)

  (defun zone-waves-animate (c col wend)
    (let ((fall-p nil)
          (o (point))
          (p (point))
          (insert-char " ")
          (halt-char " ")
          (counter 0))

      (while (< counter 10)
        (let ((next-char     (char-after p))
              (previous-char (char-after (- p 1))))

          ;;(progn (forward-line 1) (move-to-column col) (looking-at halt-char))
          ;;          (when (< counter 50)    (move-to-column (mod counter 150)))

          (when (< (random 100) 50)  (move-to-column counter))

          (setq counter (+ 1 counter))
          (setq total-count (+ 1 total-count))

          (when (< (random 100) 80)  (insert (if (< (point) wend)  "." "\n")))

          (if (and (not (looking-at ")"))
                   (not (looking-at "(")))
              (progn
                (save-excursion
                  (when (= 0 (mod counter 1)) (dotimes (_ (random counter)) (insert insert-char)))
                  (when (= 0 (mod counter 4))
                    (when (< (random 100) 10) (insert "(:music => :code)"))
                    (when (< (random 100) 12) (insert "(:code => :art)"))
                    (when (< (random 100) 1) (insert "(   )"))
                    (when (< (random 100) 1) (insert "(    )"))

                    ;;(goto-char (+ p 1))
                    (delete-char (random counter))
                    )
                  ;;(goto-char o)


                  (sit-for 0))
                (if (<= 5 (mod counter 10))
                    (setq p (- (point) 1))
                  (setq p (+ (point) 1))
                  ))

            ))
        )
      fall-p))

  (defun zone-waves ()
    (set 'truncate-lines nil)
    (setq total-count 0)
    (let* ((ww (1- (window-width)))
           (wh (window-height))
           (mc 0)                         ; miss count
           (total (* ww wh))
           (fall-p nil)
           (wend 100)
           (wbeg 1))
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (let ((cc (current-column)))
          (if (< cc ww)
              (insert (make-string (- ww cc) ? ))
            ;;    (delete-char (- ww cc))
            ))
        (unless (eobp) (forward-char 1)))

      (catch 'done; ugh
        (while (not (input-pending-p))
          (goto-char (point-min))
          (let ((wbeg (window-start))
                (wend (window-end)))
            (setq mc 0)

            (goto-char (+ wbeg (random (- wend wbeg))))
            (while (looking-at "[\t\n ]") (goto-char (+ wbeg (random (- wend wbeg)))))
            ;; character animation sequence
            (let ((p (point)))
              (goto-char p)
              (zone-waves-animate (zone-cpos p) (current-column) wend)))
          ;; assuming current-column has not changed...
          ))))

  (eval-after-load "zone"
    '(unless (memq 'zone-waves (append zone-programs nil))
       (setq zone-programs [zone-waves])))
  )

;; M-x format-all-buffer
(use-package format-all
  :config
  (add-hook 'emacs-lisp-mode-hook 'format-all-mode))

(use-package devdocs
  :autoload (devdocs--installed-docs devdocs--available-docs)
  :bind (:map prog-mode-map
              ("M-<f1>" . devdocs-dwim)
              ("C-h D"  . devdocs-dwim))
  :init
  (defconst devdocs-major-mode-docs-alist
    '((c-mode          . ("c"))
      (c++-mode        . ("cpp"))
      (python-mode     . ("python~3.10" "python~2.7"))
      (ruby-mode       . ("ruby~3.1"))
      (go-mode         . ("go"))
      (rustic-mode     . ("rust"))
      (css-mode        . ("css"))
      (html-mode       . ("html"))
      (julia-mode      . ("julia~1.8"))
      (dart-mode      . ("dart~2"))
      (js-mode         . ("javascript" "jquery"))
      (js2-mode        . ("javascript" "jquery"))
      (emacs-lisp-mode . ("elisp")))
    "Alist of major-mode and docs.")

  (mapc
   (lambda (mode)
     (add-hook (intern (format "%s-hook" (car mode)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr mode)))))
   devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (no-littering-expand-var-file-name "devdocs"))

  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.
Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (slug)
       (unless (member slug (let ((default-directory devdocs-data-dir))
                              (seq-filter #'file-directory-p
                                          (when (file-directory-p devdocs-data-dir)
                                            (directory-files "." nil "^[^.]")))))
         (mapc
          (lambda (doc)
            (when (string= (alist-get 'slug doc) slug)
              (devdocs-install doc)))
          (devdocs--available-docs))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t))))

(use-package erlang
  :if (executable-find "erl")
  :mode (("\\.erl\\'" . erlang-mode))
  :defer

  :hook
  (erlang-mode . lsp)
  )

;; use for terminal theme
(use-package moe-theme
  :ensure t)

;; Another AES algorithm encrypt/decrypt string with password.
(use-package kaesar
  :ensure t)

;; Find Emacs Lisp definitions
(use-package elisp-def
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-def-mode)))

;; unity use csharp
(use-package csharp-mode
  :ensure t
  :interpreter ("csharp" . csharp-mode)

  :mode (("\\.cs$" . csharp-mode))
  :config
  (setq lsp-csharp-server-path (executable-find "omnisharp"))
  )

;; Following https://joshwolfe.ca/posts/emacs-csharp-development/ ,
;; omnisharp gives us code completion in C#.
;;
;; Note: This doesn't use lsp-mode, it implements some things on its own.
;;
;; Useful:
;;   omnisharp-solution-errors
;;
;; Also
;; use-package docs: https://github.com/jwiegley/use-package
(use-package omnisharp
  ;; :after company
  :ensure t
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  ;; (add-to-list 'company-backends 'company-omnisharp)
  (define-key csharp-mode-map (kbd "<C-return>") 'omnisharp-go-to-definition)
  (define-key csharp-mode-map (kbd "<M-left>") 'pop-tag-mark)
  )

(use-package graphql-mode
  :ensure t)

(use-package zig-mode
  :mode "\\.zig\\'"
  :hook zig-mode)

;; GLSL (OpenGL shading language) emacs major mode
(use-package glsl-mode
  :mode "\\.frag\\'"
  :hook glsl-mode)

(use-package align                      ; Align text in buffers
  :bind (("C-c A a" . align)
         ("C-c A c" . align-current)
         ("C-c A r" . align-regexp)))

(use-package mark-align                 ; Align visualized (via Marking)
  :ensure nil
  :commands mark-align-mode)

(use-package easy-kill                  ; Easy killing and marking on C-w
  :disabled t
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill) ; M-w
         ([remap mark-sexp]      . easy-mark) ; C-M-SPC
         ))

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column         ; `fill-column' for `visual-line-mode'
  :disabled t
  ;; TODO: use-package: 自定义关键词
  ;; :description "定制 Emacs 自带 visual-line-mode 的宽度"
  ;; :url "foobar"
  :ensure t
  :defer t
  :init
  (setq visual-fill-column-width fill-column)
  ;; (setq visual-fill-column-fringes-outside-margins nil)
  (add-hook 'visual-line-mode-hook
            (defun visual-fill-column-toggle ()
              (visual-fill-column-mode (or visual-line-mode -1)))))

(use-package kotlin-mode
  :after (lsp-mode dap-mode)
  :config
  (lsp-kotlin-lens-mode)
  (require 'dap-kotlin)
  ;; should probably have been in dap-kotlin instead of lsp-kotlin
  (setq lsp-kotlin-debug-adapter-path (or (executable-find "kotlin-debug-adapter") ""))
  :hook
  (kotlin-mode . lsp))





;; xml mode
(use-package nxml-mode
  :ensure nil
  :hook (nxml-mode . lsp)
  :mode ("\\.\\(xml\\|xsd\\|wsdl\\)\\'"))

(use-package sh-script
  :ensure nil
  :mode
  ("/\\.env\\'" . sh-mode)
  ("/\\.env\\." . sh-mode)
  ("/\\.envrc\\'" . sh-mode)
  ("/\\.envrc\\." . sh-mode)
  ("\\.zsh\\'" . sh-mode)
  ("/zshenv\\'" . sh-mode)
  ("/zshrc\\'" . sh-mode)
  ("\\.tmux\\'" . sh-mode)
  ("\\.tmuxsh\\'" . sh-mode)
  ("\\.tmuxtheme\\'" . sh-mode)

  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  (sh-indent-after-continuation 'always))


;; 诸如软回车这样的空白符号转换成一条水平线
(use-package page-break-lines
  :ensure t
  :diminish
  :init (global-page-break-lines-mode))


(use-package quickrun
  :custom
  (quickrun-timeout-seconds 60)
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)
   ("C-c e" . quickrun)
   ("C-c C-e" . quickrun-shell)))

(use-package apheleia
  :ensure t
  :hook
  (tuareg-mode  . apheleia-mode)
  (caml-mode    . apheleia-mode)
  (python-mode  . apheleia-mode)
  (fsharp-mode  . apheleia-mode)
  (kotlin-mode  . apheleia-mode)
  (rustic-mode  . apheleia-mode)
  :config
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--stdout" "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(isort black)))

(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode))

(use-package fill-column-indicator
  :ensure t
  :hook
  ((markdown-mode
    git-commit-mode) . fci-mode))

;; Minimap
(use-package minimap
  :straight t
  :diminish minimap-mode
  :preface
  (defun dark-minimap ()
	  "Dark minimap."
	  (interactive)
	  (custom-set-faces
	   '(minimap-active-region-background
	     ((((background dark)) (:background "#121212121212")) ;;#15
		    (t (:background "#121212121212"))) ;;#ce
	     "Face for the active region in the minimap.
  By default, this is only a different background color."
	     :group 'minimap)))
  (defun light-minimap ()
	  "Light minimap."
	  (interactive)
	  (custom-set-faces
	   '(minimap-active-region-background
	     ((((background dark)) (:background "#151515151515"))
		    (t (:background "#d9d9d9d9d9d9"))) ;;#ce
	     "Face for the active region in the minimap.
  By default, this is only a different background color."
	     :group 'minimap)))
  (defun minimap/enable ()
	  "Enable minimap."
	  (interactive)
	  (minimap-mode t)
	  (put 'minimap-toggle 'state t))
  (defun minimap/disable ()
	  "Disable minimap."
	  (interactive)
	  (minimap-kill)
	  (put 'minimap-toggle 'state nil))
  (defun minimap/toggle ()
	  "Toggle minimap."
	  (interactive)
	  (if (get 'minimap-toggle 'state)
		    (minimap/disable)
	    (minimap/enable)))
  (defun minimap/refresh ()
	  "Refresh minimap."
	  (interactive)
	  (minimap/disable)
	  (minimap/enable))
  :config
  (setq minimap-window-location 'right)
  (setq minimap-width-fraction 0.05)
  (setq minimap-minimum-width 15)
  (setq minimap-hide-fringes t)
  ;;(setq minimap-major-modes '(typescript-mode typescriptreact-mode js-mode js2-mode))
  (setq minimap-major-modes '(prog-mode))
  ;;(setq minimap-update-delay 0)
  (light-minimap))

(use-package profiler
  ;; :init
  ;; (evilified-state-evilify profiler-report-mode profiler-report-mode-map)
  :config
  (progn
    ;; 定义列的宽度
    (setq profiler-report-cpu-line-format
          '((75 left)
            (24 right ((19 right) (5 right)))))
    (setq profiler-report-memory-line-format
          '((75 left)
            (19 right ((14 right profiler-format-number)
                       (5 right)))))
    ))

;; support Pinyin first character match for orderless, avy etc.
;; orderless 支持中文的首字母匹配
(use-package pinyinlib
  :ensure t)

;; string-inflection is a cool little plugin that allows you to
;; cycle a string through an array of standard naming conventions, such as “camelCase”, “UPCASE”, “snake_case”, and more.
(use-package string-inflection
  :ensure t
  :bind (:map prog-mode-map
              ("C-M-j" . my-string-inflection-cycle-auto))
  :init
  (defun my-string-inflection-cycle-auto ()
	  "switching by major-mode"
	  (interactive)
	  (cond
	   ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
	   ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
	   ((eq major-mode 'go-mode) ;; golang use java style
      (string-inflection-java-style-cycle))
	   (t (string-inflection-all-cycle))))  ;; default
  )

;; 两个窗口交换位置
(use-package windmove
  :ensure nil
  :bind
  ("M-i" . windmove-up)
  ("M-k" . windmove-down)
  ("M-j" . windmove-left)
  ("M-l" . windmove-right)
  ("M-K" . windmove-swap-states-down)
  ("M-I" . windmove-swap-states-up)
  ("M-J" . windmove-swap-states-left)
  ("M-L" . windmove-swap-states-right)
  ("C-x M-i" . windmove-delete-up)
  ("C-x M-k" . windmove-delete-down)
  ("C-x M-j" . windmove-delete-left)
  ("C-x M-l" . windmove-delete-right))

(use-package speedrect
  :straight (speedrect :type git :host github :repo "jdtsmith/speedrect"))

;;; Browse kill ring
;; git clone https://github.com/browse-kill-ring/browse-kill-ring.git
;; Installs `M-y' binding to run `browse-kill-ring' command.
(use-package browse-kill-ring
  :ensure t
  :init
  (setq browse-kill-ring-show-preview nil
        browse-kill-ring-highlight-current-entry t
        browse-kill-ring-highlight-inserted-item t)
  :config
  (browse-kill-ring-default-keybindings))

(provide 'init-config-packages)
;;; init-config-packages.el ends here
