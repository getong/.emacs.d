;;; -*- coding: utf-8; lexical-binding: t -*-

;; copy from https://sachachua.com/dotemacs/index.html
;; (defvar my-laptop-p (equal (system-name) "MacBook-Pro.lan"))
;; (defvar my-server-p (and (equal (system-name) "localhost") (equal user-login-name "getong")))
;; (defvar my-phone-p (not (null (getenv "ANDROID_ROOT")))
;;   "If non-nil, GNU Emacs is running on Termux.")
;; (when my-phone-p (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
;; (global-auto-revert-mode)  ; simplifies syncingr

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


;; benchmark-init records startup time by package so we can debug. It only records things after it's initialised, so put as early in config as possible.
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-hook 'after-init-hook
          (lambda () (message "loaded in %s" (emacs-init-time))))

(use-package async-await
  :config
  (promise-rejection-tracking-enable '((all-rejections . t))))

;; 让 .emacs.d 更干净
;; no littering, keep .emacs.d clean
(use-package no-littering
  :ensure t
  :config
  ;; (add-to-list 'recentf-exclude no-littering-var-directory)
  ;; (add-to-list 'recentf-exclude no-littering-etc-directory)
  (with-eval-after-load 'recentf
    (set 'recentf-exclude
         '(no-littering-var-directory
           no-littering-etc-directory
           (expand-file-name "elpa" user-emacs-directory)
           (expand-file-name "cache" user-emacs-directory))))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (unless (file-exists-p custom-file)  ;; 如果该文件不存在
    (write-region "" nil custom-file)) ;; 写入一个空内容，相当于 touch 一下它
  (load custom-file)

  ;; https://eshelyaron.com/esy.html
  ;; (setq auto-save-file-name-transforms
  ;;       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; 检查auto-save目录是否存在，不存在就创建
  ;; (unless (file-directory-p (no-littering-expand-var-file-name "auto-save/"))
  ;;   (make-directory (no-littering-expand-var-file-name "auto-save/"))
  ;;   )
  (unless (file-directory-p (no-littering-expand-var-file-name "docsets/"))
    (make-directory (no-littering-expand-var-file-name "docsets/"))
    )
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory))))
  ;; (setq recentf-max-menu-items 5)
  )

;; copy from [Error when running magit-status: run-hooks: Wrong number of arguments](https://github.com/magit/magit/issues/3837)
(use-package transient
  :init
  ;; (setq transient-history nil)
  (setq transient-history-file "~/.emacs.d/var/transient/history.el")
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
;; (use-package olivetti
;;   :ensure
;;   :defer
;;   :diminish
;;   :config
;;   (setq olivetti-body-width 0.65)
;;   (setq olivetti-minimum-body-width 72)
;;   (setq olivetti-recall-visual-line-mode-entry-state t)
;;   (define-minor-mode prot/olivetti-mode
;;     "Toggle buffer-local `olivetti-mode' with additional parameters.
;; Fringes are disabled.  The modeline is hidden, except for
;; `prog-mode' buffers (see `prot/hidden-mode-line-mode').  The
;; default typeface is set to a proportionately-spaced family,
;; except for programming modes (see `prot/variable-pitch-mode').
;; The cursor becomes a blinking bar, per `prot/cursor-type-mode'."
;;     :init-value nil
;;     :global nil
;;     (if prot/olivetti-mode
;;         (progn
;;           (olivetti-mode 1)
;;           (set-window-fringes (selected-window) 0 0)
;;           ;; (prot/variable-pitch-mode 1)
;;           (prot/cursor-type-mode 1)
;;           (unless (derived-mode-p 'prog-mode)
;;             (prot/hidden-mode-line-mode 1)))
;;       (olivetti-mode -1)
;;       (set-window-fringes (selected-window) nil) ; Use default width
;;       ;; (prot/variable-pitch-mode -1)
;;       (prot/cursor-type-mode -1)
;;       (unless (derived-mode-p 'prog-mode)
;;         (prot/hidden-mode-line-mode -1))))
;;   :bind ("C-c o" . prot/olivetti-mode))

;; copy from [Saving persistent undo to a single directory, alist format](https://emacs.stackexchange.com/questions/26993/saving-persistent-undo-to-a-single-directory-alist-format)
;;(use-package undo-tree
;;  :defer t
;;  :diminish undo-tree-mode
;;  :init (global-undo-tree-mode)
;;  :custom
;;  (undo-tree-visualizer-diff t)
;;  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
;;  (undo-tree-visualizer-timestamps t))
;;
;;;; copy from [Playing nicely with linum](https://www.emacswiki.org/emacs/UndoTree)
;;(defun undo-tree-visualizer-update-linum (&rest args)
;;  (linum-update undo-tree-visualizer-parent-buffer))
;;(advice-add 'undo-tree-visualize-undo :after #'undo-tree-visualizer-update-linum)
;;(advice-add 'undo-tree-visualize-redo :after #'undo-tree-visualizer-update-linum)
;;(advice-add 'undo-tree-visualize-undo-to-x :after #'undo-tree-visualizer-update-linum)
;;(advice-add 'undo-tree-visualize-redo-to-x :after #'undo-tree-visualizer-update-linum)
;;(advice-add 'undo-tree-visualizer-mouse-set :after #'undo-tree-visualizer-update-linum)
;;(advice-add 'undo-tree-visualizer-set :after #'undo-tree-visualizer-update-linum)

;;(use-package vundo
;;  :bind ("C-x u" . vundo)
;;  :commands (vundo)
;;  :hook ((vundo-mode . my/vundo-setup))
;;  :init
;;  (progn
;;    (setq vundo-window-max-height 5))
;;  :config
;;  (progn
;;    (setq vundo-glyph-alist vundo-unicode-symbols)
;;    ;; Take less on-screen space.
;;    (setq vundo-compact-display t)
;;    ;; Better contrasting highlight.
;;    (custom-set-faces
;;     '(vundo-node ((t (:foreground "#808080"))))
;;     '(vundo-stem ((t (:foreground "#808080"))))
;;     '(vundo-highlight ((t (:foreground "#FFFF00")))))
;;
;;    (defun my/vundo-setup ()
;;      "Remove mode-line and header-line."
;;      (setq mode-line-format nil)
;;      (setq header-line-format nil))
;;    ))

;; (use-package hydra
;;   :ensure t
;;   :commands defhydra)

;; (use-package use-package-hydra
;;   :ensure t
;;   :after hydra)
;; (if my-laptop-p
;;     (use-package hydra-posframe :if my-laptop-p :after hydra))

;; (with-eval-after-load 'hydra
;;   (defhydra my-window-movement ()
;;     ("<left>" windmove-left)
;;     ("<right>" windmove-right)
;;     ("<down>" windmove-down)
;;     ("<up>" windmove-up)
;;     ("y" other-window "other")
;;     ("h" switch-window "switch-window")
;;     ("b" consult-buffer "buffer")
;;     ("f" find-file "file")
;;     ("F" find-file-other-window "other file")
;;     ("v" (progn (split-window-right) (windmove-right)))
;;     ("o" delete-other-windows :color blue)
;;     ("a" ace-window)
;;     ("s" ace-swap-window)
;;     ("d" delete-window "delete")
;;     ("D" ace-delete-window "ace delete")
;;     ("i" ace-maximize-window "maximize")
;;     ("q" nil)))

;; (with-eval-after-load 'hydra
;;   (defhydra my-shortcuts (:exit t)
;;     ("j" my-helm-journal "Journal")
;;     ("C" my-resolve-orgzly-syncthing "Conflicts")
;;     ("n" my-capture-timestamped-note "Note")
;;     ("c" my-org-categorize-emacs-news/body "Categorize")
;;     ("d" my-emacs-news-check-duplicates "Dupe")
;;     ("s" save-buffer "Save")
;;     ("f" my-file-shortcuts/body "File shortcut")
;;     ("+" text-scale-increase "Increase")
;;     ("-" text-scale-decrease "Decrease")
;;     ("G" gif-screencast-start-or-stop "GIF screencast")
;;     ("g" my-geeqie/body "Geeqie")
;;     ("r" my-record-ffmpeg-toggle-recording "Record screen")
;;     ("l" (my-toggle-or-create "*scratch*" (lambda () (switch-to-buffer (startup--get-buffer-create-scratch)))) "Lisp")
;;     ("e" eshell-toggle "Eshell")
;;     ("w" my-engine-dmode-hydra/body "Search web")
;;     ("E" my-emacs-news/body "Emacs News"))
;;   (global-set-key (kbd "<f5>") #'my-shortcuts/body)
;;   (defhydra my-emacs-news (:exit t)
;;     "Emacs News"
;;     ("f" (find-file "~/sync/emacs-news/index.org") "News")
;;     ("C" (find-file "~/proj/emacs-calendar/README.org") "Calendar")
;;     ("C" (find-file "/ssh:web:/var/www/emacslife.com/calendar/README.org" "Calendar on server"))
;;     ("d" my-emacs-news-check-duplicates "Dupe")
;;     ("c" my-org-categorize-emacs-news/body "Categorize")
;;     ("h" (my-org-update-link-description "HN") "Link HN")
;;     ("i" (my-org-update-link-description "Irreal") "Link Irreal")
;;     ("m" my-share-emacs-news "Mail")
;;     ("t" (browse-url "https://tweetdeck.twitter.com") "Twitter")))

;; (defun my-org-update-link-description (description)
;;   "Update the current link's DESCRIPTION."
;;   (interactive "MDescription: ")
;;   (let (link)
;;     (save-excursion
;;       (cond
;;        ((org-in-regexp org-link-bracket-re 1)
;;         (setq link (org-link-unescape (match-string-no-properties 1)))
;;         (delete-region (match-beginning 0) (match-end 0))
;;         (insert (org-link-make-string link description))
;;         (sit-for 0))
;;        ((or (org-in-regexp org-link-angle-re)
;;             (org-in-regexp org-link-plain-re))
;;         (setq link (org-unbracket-string "<" ">" (match-string 0)))
;;         (delete-region (match-beginning 0) (match-end 0))
;;         (insert (org-link-make-string link description))
;;         (sit-for 0))))))

;; (defun my-org-insert-link ()
;;   (interactive)
;;   (when (org-in-regexp org-bracket-link-regexp 1)
;;     (goto-char (match-end 0))
;;     (insert "\n"))
;;   (call-interactively 'org-insert-link))

;; (defun my-switch-to-previous-buffer ()
;;   "Switch to previously open buffer.
;;       Repeated invocations toggle between the two most recently open buffers."
;;   (interactive)
;;   (switch-to-buffer (other-buffer (current-buffer) 1)))

;; (defun my-org-check-agenda ()
;;   "Peek at agenda."
;;   (interactive)
;;   (cond
;;    ((derived-mode-p 'org-agenda-mode)
;;     (if (window-parent) (delete-window) (bury-buffer)))
;;    ((get-buffer "*Org Agenda*")
;;     (switch-to-buffer-other-window "*Org Agenda*"))
;;    (t (org-agenda nil "a"))))

;; (defun my-goto-random-char ()
;;   (interactive)
;;   (goto-char (random (point-max))))

;; (defvar hydra-stack nil)

;; (defun my-hydra-push (expr)
;;   (push `(lambda () ,expr) hydra-stack))

;; (defun my-hydra-pop ()
;;   (interactive)
;;   (let ((x (pop hydra-stack)))
;;     (when x (funcall x))))

;; (defun my-hydra-go-and-push (expr)
;;   (push hydra-curr-body-fn hydra-stack)
;;   (prin1 hydra-stack)
;;   (funcall expr))

;; (use-package undo-tree
;;   :ensure t
;;   :init (global-undo-tree-mode)
;;   :after hydra
;;   :bind ("C-x C-h u" . hydra-undo-tree/body)
;;   :custom
;;   (undo-tree-visualizer-diff t)
;;   (undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo")))
;;   (undo-tree-visualizer-timestamps t)
;;   :hydra (hydra-undo-tree (:hint nil)
;;                           "
;;   _p_: undo  _n_: redo _s_: save _l_: load   "
;;                           ("p"   undo-tree-undo)
;;                           ("n"   undo-tree-redo)
;;                           ("s"   undo-tree-save-history)
;;                           ("l"   undo-tree-load-history)
;;                           ("u"   undo-tree-visualize "visualize" :color blue)
;;                           ("q"   nil "quit" :color blue)))

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
  (setq undohist-directory (expand-file-name "var/undohist" user-emacs-directory))
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

;; (use-package iedit
;;   :ensure t)

;;
;; Multiple cursors
;;
;; Support for multiple cursor selection and editing.
;;
(use-package multiple-cursors
  :ensure t
  :bind (
         ;; 每行一个光标
         ("C-S-c" . mc/edit-lines)
         ;; 全选光标所在单词并在下一个单词增加一个光标。通常用来启动一个流程
         ("C->" . mc/mark-next-like-this-symbol)
         ;; 跳过当前单词并跳到下一个单词，和上面在同一个流程里。
         ("C-M->" . mc/skip-to-next-like-this)
         ;; 同样是开启一个多光标流程，但是是「向上找」而不是向下找。
         ("C-<" . mc/mark-previous-like-this-symbol)
         ;; 跳过当前单词并跳到上一个单词，和上面在同一个流程里。
         ("C-M-<" . mc/skip-to-previous-like-this)
         ;; 直接多选本 buffer 所有这个单词
         ("C-c C->" . mc/mark-all-symbols-like-this)
         ))

;; (use-package multiple-cursors
;;   :ensure t
;;   :after hydra
;;   :bind
;;   (("C-x C-h m" . hydra-multiple-cursors/body)
;;    ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))
;;   :hydra (hydra-multiple-cursors
;; 		  (:hint nil)
;; 		  "
;; Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
;; ------------------------------------------------------------------
;;  [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
;;  [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
;;  [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
;;  [_|_] Align with input CHAR       [Click] Cursor at point"
;; 		  ("l" mc/edit-lines :exit t)
;; 		  ("a" mc/mark-all-like-this :exit t)
;; 		  ("n" mc/mark-next-like-this)
;; 		  ("N" mc/skip-to-next-like-this)
;; 		  ("M-n" mc/unmark-next-like-this)
;; 		  ("p" mc/mark-previous-like-this)
;; 		  ("P" mc/skip-to-previous-like-this)
;; 		  ("M-p" mc/unmark-previous-like-this)
;; 		  ("|" mc/vertical-align)
;; 		  ("s" mc/mark-all-in-region-regexp :exit t)
;; 		  ("0" mc/insert-numbers :exit t)
;; 		  ("A" mc/insert-letters :exit t)
;; 		  ("<mouse-1>" mc/add-cursor-on-click)
;; 		  ;; Help with click recognition in this hydra
;; 		  ("<down-mouse-1>" ignore)
;; 		  ("<drag-mouse-1>" ignore)
;; 		  ("q" nil)))

;; copy from https://emacs-china.org/t/vterm-zsh/20497
;;; Terminal
(use-package vterm
  :when (memq window-system '(mac ns x pgtk))
  :bind (:map vterm-mode-map
              ("C-y" . vterm-yank)
              ("M-y" . vterm-yank-pop)
              ("C-k" . vterm-send-C-k-and-kill))
  :custom
  ((vterm-buffer-name-string "*my-own-vterm*")
   (vterm-use-vterm-prompt-detection-method t)
   (vterm-max-scrollback 10000)
   )
  :init
  (setq vterm-shell "zsh")
  :config
  ;; TODO disable c-] shortcut
  ;; shell 退出时 kill 掉这个 buffer
  (setq vterm-kill-buffer-on-exit t)
  ;; 使用 M-x vterm 新建一个 terminal
  ;; 在 terminal 中使用 C-c C-t 进入「选择」模式（类似 Tmux 里的 C-b [ ）
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  ;; (setq vterm-max-scrollback 99999)
  (setq vterm-always-compile-module t)
  (defun vterm-send-C-k-and-kill ()
    "Send `C-k' to libvterm, and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (vterm-end-of-line))
    (vterm-send-key "k" nil nil t))
  (defun turn-off-chrome ()
    ;; vterm 模式下禁用hl-line-mode
    (hl-line-mode -1)
    ;; vterm 模式下禁用display-line-numbers-mode
    (display-line-numbers-mode -1)
    ;; vterm 模式下禁用emojify-mode
    (emojify-mode -1)
    (setq-local global-hl-line-mode nil)
    )
  :hook
  (vterm-mode . turn-off-chrome)
  ;; copy from https://erickgnavar.github.io/emacs-config/
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
  (vterm-always-compile-module t)
  )

(use-package vterm-toggle
  :when (memq window-system '(mac ns x pgtk))
  :custom
  (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
  (vterm-toggle-scope 'project)
  :bind (
         ([f9] . vterm-compile)
         :map vterm-mode-map
         ([(control return)] . vterm-toggle-insert-cd)
         ("C-c t" . #'vterm-toggle)
         ("C-\\" . #'popper-cycle)
         ("s-t" . #'vterm) ; Open up new tabs quickly
         ("s-v" . #'vterm-yank)
         )
  :config
  (setq vterm-toggle-cd-auto-create-buffer nil)
  (setq vterm-toggle-fullscreen-p t)
  (defvar vterm-compile-buffer nil)
  (defun vterm-compile ()
    "Compile the program including the current buffer in `vterm'."
    (interactive)
    (setq compile-command (compilation-read-command compile-command))
    (let ((vterm-toggle-use-dedicated-buffer t)
          (vterm-toggle--vterm-dedicated-buffer (if (vterm-toggle--get-window)
                                                    (vterm-toggle-hide)
                                                  vterm-compile-buffer)))
      (with-current-buffer (vterm-toggle-cd)
        (setq vterm-compile-buffer (current-buffer))
        (rename-buffer "*vterm compilation*")
        (compilation-shell-minor-mode 1)
        (vterm-send-M-w)
        (vterm-send-string compile-command t)
        (vterm-send-return)))))

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
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; no tabs
  ;; (setq indent-tabs-mode nil)
  (setq-default indent-tabs-mode nil)
  ;; keep everything under vc
  (setq make-backup-files nil)
  ;; no need to create lockfiles
  (setq create-lockfiles nil)
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
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything

  ;; Don't persist a custom file
  ;; (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  ;; (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings

  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  ;; FIXME currently using tempel in org-mode triggers this warning
  ;; (setq warning-suppress-types (append warning-suppress-types '((org-element-cache))))

  ;; 显示括号匹配
  (show-paren-mode t)

  ;; Hide commands in M-x which don't work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  :config
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
    (load-file "~/.emacs.d/init.el"))
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

  ;; (define-minor-mode prot/scroll-centre-cursor-mode
  ;;   "Toggle centred cursor scrolling behaviour."
  ;;   :init-value nil
  ;;   :lighter " S="
  ;;   :global nil
  ;;   (if prot/scroll-centre-cursor-mode
  ;;       (setq-local scroll-margin (* (frame-height) 2)
  ;;                   scroll-conservatively 0
  ;;                   maximum-scroll-margin 0.5)
  ;;     (dolist (local '(scroll-preserve-screen-position
  ;;                      scroll-conservatively
  ;;                      maximum-scroll-margin
  ;;                      scroll-margin))
  ;;       (kill-local-variable `,local))))
  ;; (define-minor-mode prot/hidden-mode-line-mode
  ;;   "Toggle modeline visibility in the current buffer."
  ;;   :init-value nil
  ;;   :global nil
  ;;   (if prot/hidden-mode-line-mode
  ;;       (setq-local mode-line-format nil)
  ;;     (kill-local-variable 'mode-line-format)
  ;;     (force-mode-line-update)))
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

  ;; copy from [Aligning columns in Emacs](https://blog.lambda.cx/posts/emacs-align-columns/)
  (defun align-non-space (BEG END)
    "Align non-space columns in region BEG END."
    (interactive "r")
    (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))
  )

;; Copy from https://www.danielde.dev/blog/emacs-for-swift-development
(use-package swift-mode
  :bind (("C-c l" . print-swift-var-under-point))
  :config
  (defun print-swift-var-under-point()
    (interactive)
    (if (string-match-p (string (preceding-char)) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
        (backward-sexp)
      nil)
    (kill-sexp)
    (yank)
    (move-end-of-line nil)
    (newline)
    (insert "print(\"")
    (yank)
    (insert ": \\(")
    (yank)
    (insert ")\")")
    (indent-for-tab-command))
  (defun xcode-build()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))
  (defun xcode-run()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))
  (defun xcode-test()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'"))
  (global-set-key (kbd "C-c p b") 'xcode-build)
  (global-set-key (kbd "C-c p r") 'xcode-run)
  (global-set-key (kbd "C-c p t") 'xcode-test)

  (defun xcode-open-current-file()
    (interactive)
    (shell-command-to-string
     (concat "open -a \"/Applications/Xcode.app\" " (buffer-file-name)))
    (kill-new (car (cdr (split-string (what-line)))))
    (shell-command-to-string
     "open keysmith://run-shortcut/796BB627-5433-48E4-BB54-1AA6C54A14E8"))
  (global-set-key (kbd "C-c p o") 'xcode-open-current-file)

  ;; copy from [launch love2d app from Emacs](https://gist.github.com/legumbre/38ef323645f17a3c8033)
  (defvar love2d-program "/usr/local/bin/love")

  (defun love2d-launch-current ()
    (interactive)
    (let ((app-root (locate-dominating-file (buffer-file-name) "main.lua")))
      (if app-root
          (shell-command (format "%s %s &" love2d-program app-root))
        (error "main.lua not found"))))

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

;;(require 'pangu-spacing)
;;(global-pangu-spacing-mode 1)
(use-package pangu-spacing
  :config
  (global-pangu-spacing-mode 1)
  )

;;(require 'highlight-thing)
;;(global-highlight-thing-mode)
(use-package highlight-thing
  :config
  (global-highlight-thing-mode)
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-delay-seconds 0.1)
  (setq highlight-thing-limit-to-defun t)
  (setq highlight-thing-case-sensitive-p t)
  )

;; copy from https://immerrr.github.io/lua-mode/
;;(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(defun pnh-lua-completion-string-for (expr file)
  (mapconcat 'identity
             `("do"
               "local clone = function(t)"
               "  local n = {} for k,v in pairs(t) do n[k] = v end return n"
               "end"
               "local function cpl_for(input_parts, ctx, prefixes)"
               "  if #input_parts == 0 and ctx ~= _G then"
               "    return ctx"
               "  elseif #input_parts == 1 then"
               "    local matches = {}"
               "    for k in pairs(ctx) do"
               "      if k:find('^' .. input_parts[1]) then"
               "        local parts = clone(prefixes)"
               "        table.insert(parts, k)"
               "        table.insert(matches, table.concat(parts, '.'))"
               "      end"
               "    end"
               "    return matches"
               "  else"
               "    local token1 = table.remove(input_parts, 1)"
               "    table.insert(prefixes, first_part)"
               "    return cpl_for(input_parts, ctx[token1], prefixes)"
               "  end"
               "end"
               "local i = {" ,@(mapcar (apply-partially 'format "'%s',")
                                       (split-string expr "\\.")) "}"
               ,(format "local f = io.open('%s', 'w')" file)
               ;; TODO: using _G here is pretty lame! try to get local context
               "for _,l in ipairs(cpl_for(i, _G, {})) do"
               "  f:write(l .. string.char(10))"
               "end"
               "f:close()"
               "end") "\n"))



(use-package lua-mode
  :mode "\\.lua\\'"
  :init
  (add-hook 'lua-mode-hook
            (defun pnh-lua-mode-hook ()
              (make-variable-buffer-local 'completion-at-point-functions)
              (add-to-list 'completion-at-point-functions 'pnh-lua-complete)))
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  (defun pnh-lua-complete ()
    (let* ((boe (save-excursion (search-backward-regexp "[^\.a-zA-Z0-9_]")
                                (point)))
           (bot (save-excursion (when (symbol-at-point)
                                  (backward-word)) (point)))
           (expr (buffer-substring-no-properties (1+ boe) (point)))
           (file (make-temp-file "lua-completions-")))
      (lua-send-string (pnh-lua-completion-string-for expr file))
      (sit-for 0.1)
      (list bot (point) (when (file-exists-p file)
                          (with-temp-buffer
                            (insert-file-contents file)
                            (delete-file file)
                            (butlast (split-string (buffer-string) "\n")))))))
  )

(use-package fennel-mode
  :ensure t)

(use-package reformatter
  :ensure t)

(reformatter-define lua-format
  :program "stylua"
  :args '("-")
  :group 'lua)

(reformatter-define js-format
  :program "npx"
  :args '("prettier" "--stdin-filepath" "a.js"))

;;(with-eval-after-load 'js
;;  (evil-leader/set-key-for-mode 'js-mode "d" 'dumb-jump-go)
;;  (define-key js-mode-map (kbd "C-c C-f") 'js-format-buffer))

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
   '(rainbow-delimiters-unmatched-face ((t (:background "red" :foreground "green" :weight bold)))))
  :hook
  ((css-mode . rainbow-mode)
   (sass-mode . rainbow-mode)
   (scss-mode . rainbow-mode))
  ((prog-mode . rainbow-delimiters-mode)))

;;(add-to-list 'default-frame-alist '(foreground-color . "white"))
;;(add-to-list 'default-frame-alist '(background-color . "black"))
;;(require 'color-theme-sanityinc-tomorrow)
;;(load-theme 'sanityinc-tomorrow-blue t)
;;(color-theme-sanityinc-tomorrow--define-theme blue)
;;(add-to-list 'default-frame-alist '(cursor-color . "black"))
;; (add-to-list 'default-frame-alist '(cursor-type . bar))
;;(blink-cursor-mode -1)
;;(setq blink-cursor-blinks -1)
;; theme
;;(use-package color-theme-sanityinc-tomorrow
;;  :defer t
;;  :init (load-theme 'sanityinc-tomorrow-night t))

;; copy from https://protesilaos.com/codelog/2022-08-15-intro-ef-themes-emacs/
;; (use-package ef-themes
;;   :defer t
;;   :init (load-theme 'ef-winter t))

;; (use-package indent-guide
;;   :config
;;   (indent-guide-global-mode)
;;   )
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

(use-package switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window)
  (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
  (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
  (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
  (global-set-key (kbd "C-x 0") 'switch-window-then-delete)

  (global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
  (global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
  (global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
  (global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

  (global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
  (global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

  (global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)

  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
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
        auto-package-update-interval 4)
  (auto-package-update-maybe))

(use-package general
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer patrl/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer patrl/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "," ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  (general-define-key
   :states 'insert
   "C-g" 'evil-normal-state) ;; don't stretch for ESC

  ;; unbind some annoying default bindings
  (general-unbind
    "C-x C-r"   ;; unbind find file read only
    "C-x C-z"   ;; unbind suspend frame
    "C-x C-d"   ;; unbind list directory
    "<mouse-2>") ;; pasting with mouse wheel click


  (patrl/leader-keys
    "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    ;;"TAB" '(:keymap tab-prefix-map :wk "tab") ;; remap tab bindings
    )

  (patrl/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; help
  ;; namespace mostly used by 'helpful'
  (patrl/leader-keys
    "h" '(:ignore t :wk "help"))

  ;; file
  (patrl/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file") ;; gets overridden by consult
    "fs" '(save-buffer :wk "save file"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (patrl/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
    "bk" '(kill-this-buffer :wk "kill this buffer")
    "br" '(revert-buffer :wk "reload buffer"))

  ;; bookmark
  (patrl/leader-keys
    "B" '(:ignore t :wk "bookmark")
    "Bs" '(bookmark-set :wk "set bookmark")
    "Bj" '(bookmark-jump :wk "jump to bookmark"))

  ;; universal argument
  (patrl/leader-keys
    "u" '(universal-argument :wk "universal prefix"))

  ;; notes
  ;; see 'citar' and 'org-roam'
  (patrl/leader-keys
    "n" '(:ignore t :wk "notes")
    ;; see org-roam and citar sections
    "na" '(org-todo-list :wk "agenda todos")) ;; agenda

  ;; code
  ;; see 'flymake'
  (patrl/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; open
  (patrl/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar")) ;; TODO this needs some love

  ;; search
  ;; see 'consult'
  (patrl/leader-keys
    "s" '(:ignore t :wk "search"))

  ;; templating
  ;; see 'tempel'
  (patrl/leader-keys
    "t" '(:ignore t :wk "template")))

;; "c" '(org-capture :wk "capture")))

;; copy from https://quant67.com/post/emcas/init-config.html
;; 默认的 mode-line 不是很好看，用 doom-modeline 好一些。
(use-package all-the-icons
  :if (display-graphic-p)
  ;; (set-fontset-font t 'symbol "Apple Color Emoji")
  ;; (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  ;; (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  ;; (set-fontset-font t 'symbol "Symbola" nil 'append)
  )

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; (use-package all-the-icons-dired
;;   :ensure t
;;   :defer t
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; https://www.emacswiki.org/emacs/KeyCast
;; copy from https://book.emacs-china.org/#org737719a
;; ;;modeline上显示我的所有的按键和执行的命令
;; https://sqrtminusone.xyz/configs/emacs/
(use-package keycast
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

;; 这里的执行顺序非常重要，doom-modeline-mode 的激活时机一定要在设置global-mode-string 之后‘
(use-package doom-modeline
  :ensure t
  :defer t
  :init
  (doom-modeline-mode 1)
  (setq doom-moeline-time nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-height 15)
  (setq doom-modeline-project-detection 'projectile)
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


;; copy from https://quant67.com/post/emcas/init-config.html
;; F8 侧边打开项目目录
;; (use-package neotree
;;   :config
;;   ;; f8 to view tree strucure of folder
;;   (defun neotree-project-dir ()
;;     "Open NeoTree using the git root."
;;     (interactive)
;;     (let ((project-dir (projectile-project-root))
;;           (file-name (buffer-file-name)))
;;       (neotree-toggle)
;;       (if project-dir
;;           (if (neo-global--window-exists-p)
;;               (progn
;;                 (neotree-dir project-dir)
;;                 (neotree-find file-name)))
;;         (message "Could not find git project root."))))
;;   (setq-default neo-show-hidden-files t)
;;   (global-set-key [f8] 'neotree-project-dir)
;;   ;; switch with projectile
;;   (use-package projectile)
;;   (setq projectile-switch-project-action 'neotree-projectile-action))

;; copy from [Highlight current active window](https://stackoverflow.com/questions/33195122/highlight-current-active-window)
(use-package auto-dim-other-buffers
  :ensure t
  :config
  (auto-dim-other-buffers-mode))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

;; (use-package dired-subtree
;;   :ensure t
;;   :after dired
;;   ;;:config
;;   ;;(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
;;   )

;; Share clipoard with OS
(use-package pbcopy
  :ensure t)

;; XML formatter
(reformatter-define xml-format
  :program "xmlformat"
  :group 'xml)

(with-eval-after-load 'nxml-mode
  (define-key nxml-mode-map (kbd "C-c C-f") 'xml-format-buffer))

;; copy fromhttps://devbins.github.io/post/emacs_flutter/
(use-package lsp-dart
  :init (setq lsp-dart-sdk-dir (concat (file-name-directory (file-truename (executable-find "flutter"))) "cache/dart-sdk"))
  :hook (dart-mode . lsp))

(use-package dart-mode
  :ensure t
  :defines (projectile-project-root-files-bottom-up)
  :if (or (executable-find "dart") (executable-find "flutter"))
  :bind (:map dart-mode-map
              ("C-c C-f" . dart-format-buffer)
              ("C-c C-c" . my/dart-run-file))
  :config
  (reformatter-define dart-format
    :program "dart"
    :args '("format")
    :group 'dart)
  (defun my/dart-run-file ()
    "Execute the code of the current file."
    (interactive)
    (compile (format "dart %s" (buffer-file-name))))
  (evil-leader/set-key-for-mode 'dart-mode "d" 'xref-find-definitions)
  (with-eval-after-load "projectile"
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))
  )

(use-package flutter
  :ensure t
  :after dart-mode
  :config
  (defun my/flutter-goto-logs-buffer()
    "Go to buffer logs buffer."
    (interactive)
    (let ((buffer (get-buffer flutter-buffer-name)))
      (unless buffer
        (user-error "flutter is not running."))
      (switch-to-buffer buffer)
      (goto-line (point-max))))
  :bind (:map dart-mode-map
              ("C-c C-r" . #'flutter-run-or-hot-reload)
              ("C-c C-l" . #'my/flutter-goto-logs-buffer))
  :hook (dart-mode . flutter-test-mode)
  :custom
  ;; sdk path will be the parent-parent directory of flutter cli
  (flutter-sdk-path (directory-file-name
                     (file-name-directory
                      (directory-file-name
                       (file-name-directory (file-truename (executable-find "flutter"))))))))

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

;; SQL formatter
(reformatter-define sql-format
  :program "pg_format")

(defun my/format-sql ()
  "Format active region otherwise format the entire buffer."
  (interactive)
  (if (region-active-p)
      (sql-format-region (region-beginning) (region-end))
    (sql-format-buffer)))

(with-eval-after-load 'sql
  (add-hook 'sql-mode-hook 'flymake-sqlfluff-load)
  (add-hook 'sql-mode-hook 'flymake-mode)
  (define-key sql-mode-map (kbd "C-c C-f") 'my/format-sql))

;; SQL linter using sqlfluff
(use-package flymake-sqlfluff
  :ensure t)

;; Org tree slide
(use-package hide-mode-line
  :ensure t)

(use-package org-tree-slide
  :ensure t
  :defer t
  :config
  (defun my/org-tree-slide-setup ()
    (org-display-inline-images)
    (hide-mode-line-mode 1))

  (defun my/org-tree-slide-end ()
    (org-display-inline-images)
    (hide-mode-line-mode 0))
  :custom
  (org-image-actual-width nil)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  :hook ((org-tree-slide-play . my/org-tree-slide-setup)
         (org-tree-slide-stop . my/org-tree-slide-end))
  :bind (:map org-tree-slide-mode-map
              ("C-<" . org-tree-slide-move-previous-tree)
              ("C->" . org-tree-slide-move-next-tree)))

;; latex
(use-package auctex
  :ensure t
  :defer t)

(use-package latex-preview-pane
  :ensure t
  :defer t)

;; git
(use-package git-link
  :ensure t
  :defer t)

(use-package git-modes
  :defer t
  :ensure t)

;; (use-package diff-hl
;;   :ensure t
;;   :custom
;;   (diff-hl-show-staged-changes nil)
;;   ;; for some reason the :hook form doesn't work so we have to use :init
;;   :init
;;   (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;   (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;;   :config
;;   (global-diff-hl-mode))

(use-package gist
  :ensure t
  :defer t)

(use-package linkode
  :ensure t
  :defer t)

(use-package git-timemachine)

;; Cursor 光标彩虹效果
(use-package beacon
  :ensure t
  :custom
  (beacon-color "blue")
  :config
  (setq-default beacon-size 15)
  (add-hook 'after-init-hook 'beacon-mode)
  (beacon-mode 1))


(use-package diredfl
  :ensure t
  :commands diredfl-global-mode
  :init
  (diredfl-global-mode)
  (put 'diredp-tagged-autofile-name 'face-alias 'diredfl-tagged-autofile-name)
  (put 'diredp-autofile-name 'face-alias 'diredfl-autofile-name)
  (put 'diredp-ignored-file-name 'face-alias 'diredfl-ignored-file-name)
  (put 'diredp-symlink 'face-alias 'diredfl-symlink)
  (put 'diredp-compressed-file-name 'face-alias 'diredfl-compressed-file-name)
  (put 'diredp-file-suffix 'face-alias 'diredfl-file-suffix)
  (put 'diredp-compressed-extensions 'face-alias 'diredfl-compressed-extensions)
  (put 'diredp-deletion 'face-alias 'diredfl-deletion)
  (put 'diredp-deletion-file-name 'face-alias 'diredfl-deletion-file-name)
  (put 'diredp-flag-mark-line 'face-alias 'diredfl-flag-mark-line)
  (put 'diredp-rare-priv 'face-alias 'diredfl-rare-priv)
  (put 'diredp-number 'face-alias 'diredfl-number)
  (put 'diredp-exec-priv 'face-alias 'diredfl-exec-priv)
  (put 'diredp-file-name 'face-alias 'diredfl-file-name)
  (put 'diredp-dir-heading 'face-alias 'diredfl-dir-heading)
  (put 'diredp-compressed-file-suffix 'face-alias 'diredfl-compressed-file-suffix)
  (put 'diredp-flag-mark 'face-alias 'diredfl-flag-mark)
  (put 'diredp-mode-set-explicitly 'face-alias 'diredfl-mode-set-explicitly)
  (put 'diredp-executable-tag 'face-alias 'diredfl-executable-tag)
  (put 'diredp-global-mode-hook 'face-alias 'diredfl-global-mode-hook)
  (put 'diredp-ignore-compressed-flag 'face-alias 'diredfl-ignore-compressed-flag)
  (put 'diredp-dir-priv 'face-alias 'diredfl-dir-priv)
  (put 'diredp-date-time 'face-alias 'diredfl-date-time)
  (put 'diredp-other-priv 'face-alias 'diredfl-other-priv)
  (put 'diredp-no-priv 'face-alias 'diredfl-no-priv)
  (put 'diredp-link-priv 'face-alias 'diredfl-link-priv)
  (put 'diredp-write-priv 'face-alias 'diredfl-write-priv)
  (put 'diredp-global-mode-buffers 'face-alias 'diredfl-global-mode-buffers)
  (put 'dired-directory 'face-alias 'diredfl-dir-name)
  (put 'diredp-read-priv 'face-alias 'diredfl-read-priv))

;; RET 后仅保留一个 dired buffer
;; For Emacs 28
(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  (put 'dired-find-alternate-file 'disabled nil)
  (setq
   dired-dwim-target t
   dired-clean-up-buffers-too t
   dired-recursive-copies 'always
   dired-recursive-deletes 'top
   ;; dired-listing-switches "lhvA"
   dired-omit-verbose nil
   dired-hide-details-hide-symlink-targets nil)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              ;; (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (hl-line-mode 1)))
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
  ;; copy from https://github.com/d12frosted/homebrew-emacs-plus/issues/383
  (when (eq system-type 'darwin)
    (setq insert-directory-program "/usr/local/bin/gls"))
  ;; (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first")
  )
;; 基于 Dired 的极简、一站式文件管理器
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("o" "~/Syncthings/org/"           "Org")
     ("r" "~/Syncthings/org/roam/"      "Roam")))
  :after (diredfl all-the-icons)
  :config
  ;; 异步读取含 10000 个以上文件的文件夹
  (setq dirvish-async-listing-threshold 10000)
  (setq dirvish-depth 0)
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-override-dired-mode +1)
  (setq dirvish-attributes '(all-the-icons file-size))
  (set-face-attribute 'dirvish-hl-line nil
                      :foreground (face-attribute 'diredfl-flag-mark :foreground)
                      :background (face-attribute 'diredfl-flag-mark :background))
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   ("C-x d" . dirvish)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("-"   . dired-jump)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(dirvish-define-preview exa (file)
                        "Use `exa' to generate directory preview."
                        :require ("exa") ; tell Dirvish to check if we have the executable
                        (when (file-directory-p file) ; we only interest in directories here
                          `(shell . ("exa" "-al" "--color=always" "--icons"
                                     "--group-directories-first" ,file))))

(add-to-list 'dirvish-preview-dispatchers 'exa)

(setq insert-directory-program "gls")

(use-package s)

(use-package dash
  :defer t)

;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list
;;   :config
;;   ;;(lsp-metals-treeview-enable t)
;;   (setq lsp-metals-treeview-show-when-views-received t))

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
(use-package ediff
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
  (hl-line ((nil (:background "light yellow"))))
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

;; 显示空白字符，如 \t \v \v 空格等等。
;; 可以配置在 prog-mode，markdown-mode 和 conf-mode 下，显示行尾的空白字符。
;;(use-package whitespace
;;  :ensure nil
;;  :hook
;;  (after-init . global-whitespace-mode)
;;
;;  ((prog-mode markdown-mode conf-mode) . whitespace-mode)
;;  :config
;;  ;; makefile等以tab为标识的文件中也会将tab转换为空格, 排除特定的mode呢
;;  (setq whitespace-global-modes '(not makefile-mode))
;;  ;; Don't use different background for tabs.
;;  (face-spec-set 'whitespace-tab
;;                 '((t :background unspecified)))
;;  ;; Only use background and underline for long lines, so we can still have
;;  ;; syntax highlight.
;;
;;  ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
;;  ;; is it's due to the variables with the same name as the faces in
;;  ;; whitespace.el.  Anyway, we have to manually set some attribute to
;;  ;; unspecified here.
;;  (face-spec-set 'whitespace-line
;;                 '((((background light))
;;                    :background "#d8d8d8" :foreground unspecified
;;                    :underline t :weight unspecified)
;;                   (t
;;                    :background "#404040" :foreground unspecified
;;                    :underline t :weight unspecified)))
;;
;;  ;; Use softer visual cue for space before tabs.
;;  (face-spec-set 'whitespace-space-before-tab
;;                 '((((background light))
;;                    :background "#d8d8d8" :foreground "#de4da1")
;;                   (t
;;                    :inherit warning
;;                    :background "#404040" :foreground "#ee6aa7")))
;;
;;  (setq
;;   whitespace-line-column nil
;;   whitespace-style
;;   '(face             ; visualize things below:
;;     empty            ; empty lines at beginning/end of buffer
;;     lines-tail       ; lines go beyond `fill-column'
;;     space-before-tab ; spaces before tab
;;     trailing         ; trailing blanks
;;     tabs             ; tabs (show by face)
;;     tab-mark         ; tabs (show by symbol)
;;     ))
;;  )

;; 当打开一个具有长行的文件时，它会自动检测并将一些可能导致严重性能的 mode 关闭， 如 syntax highlight。
(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

;; 有时候Emacs里打开的文件可能被外部修改，启用autorevert的话可以自动更新对应的 buffer.
(use-package autorevert
  :ensure t
  :hook (after-init . global-auto-revert-mode))

;; 来显示如 10/100 这种状态。
;; 在搜索中删除字符会回退搜索结果，而不是停在当前位置将最后一个搜 索字符删除。这里可以通过remap isearch-delete-char来实现。
;; 还可以将搜索结果保持在高亮状态以方便肉眼识别。这个是通过设置 lazy-highlight-cleanup为nil实现的。
;; 去除高亮状态需要人工M-x调用 lazy-highlight-cleanup。
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-highlight-cleanup nil))

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

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
;;(use-package tsi
;;  :after tree-sitter
;;  ;; define autoload definitions which when actually invoked will cause package to be loaded
;;  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;  :init
;;  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; auto-format different source code files extremely intelligently
;; apheleia 会折断长行, 非常耗费资源，不建议日常开启
;; https://github.com/radian-software/apheleia
;; (use-package apheleia
;;   :ensure t
;;   :config
;;   (apheleia-global-mode +1))

;; (global-linum-mode 1)
;; (setq linum-format "%3d ")
;; (add-hook 'prog-mode-hook 'linum-mode)
;; (use-package linum
;;   :init
;;   (progn
;;     (global-linum-mode t)
;;     (setq linum-format "%4d  ")
;;     (set-face-background 'linum nil)
;;     ))

;; copy from [极简Emacs开发环境配置](https://huadeyu.tech/tools/emacs-setup-notes.html)
;; Json
(use-package json-mode)

;; Yaml
(use-package yaml-mode)

;; Dockfile
(use-package dockerfile-mode)

;; Protobuf
(use-package protobuf-mode)

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :commands flycheck-rust-setup
  :config
  :init (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

;; Flycheck is a general syntax highlighting framework which other packages hook into. It's an improvment on the built in flymake.
;; Setup is pretty simple - we just enable globally and turn on a custom eslint function, and also add a custom checker for proselint.
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (setq flycheck-indication-mode 'right-fringe)
  ;; only check on save
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-highlighting-mode 'lines)
  ;; Define fringe indicator / warning levels
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info))

(use-package rustic
  :ensure
  :init
  (setq rustic-treesitter-derive t)
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-r l" . flycheck-list-errors)
              ("C-c C-r a" . lsp-execute-code-action)
              ("C-c C-r R" . lsp-rename)
              ("C-c C-r q" . lsp-workspace-restart)
              ("C-c C-r Q" . lsp-workspace-shutdown)
              ("C-c C-r s" . lsp-rust-analyzer-status)
              ("C-c C-r e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-r f" . rustic-format-buffer)
              ("C-c C-r h" . lsp-rust-analyzer-inlay-hints-mode)
              ("C-c C-r b" . rustic-cargo-build)
              ("C-c C-r c" . rustic-cargo-clean)
              ("C-c C-r r" . rustic-cargo-run)
              ("C-c C-r t" . rustic-cargo-test)
              ("C-c C-r y" . rustic-cargo-clippy)
              ;; ("C-c C-c d" . dap-hydra)
              ("C-c C-r g" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-lsp-format t)
  (setq rustic-format-trigger 'on-compile)
  (setq compilation-read-command nil) ;; not prompt on minibuffer when do compile.
  (push 'rustic-clippy flycheck-checkers)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; (setq lsp-rust-analyzer-cargo-watch-enable nil)
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save nil)
  (setq rustic-lsp-format nil)

  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-lsp-client 'lsp-mode)
  ;;(setq rustic-lsp-client 'eglot)
  (defun rk/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
      (setq-local buffer-save-without-query t))
    ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
    )
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;;(use-package eglot
;;  :ensure t
;;  :config
;;  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
;;  (add-hook 'rustic-mode-hook 'eglot-ensure)
;;  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename))

;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))
;; copy from https://nyk.ma/posts/emacs-write-your-own/
;; 由于 lsp-mode 的一次大更新，把使用方式变成如今调用 (lsp) 即可，所以目前 eglot 竞争力不强了。
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init ;; 在 (reuqire) 之前执行
  (setq
   ;; 尝试自动配置自己
   lsp-auto-configure t
   ;; 尝试自动猜测项目根文件夹
   lsp-auto-guess-root t
   ;; 多少时间idle后向服务器刷新信息
   lsp-idle-delay 0.500
   ;; 给缓存文件换一个位置
   lsp-session-file (no-littering-expand-var-file-name "lsp-sessions"))
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  ;;(lsp-eldoc-render-all t)
  (lsp-enable-snippet t)
  (lsp-keep-workspace-alive t)
  (lsp-enable-xref t)
  (lsp-enable-imenu t)
  (lsp-enable-completion-at-point t)
  ;; copy from https://github.com/emacs-lsp/lsp-mode/issues/3231
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-headerline-breadcrumb-segments '(file symbols))
  (lsp-modeline-diagnostics-enable  t)
  (lsp-modeline-diagnostics-scope  :project)
  (lsp-completion-provider  :capf)
  ;; Disable "use" statement insertion.
  (lsp-intelephense-completion-insert-use-declaration nil)
  ;; Decrease completion suggestions to 25.
  (lsp-intelephense-completion-max-items 25)
  ;; Disable breadcrumbs for all modes.
  (lsp-headerline-breadcrumb-enable nil)
  ;; Setup licence key.
  (lsp-intelephense-licence-key "KEY-GOES-HERE")
  ;; Disable multi-root server.
  (lsp-intelephense-multi-root nil)
  (lsp-intelephense-storage-path (no-littering-expand-var-file-name "lsp-cache"))
  (lsp-intelephense-global-storage-path (no-littering-expand-var-file-name "intelephense"))
  ;; Disable telemetry.
  (lsp-intelephense-telemetry-enabled nil)
  ;; Show verbose output from the intelephense server.
  (lsp-intelephense-trace-server "verbose")
  ;; Add "exclude_me" directory to list of excluded directories.
  ;; (setq lsp-intelephense-files-exclude
  ;; (vconcat ["**/exclude_me/**"] lsp-intelephense-files-exclude))
  ;; Reduce max file size to 100kb
  (lsp-intelephense-files-max-size 100000)

  :hook
  ;; (php-mode . lsp)
  ;; 在哪些语言 major mode 下启用 LSP
  ((c-mode
    c++-mode
    python-mode
    sh-mode
    rust-mode
    rustic-mode
    php-mode
    lua-mode
    html-mode
    json-mode
    dockerfile-mode
    css-mode
    yaml-mode
    typescript-mode
    go-mode) .
    (lambda ()
      (hack-local-variables)
      (lsp)
      (which-function-mode)
      ))
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  ;; copy from [极简Emacs开发环境配置](https://huadeyu.tech/tools/emacs-setup-notes.html)
  ;; (add-hook 'go-mode-hook #'lsp)
  ;; (add-hook 'python-mode-hook #'lsp)
  ;; (add-hook 'c++-mode-hook #'lsp)
  ;; (add-hook 'c-mode-hook #'lsp)
  ;; (add-hook 'rust-mode-hook #'lsp)
  ;; (add-hook 'html-mode-hook #'lsp)
  ;; (add-hook 'php-mode-hook #'lsp)
  ;;(add-hook 'js-mode-hook #'lsp)
  ;;(add-hook 'typescript-mode-hook #'lsp)
  ;; (add-hook 'json-mode-hook #'lsp)
  ;; (add-hook 'yaml-mode-hook #'lsp)
  ;; (add-hook 'dockerfile-mode-hook #'lsp)
  ;; (add-hook 'shell-mode-hook #'lsp)
  ;; (add-hook 'css-mode-hook #'lsp)
  ;; (add-hook 'lua-mode-hook #'lsp)
  ;; copy from https://sagot.dev/en/articles/emacs-typescript/
  ;; (add-hook 'typescript-mode-hook 'lsp-deferred)
  ;;(add-hook 'javascript-mode-hook 'lsp-deferred)
  (with-eval-after-load "lsp-rust"
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda ()
                         `(,(or (executable-find
                                 (cl-first lsp-rust-analyzer-server-command))
                                (lsp-package-path 'rust-analyzer)
                                "rust-analyzer")
                           ,@(cl-rest lsp-rust-analyzer-server-args))))
      :remote? t
      :major-modes '(rust-mode rustic-mode)
      :initialization-options 'lsp-rust-analyzer--make-init-options
      :notification-handlers (ht<-alist lsp-rust-notification-handlers)
      :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
      :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
      :after-open-fn (lambda ()
                       (when lsp-rust-analyzer-server-display-inlay-hints
                         (lsp-rust-analyzer-inlay-hints-mode)))
      :ignore-messages nil
      :server-id 'rust-analyzer-remote)))
  ;; (with-eval-after-load "lsp-php"
  ;;   (if (featurep 'no-littering)
  ;;       (setq
  ;;        lsp-intelephense-storage-path (no-littering-expand-var-file-name "lsp-cache")
  ;;        lsp-intelephense-global-storage-path (no-littering-expand-var-file-name "intelephense"))
  ;;     )
  ;;   )
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
                    :major-modes '(python-mode)
                    :server-id 'pyls))
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.500) ;; default is 0.2
  ;;(require 'lsp-clients)
  (setq lsp-completion-provider :none) ;; 阻止 lsp 重新设置 company-backend 而覆盖我们 yasnippet 的设置
  (setq lsp-headerline-breadcrumb-enable t)

  (setq lsp-enable-file-watchers nil)
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  (setq lsp-print-performance t)
  (setq lsp-auto-guess-root t) ; auto detect workspace and start lang server
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; copy from https://emacs-china.org/t/doom-emacs-lsp-lua-mode/16432/7
  ;; lua
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-lua-language-server/
  (setq
   ;; "/usr/local/Cellar/lua-language-server/3.6.6/"
   lsp-clients-lua-language-server-install-dir (substring (file-name-directory (file-truename (executable-find "lua-language-server"))) 0 -4)
   lsp-clients-lua-language-server-bin (f-join lsp-clients-lua-language-server-install-dir "bin/lua-language-server")
   lsp-clients-lua-language-server-main-location (f-join lsp-clients-lua-language-server-install-dir "libexec/main.lua")
   lsp-lua-workspace-max-preload 8192
   lsp-lua-workspace-preload-file-size 1024
   )
  )

(use-package lsp-ui
  :ensure t
  ;; 仅在某软件包被加载后再加载
  :after (lsp-mode)
  ;; :requires use-package-hydra
  :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
        ;; 查询符号定义：使用 LSP 来查询。通常是 M-.
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ;; 查询符号引用：使用 LSP 来查询。通常是 M-?
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ;; 该文件里的符号列表：类、方法、变量等。前提是语言服务支持本功能。
        ("C-c u" . lsp-ui-imenu))
  ;; 当 lsp 被激活时自动激活 lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; copy from [A guide on disabling/enabling lsp-mode features](https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/)
  (setq
   ;; Symbol highlighting
   lsp-enable-symbol-highlighting t
   lsp-print-io nil
   lsp-prefer-flymake :none
   flycheck-checker-error-threshold 10000
   lsp-ui-flycheck-enable t
   lsp-ui-flycheck-list-position 'right
   lsp-ui-flycheck-live-reporting t
   lsp-ui-peek-enable t
   lsp-ui-peek-list-width 60
   lsp-ui-peek-peek-height 25
   lsp-ui-imenu-enable t
   ;; you could manually request them via `lsp-signature-activate`
   lsp-signature-auto-activate nil
   ;; Signature help documentation (keep the signatures)
   lsp-signature-render-documentation nil
   ;; lsp-ui-doc - on hover dialogs. * disable via
   lsp-ui-doc-enable t
   ;; Lenses
   lsp-lens-enable t
   ;; Headerline
   lsp-headerline-breadcrumb-enable t
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-show-with-mouse nil
   ;; Sideline code actions * disable whole sideline via
   lsp-ui-sideline-enable t
   lsp-ui-sideline-show-code-actions t
   ;; hover symbols
   lsp-ui-sideline-show-hover t
   lsp-completion-show-detail t
   lsp-completion-show-kind t
   lsp-ui-sideline-show-diagnostics t
   lsp-modeline-code-actions-enable nil
   lsp-eldoc-enable-hover nil
   lsp-modeline-diagnostics-enable nil
   lsp-ui-peek-always-show t
   lsp-ui-sideline-ignore-duplicate t)
  ;; (setq-local flycheck-checker 'python-flake8)
  )

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (defun company-yasnippet-or-completion ()
    (interactive)
    (or (do-yas-expand)
        (company-complete-common)))

  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "::") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))

  ;; setq 可以像这样连着设置多个变量的值
  (setq
   ;; 注释贴右侧对齐
   company-tooltip-align-annotations t
   ;; 菜单里可选项数量
   company-tooltip-limit 20
   ;; 显示编号（然后可以用 M-数字 快速选定某一项）
   company-show-numbers t
   ;; 延时多少秒后弹出
   company-idle-delay 0
   ;; 至少几个字符后开始补全
   company-minimum-prefix-length 1
   company-selection-wrap-around t
   ;; 根据选择的频率进行排序，读者如果不喜欢可以去掉
   company-transformers '(company-sort-by-occurrence)
   ;; copy from [Emacs + Company-Mode 配置多个补全后端](https://manateelazycat.github.io/emacs/2021/06/30/company-multiple-backends.html)
   ;; Customize company backends.
   company-backends
   '(
     (company-tabnine company-dabbrev company-keywords company-files company-capf)
     )
   )
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

  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  ;;(:map company-mode-map
  ("<tab>". tab-indent-or-complete)
  ("TAB". tab-indent-or-complete))

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

(use-package company-tabnine
  :ensure t
  :init (add-to-list 'company-backends #'company-tabnine))

;; Create / cleanup rust scratch projects quickly
(use-package rust-playground :ensure)

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
  ;;(require 'dap-gdb-lldb)
  ;;(dap-gdb-lldb-setup)
  ;;(require 'dap-codelldb)
  ;;(dap-codelldb-setup)
  (require 'dap-cpptools)
  ;;(dap-cpptools-setup)
  ;; (require 'dap-lldb)
  (add-hook 'dap-stopped-hook
            (lambda (arg)
              (call-interactively #'dap-hydra)))
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
;; (use-package dap-mode
;;   :ensure
;;   :config
;;   (dap-ui-mode)
;;   (dap-ui-controls-mode 1)

;;   (require 'dap-lldb)
;;   (require 'dap-gdb-lldb)
;;   ;; installs .extension/vscode
;;   (dap-gdb-lldb-setup)
;;   (dap-register-debug-template
;;    "Rust::LLDB Run Configuration"
;;    (list :type "lldb"
;;          :request "launch"
;;          :name "LLDB::Run"
;; 	     :gdbpath "rust-lldb"
;;          ;; uncomment if lldb-mi is not in PATH
;;          ;; :lldbmipath "path/to/lldb-mi"
;;          )))

(use-package dap-lldb
  :ensure nil
  :after dap-mode
  :config
  ;; (setq dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
  (setq dap-lldb-debug-program '((executable-find "lldb-vscode")))
  ;; ask user for executable to debug if not specified explicitly (c++)
  (setq dap-lldb-debugged-program-function
	    (lambda () (read-file-name "Select file to debug: "))))

;; copy from https://gitter.im/emacs-lsp/lsp-mode?at=5f7fea9824a20801a8d60649
(use-package rust-mode
  :ensure t
  :mode
  ("\\.rs\\'" . rust-mode)
  :hook
  (rust-mode . flycheck-mode)
  (rust-mode . lsp)
  :init (setq lsp-rust-server 'rust-analyzer)
  :config
  (setq rust-format-on-save t)
  (setq lsp-completion-provider :capf)
  (setq lsp-progress-via-spinner t)
  (require 'lsp-mode)
  (add-hook 'rust-mode-hook (lambda ()
                              (flycheck-rust-setup)
                              (lsp)
                              (flycheck-mode)
                              (yas-minor-mode)
                              ))
  )

(use-package cargo
  :ensure t
  :commands cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))


;; copy from https://zenn.dev/yukit/articles/25a88b33a35633
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (add-to-list 'exec-path (expand-file-name  "~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin"))
    (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
    ))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (add-to-list 'exec-path (expand-file-name  "~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin"))
    (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
    (setq rustic-analyzer-command '("~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/rust-analyzer"))
    (setq lsp-rust-analyzer-server-command '("~/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/rust-analyzer"))
    ))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (add-to-list 'exec-path (expand-file-name "/backup/backup/rust_installation/cargo/bin"))
    (add-to-list 'exec-path (expand-file-name "/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-apple-darwin/bin"))
    (setq rustic-analyzer-command '("/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))
    (setq lsp-rust-analyzer-server-command '("/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))
    )))

;; copy from [Rust development environment for Emacs](https://rustrepo.com/repo/brotzeit-rustic-rust-ides)
;; (defun rustic-mode-auto-save-hook ()
;;   "Enable auto-saving in rustic-mode buffers."
;;   (when buffer-file-name
;;     (setq-local compilation-ask-about-save nil)))
;; (add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

;; 正在从ivy、swiper、counsel、hydra转向vertico、consult、embark、orderless。
;; 增强 minibuffer 补全：vertico 和 Orderless, 垂直补全
(use-package vertico
  :init
  (vertico-mode)
  :config
  ;; (vertico-multiform-mode)
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
	    (lambda (&rest args)
	      (apply (if vertico-mode
		             #'consult-completion-in-region
		           #'completion--in-region)
		         args)))
  (setq read-file-name-completion-ignore-case t
	    read-buffer-completion-ignore-case t
	    completion-ignore-case t)
  (setq vertico-cycle t))

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
  (setq completion-styles '(orderless basic)
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
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  (embark-collect-mode . embark-consult-preview-minor-mode)
  )

;; 增强文件内搜索和跳转函数定义：Consult
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
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
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
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
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ;; (global-set-key (kbd "C-s") 'my-isearch-or-consult)
         ;; ("C-s" . my-isearch-or-consult)
         ("C-s" . consult-line-multi)
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  (projectile-load-known-projects)
  (setq my-consult-source-projectile-projects
        `(:name "Projectile projects"
                :narrow   ?P
                :category project
                :action   ,#'projectile-switch-project-by-name
                :items    ,projectile-known-projects))
  (add-to-list 'consult-buffer-sources my-consult-source-projectile-projects 'append)
  ;; copy from https://tam5917.hatenablog.com/entry/2021/05/01/063232
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  ;; (defun my-isearch-or-consult (use-consult)
  ;;   (interactive "p")
  ;;   (cond ((eq use-consult 1)
  ;;          (call-interactively 'isearch-forward))
  ;;         ((eq use-consult 4)
  ;;          (call-interactively 'consult-line-symbol-at-point))
  ;;         ((eq use-consult 16)
  ;;          (call-interactively 'consult-line-migemo))))
  )

;; (global-set-key (kbd "C-s") 'consult-line)

(use-package consult-flycheck)

;; copy from https://huadeyu.tech/tools/emacs-setup-notes.html
;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay      0.5
;;           treemacs-directory-name-transformer    #'identity
;;           treemacs-display-in-side-window        t
;;           treemacs-eldoc-display                 t
;;           treemacs-file-event-delay              1000
;;           treemacs-file-extension-regex          treemacs-last-period-regex-value
;;           treemacs-file-follow-delay             0.2
;;           treemacs-file-name-transformer         #'identity
;;           treemacs-follow-after-init             t
;;           treemacs-git-command-pipe              ""
;;           treemacs-goto-tag-strategy             'refetch-index
;;           treemacs-indentation                   2
;;           treemacs-indentation-string            " "
;;           treemacs-is-never-other-window         nil
;;           treemacs-max-git-entries               5000
;;           treemacs-missing-project-action        'ask
;;           treemacs-no-png-images                 nil
;;           treemacs-no-delete-other-windows       t
;;           treemacs-project-follow-cleanup        nil
;;           ;; treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-persist-file (no-littering-expand-etc-file-name '"treemacs-persist.org")
;;           ;; treemacs–last-error-persist-file (no-littering-expand-etc-file-name '“treemacs-last-error-persist-file.org”)
;;           treemacs-position                      'left
;;           treemacs-recenter-distance             0.1
;;           treemacs-recenter-after-file-follow    nil
;;           treemacs-recenter-after-tag-follow     nil
;;           treemacs-recenter-after-project-jump   'always
;;           treemacs-recenter-after-project-expand 'on-distance
;;           treemacs-show-cursor                   nil
;;           treemacs-show-hidden-files             t
;;           treemacs-silent-filewatch              nil
;;           treemacs-silent-refresh                nil
;;           treemacs-sorting                       'alphabetic-asc
;;           treemacs-space-between-root-nodes      t
;;           treemacs-tag-follow-cleanup            t
;;           treemacs-tag-follow-delay              1.5
;;           treemacs-user-mode-line-format         nil
;;           treemacs-width                         35)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode t)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)

;; 保存光标历史，记住上个命令
;; copy from https://book.emacs-china.org/#orga142e60
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
	          history-length 1000
	          savehist-additional-variables '(mark-ring
					                          global-mark-ring
					                          search-ring
					                          regexp-search-ring
					                          extended-command-history)
	          savehist-autosave-interval 300)
  :config
  ;; (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (setq savehist-file (concat user-emacs-directory "var/savehist")
        savehist-save-minibuffer-history 1
        )
  )

;; 显示文件列
(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode)
  :init
  ;; 高亮显示选中区域
  (transient-mark-mode t)
  ;; 高亮选中区域颜色
  ;; (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
  (custom-set-faces
   '(region
     ;; ((nil (:background "#666" :foreground "#ffffff")))
     ((nil (:background "purple" :foreground "black")))
     ))
  (progn
    (setq column-number-mode t)
    ))

;; copy from https://blog.sumtypeofway.com/posts/emacs-config.html
(use-package recentf
  :init
  ;; (add-to-list 'recentf-exclude "\\elpa")
  ;; (add-to-list 'recentf-exclude "private/tmp")
  ;; 2000 files ought to be enough.
  (setq recentf-max-saved-items 2000)
  ;;(setq recentf-max-menu-items 5000)
  (setq recentf-auto-cleanup 'never)  ;
  (setq recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"  "~$" "^/ftp:" "^/ssh:" "sync-recentf-marker" (expand-file-name "var/undohist/*" user-emacs-directory)))
  ;; (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  (setq recentf-save-file "~/.emacs.d/var/recentf")
  ;; (bind-key "C-c っ" 'helm-recentf)
  ;; (bind-key "C-c t" 'helm-recentf)
  (recentf-mode 1)
  ;;(run-at-time nil (* 5 60) 'recentf-save-list)
  )

(use-package sync-recentf
  :ensure t
  :config
  (setq recentf-auto-cleanup 60)
  (recentf-mode 1)
  :init
  ;; defvar 必须在init修改，才起效
  (setq sync-recentf-marker (no-littering-expand-var-file-name "sync-recentf-marker"))
  )

;; (defun suppress-messages (func &rest args)
;;   "Suppress message output from FUNC."
;;   ;; Some packages are too noisy.
;;   ;; https://superuser.com/questions/669701/emacs-disable-some-minibuffer-messages
;;   (cl-flet ((silence (&rest args1) (ignore)))
;;     (advice-add 'message :around #'silence)
;;     (unwind-protect
;;         (apply func args)
;;       (advice-remove 'message #'silence))))

;; ;; Suppress "Cleaning up the recentf...done (0 removed)"
;; (advice-add 'recentf-cleanup :around #'suppress-messages)
;; (defconst recentf-used-hooks
;;   '(
;;     (find-file-hook       recentf-track-opened-file)
;;     (write-file-functions recentf-track-opened-file)
;;     (kill-buffer-hook     recentf-track-closed-file)
;;     (kill-emacs-hook      recentf-save-list)
;;     )
;;   "Hooks used by recentf.")
;; (defun recentf-save-list/silent ()
;;   (let ((save-silently t)) (recentf-save-list)))

(use-package yasnippet-snippets
  :disabled
  )

;; (use-package counsel-projectile
;;   :after (counsel projectile)
;;   )

;; Hit M-m, expand up to the next largest region based on mode-context sensitive scope.
;; (use-package expand-region
;;   :ensure expand-region
;;   :bind (("M-#" . er/mark-symbol)
;;          ("M-m" . er/expand-region))
;;   :commands (er/expand-region er/enable-mode-expansions))

(use-package deadgrep
  :commands (deadgrep)
  :config
  (defun my/deadgrep-fix-buffer-advice (fun &rest args)
    (let ((buf (apply fun args)))
      (with-current-buffer buf
        (toggle-truncate-lines 1))
      buf))
  (advice-add #'deadgrep--buffer :around #'my/deadgrep-fix-buffer-advice))

(use-package perspective
  :init
  ;; (setq persp-show-modestring 'header)
  (setq persp-sort 'created)
  (setq persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode)
  ;;(my-leader-def "x" '(:keymap perspective-map :which-key "perspective"))
  (general-define-key
   :keymaps 'override
   :states '(normal emacs)
   "gt" 'persp-next
   "gT" 'persp-prev
   "gn" 'persp-switch
   "gN" 'persp-kill)
  (general-define-key
   :keymaps 'perspective-map
   ;;"b" 'persp-ivy-switch-buffer
   ;;"x" 'persp-ivy-switch-buffer
   "u" 'persp-ibuffer))

(use-package flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup))

(use-package dashboard
  :ensure t
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
  )

;; (use-package highlight-symbol
;;   :ensure t
;;   :init (highlight-symbol-mode)
;;   ;; :bind ("<f5>" . highlight-symbol)  ;; 按下 F3 键就可高亮当前符号
;;   :config
;;   (setq highlight-symbol-idle-delay 1.0)
;;   )

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

;; (use-package company-tabnine :ensure t)

(use-package lsp-docker
  :ensure t
  )

;; Make rectangular region marking easier.
(use-package rect-mark
  :ensure nil
  :bind (("C-x r C-SPC" . rm-set-mark)
         ("C-x r C-x" . rm-exchange-point-and-mark)
         ("C-x r C-k" . rm-kill-region)
         ("C-x r M-w" . rm-kill-ring-save)))

;; Tramp should default to the sshx mode.
(use-package tramp
  :commands tramp
  :config
  (setq tramp-default-method "sshx"))

;; copy from https://github.com/jwiegley/use-package/issues/320
;; Make buffer names unique, handy when opening files with similar names
(use-package uniquify
  :ensure nil
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*")))

;; copy from https://se30.xyz/conf.html
;; Turn on ansi in shells
;; (use-package ansi-color
;;   :ensure ansi-color
;;   :commands shell
;;   :config
;;   (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

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

;; theme
;; (use-package spacemacs-theme
;;   :defer t
;;   :init (load-theme 'spacemacs-dark t))


;; 自动保存
(use-package super-save
  :ensure t
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  ;;  exclude specific files from super-save, not save gpg files
  (setq super-save-exclude '(".gpg"))
  ;; predicates must not take arguments and return nil, when current buffer shouldn't save.
  (add-to-list 'super-save-predicates
               (lambda ()
                 (not (eq major-mode 'markdown-mode))))
  )

(use-package magit
  :general
  (lc/leader-keys
   "g b" 'magit-blame
   "g g" 'magit-status
   "g G" 'magit-status-here
   "g l" 'magit-log)
  (general-nmap
    :keymaps '(magit-status-mode-map
               magit-stash-mode-map
               magit-revision-mode-map
               magit-process-mode-map
               magit-diff-mode-map)
    "TAB" #'magit-section-toggle
    "<escape>" #'transient-quit-one)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
  (setq git-commit-fill-column 72)
  ;; (setq magit-log-margin (t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  ;; (when lc/is-ipad (require 'sendmail))
  :config
  (setq magit-buffer-name-format (concat "*" magit-buffer-name-format "*"))
  (with-eval-after-load 'magit ;; your code
    ;; copy from [在 magit 中使用 difftastic](https://emacs-china.org/t/magit-difftastic/23207)
    (defun my/magit--with-difftastic (buffer command)
      "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
      (let ((process-environment
             (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                           (number-to-string (frame-width)))
                   process-environment)))
        ;; Clear the result buffer (we might regenerate a diff, e.g., for
        ;; the current changes in our working directory).
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (erase-buffer))
        ;; Now spawn a process calling the git COMMAND.
        (make-process
         :name (buffer-name buffer)
         :buffer buffer
         :command command
         ;; Don't query for running processes when emacs is quit.
         :noquery t
         ;; Show the result buffer once the process has finished.
         :sentinel (lambda (proc event)
                     (when (eq (process-status proc) 'exit)
                       (with-current-buffer (process-buffer proc)
                         (goto-char (point-min))
                         (ansi-color-apply-on-region (point-min) (point-max))
                         (setq buffer-read-only t)
                         (view-mode)
                         (end-of-line)
                         ;; difftastic diffs are usually 2-column side-by-side,
                         ;; so ensure our window is wide enough.
                         (let ((width (current-column)))
                           (while (zerop (forward-line 1))
                             (end-of-line)
                             (setq width (max (current-column) width)))
                           ;; Add column size of fringes
                           (setq width (+ width
                                          (fringe-columns 'left)
                                          (fringe-columns 'right)))
                           (goto-char (point-min))
                           (pop-to-buffer
                            (current-buffer)
                            `(;; If the buffer is that wide that splitting the frame in
                              ;; two side-by-side windows would result in less than
                              ;; 80 columns left, ensure it's shown at the bottom.
                              ,(when (> 80 (- (frame-width) width))
                                 #'display-buffer-at-bottom)
                              (window-width
                               . ,(min width (frame-width))))))))))))
    (defun my/magit-show-with-difftastic (rev)
      "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
      (interactive
       (list (or
              ;; If REV is given, just use it.
              (when (boundp 'rev) rev)
              ;; If not invoked with prefix arg, try to guess the REV from
              ;; point's position.
              (and (not current-prefix-arg)
                   (or (magit-thing-at-point 'git-revision t)
                       (magit-branch-or-commit-at-point)))
              ;; Otherwise, query the user.
              (magit-read-branch-or-commit "Revision"))))
      (if (not rev)
          (error "No revision specified")
        (my/magit--with-difftastic
         (get-buffer-create (concat "*git show difftastic " rev "*"))
         (list "git" "--no-pager" "show" "--ext-diff" rev))))
    (defun my/magit-diff-with-difftastic (arg)
      "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
      (interactive
       (list (or
              ;; If RANGE is given, just use it.
              (when (boundp 'range) range)
              ;; If prefix arg is given, query the user.
              (and current-prefix-arg
                   (magit-diff-read-range-or-commit "Range"))
              ;; Otherwise, auto-guess based on position of point, e.g., based on
              ;; if we are in the Staged or Unstaged section.
              (pcase (magit-diff--dwim)
                ('unmerged (error "unmerged is not yet implemented"))
                ('unstaged nil)
                ('staged "--cached")
                (`(stash . ,value) (error "stash is not yet implemented"))
                (`(commit . ,value) (format "%s^..%s" value value))
                ((and range (pred stringp)) range)
                (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
      (let ((name (concat "*git diff difftastic"
                          (if arg (concat " " arg) "")
                          "*")))
        (my/magit--with-difftastic
         (get-buffer-create name)
         `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))
    (transient-define-prefix my/magit-aux-commands ()
      "My personal auxiliary magit commands."
      ["Auxiliary commands"
       ("d" "Difftastic Diff (dwim)" my/magit-diff-with-difftastic)
       ("s" "Difftastic Show" my/magit-show-with-difftastic)])
    (transient-append-suffix 'magit-dispatch "!"
      '("#" "My Magit Cmds" my/magit-aux-commands))

    (define-key magit-status-mode-map (kbd "#") #'my/magit-aux-commands)
    )
  )

;; (use-package unobtrusive-magit-theme
;;   :defer t
;;   :init
;;   (defun change-new-theme ()
;;     (load-theme 'manoj-dark)
;;     (load-theme 'unobtrusive-magit)
;;     )
;;   :hook
;;   (magit-mode . change-new-theme)
;;   )

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

;; copy from https://www.zhihu.com/column/p/23359721
;; (use-package dired-ranger
;;   :ensure t
;;   :bind (:map dired-mode-map
;;               ("W" . dired-ranger-copy)
;;               ("X" . dired-ranger-move)
;;               ("Y" . dired-ranger-paste)))

;; copy from https://www.lucacambiaghi.com/vanilla-emacs/readme.html
;; 安装后可以通过 M-x restart-emacs 重启 emacs
(use-package restart-emacs
  :bind ("C-c x r" . restart-emacs))

;; copy from https://codeberg.org/ideasman42/emacs-elisp-autofmt
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;;; plantuml
;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :init
  ;; (setq plantuml-jar-path "d:/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  )

;; Will automated download images for the first time
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode)
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
  :config
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
  :bind (
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c D" . crux-delete-file-and-buffer)
         ;; 优化版的回到行首
         ("C-a" . crux-move-beginning-of-line)
         ;; 快速连接两行等
         ("C-c ^" . crux-top-join-line)
         ;; 快速打开Emacs配置文件
         ("C-x ," . crux-find-user-init-file)
         ("C-c k" . crux-smart-kill-line)
  ))

;; switch-window
;; How to navigate between windows
;; (use-package ace-window
;;   :ensure t
;;   :config
;;   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;   (custom-set-faces
;;    '(aw-leading-char-face
;;      ((t (:inherit ace-jump-face-foreground :height 5.0)))))
;;   ;; :bind
;;   ;; ("M-o" . ace-window)
;;   )

;; How to delete consecutive space at once
(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks))

;; M-<up> M-<down> to move line up and down
(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))



;; In some case I want to hide the mode line
;; (use-package hide-mode-line
;;   :ensure t)

;; Nyan Cat is lovely, it can live on mode line
(use-package nyan-mode
  :ensure t
  :init
  (setq nyan-animate-nyancat t)
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
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)))

;; Display line changes in gutter based on git history. Enable it everywhere.
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t))

;; TimeMachine lets us step through the history of a file as recorded in git.
(use-package git-timemachine
  :ensure t)

;; Color Identifier
(use-package color-identifiers-mode
  :ensure t
  :commands color-identifiers-mode)

;; elixir
(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t)

;; Emacs has a great built in C/C++ mode, but we can improve on it with irony-mode for code completion via libclang.
(use-package irony
  :ensure t
  :hook (c-mode . irony-mode))

;; Add company mode support.
(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

;; Add flycheck support.
(use-package flycheck-irony
  :ensure t
  :hook (flycheck-mode . flycheck-irony-setup))

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

;; Capture my task or idea
;; (use-package org-capture
;;   :bind ("C-c c" . org-capture)
;;   :after org
;;   :config
;;   (add-to-list 'org-capture-templates
;;                '("t" "Todo"  entry
;;                  (file "~/Documents/org/todo.org")
;;                  "* TODO %?" :empty-lines 0))

;;   (add-to-list 'org-capture-templates
;;                '("w" "Work" entry
;;                  (file+olp "~/Documents/org/work.org" "2021")
;;                  "* %?" :empty-lines 0)))
;; Beautify Org heading symbol
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))

;; Emoji Org tag
(use-package org-pretty-tags
  :diminish org-pretty-tags-mode
  :ensure t
  :config
  (setq org-pretty-tags-surrogate-strings
        '(
          ("work"  . "⚒")
          ("@pc" . "🖥")
          ("@ps5" . "🎮")
          ("@switch" . "🕹")
          ("script" . "📝")
          ))
  (org-pretty-tags-global-mode))

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

;; Org fancy Priorities
(use-package org-fancy-priorities
  :diminish
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

;; Interactive agenda in the console
;; (use-package org-agenda
;;   :bind ("C-c a" . org-agenda)
;;   :config
;;   (setq org-agenda-files (directory-files-recursively "~/Documents/org/" "\\.org$"))
;;   ;; (setq org-agenda-files '(
;;   ;;                          "~/Documents/org/work.org"
;;   ;;                          "~/Documents/org/reminder.org"
;;   ;;                         ))
;;   (setq org-agenda-start-with-log-mode t)
;;   (setq org-agenda-prefix-format
;;         '((agenda . " %i %-24:c%?-16t%-10e% s")
;;           (todo   . " %i %-24:c %-10e")
;;           (tags   . " %i %-24:c")
;;           (search . " %i %-24:c")))

;;   ;;https://www.philnewton.net/blog/how-i-get-work-done-with-emacs/
;;   (setq org-agenda-custom-commands
;;         '(("d" "Today's Tasks"
;;            ((agenda "" ((org-agenda-span 1)
;;                         (org-agenda-overriding-header "Today's Tasks")))))))
;;   )

;; (use-package org-roam
;;   :ensure t
;;   :diminish org-roam-mode
;;   :hook
;;   (after-init . org-roam-mode)
;;   :custom
;;   (org-roam-directory "~/Documents/org/roam/")
;;   (org-roam-db-update-method 'immediate)
;;   (org-roam-completion-system 'ivy)
;;   :bind
;;   (:map org-roam-mode-map
;;         (("C-c n l" . org-roam)
;;          ("C-c n f" . org-roam-find-file)
;;          ("C-c n g" . org-roam-graph))
;;         :map org-mode-map
;;         (("C-c n i" . org-roam-insert))
;;         (("C-c n I" . org-roam-insert-immediate))))

;; (use-package org-roam-server
;;   :ensure t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8686
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))

;; English Chinese Dictionary
(use-package youdao-dictionary
  :ensure t
  :config
  (setq url-automatic-caching t))

;; Disk Usage
(use-package disk-usage
  :ensure t)

(use-package shell-pop
  :ensure t
  :custom
  (shell-pop-shell-type '("vterm" "*vterm*" (lambda () (vterm))))
  (shell-pop-full-span t))

;; view PDF in emacs
(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)))
;;(pdf-tools-install)
;;(pdf-info-check-epdfinfo)
;; copy  from https://www.songofcode.com/dotfiles/

;; C/C++
(use-package cc-mode
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  ;; TODO: open these hooks.
  ;; Sadly I work with a dirty team, i will change the whole code base if
  ;; uncommend config below.
  ;; (before-save-hook . lsp-format-buffer)
  ;; (before-save-hook . lsp-organize-imports)
  :config
  (define-key c-mode-base-map (kbd "M-/") 'ff-find-related-file)
  ;; Open a header file in C++ mode by defaults
  ;;(add-auto-mode 'c++-mode "\\.h\\'")
  )

(use-package cmake-mode
  :init
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))


;; C++20 highlighting
(use-package modern-cpp-font-lock
  :diminish nil
  :hook
  (c++-mode . modern-c++-font-lock-mode)
  (modern-c++-font-lock-mode . (lambda () (diminish
                                           'modern-c++-font-lock-mode))))

;; google cpplint
(use-package flycheck-google-cpplint
  :config
  (with-eval-after-load 'flycheck
    '(progn
       (require 'flycheck-google-cpplint)
       ;; Add Google C++ Style checker.
       ;; In default, syntax checked by Clang and Cppcheck.
       (flycheck-add-next-checker 'c/c++-cppcheck
                                  '(warning . c/c++-googlelint)))))

;; google style, but with 4 space indent.
(defun google-set-c-style-with-4-indent ()
  "Set current buffer to google style, but with 4 space indent."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "Google" google-c-style t)
  (setq tab-width 4
        c-indent-tabs-mode t
        c-indent-level 4
        c-basic-offset 4))

(use-package google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style-with-4-indent)

;; copy from https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      ;; treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

;; (use-package swiper
;;   :ensure t
;;   :config
;;   (defun isearch-forward-or-swiper (use-swiper)
;;     (interactive "p")
;;     (let (current-prefix-arg)
;;       (call-interactively (if use-swiper 'swiper 'isearch-forward))))
;;   (global-set-key (kbd "C-s") 'isearch-forward-or-swiper)
;;   )

;;以前是ivy用户，现在则是仅使用vertico, embark和consult了
;;(use-package ivy
;;  :ensure t
;;  :diminish ivy-mode
;;  :hook (after-init . ivy-mode))

(use-package ag
  :ensure t
  :ensure-system-package (ag . "brew install ag")
  )

;; (use-package helm
;;   :bind (("M-x" . helm-M-x)
;;          ;; ("C-x b" . helm-mini)
;;          ("C-x C-f" . helm-find-files)
;;          ("C-c y"   . helm-show-kill-ring)
;;          ("C-c m"   . helm-man-woman)
;;          ("C-c o"   . helm-occur)
;;          :map helm-map
;;          ("C-h" . delete-backward-char)
;;          :map helm-find-files-map
;;          ("C-h" . delete-backward-char))
;;   :init
;;   (custom-set-faces
;;    '(helm-header           ((t (:background "#3a3a3a" :underline nil))))
;;    '(helm-source-header    ((t (:background "gray16" :foreground "gray64" :slant italic))))
;;    '(helm-candidate-number ((t (:foreground "#00afff"))))
;;    '(helm-selection        ((t (:background "#005f87" :weight normal))))
;;    '(helm-match            ((t (:foreground "darkolivegreen3")))))
;;   :config
;;   (helm-mode 1))
;; copy from https://sachachua.com/dotemacs/index.html
;; (use-package helm
;;   :diminish helm-mode
;;   :if my-laptop-p
;;   :config
;;   (progn
;;     (require 'helm-for-files)
;;     (setq helm-candidate-number-limit 100)
;;     (setq helm-completing-read-handlers-alist
;;           '((describe-function)
;;             (consult-bookmark)
;;             (org-refile-get-location)
;;             (consult-outline)
;;             (consult-line)
;;             (org-olpath-completing-read)
;;             (consult-mark)
;;             (org-refile)
;;             (consult-multi-occur)
;;             (describe-variable)
;;             (execute-extended-command)
;;             (consult-yank)))
;;     ;; From https://gist.github.com/antifuchs/9238468
;;     (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
;;           helm-input-idle-delay 0.01  ; this actually updates things
;;                                         ; reeeelatively quickly.
;;           helm-yas-display-key-on-candidate t
;;           helm-quick-update t
;;           helm-M-x-requires-pattern nil
;;           helm-ff-skip-boring-files t))
;;   (defadvice helm-files-insert-as-org-links (around sacha activate)
;;     (insert (mapconcat (lambda (candidate)
;;                          (org-link-make-string candidate))
;;                        (helm-marked-candidates)
;;                        "\n")))
;;   :bind (("C-c h" . helm-mini)
;;          ("C-h a" . helm-apropos)
;;          ("C-x C-b" . helm-buffers-list)
;;          ("C-x c o" . helm-occur)
;;          ("C-x c s" . helm-swoop)
;;          ("C-x c y" . helm-yas-complete)
;;          ("C-x c Y" . helm-yas-create-snippet-on-region)
;;          ("C-x c SPC" . helm-all-mark-rings)))

;; (use-package helm-projectile
;;   :diminish projectile-mode
;;   :bind ("C-c p p" . helm-projectile-switch-project)
;;   :init
;;   (use-package helm-ag)
;;   :config
;;   (projectile-global-mode t)
;;   (helm-projectile-on))

;;(use-package lsp-ivy
;;  :ensure t
;; :after (lsp-mode))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; https://karthinks.com/software/jumping-directories-in-eshell/
;; (defun eshell/z (&optional regexp)
;;   "Navigate to a previously visited directory in eshell, or to
;; any directory proferred by `consult-dir'."
;;   (let ((eshell-dirs (delete-dups
;;                       (mapcar 'abbreviate-file-name
;;                               (ring-elements eshell-last-dir-ring)))))
;;     (cond
;;      ((and (not regexp) (featurep 'consult-dir))
;;       (let* ((consult-dir--source-eshell `(:name "Eshell"
;;                                                  :narrow ?e
;;                                                  :category file
;;                                                  :face consult-file
;;                                                  :items ,eshell-dirs))
;;              (consult-dir-sources (cons consult-dir--source-eshell
;;                                         consult-dir-sources)))
;;         (eshell/cd (substring-no-properties
;;                     (consult-dir--pick "Switch directory: ")))))
;;      (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
;;                      (completing-read "cd: " eshell-dirs)))))))

;; environment
;; (defconst *is-windows* (eq system-type 'windows-nt))
;; (defconst *is-unix* (not *is-windows*))

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

;; (use-package parinfer-rust-mode
;;   :ensure t
;;   :hook
;;   emacs-lisp-mode
;;   lisp-mode
;;   clojure-mode
;;   :config
;;   (setq parinfer-rust-library "~/.emacs.d/var/parinfer-rust/parinfer-rust-library.so")
;;   :custom
;;   (parinfer-rust-auto-download t))

;; keyfreq to analyze the key using situation
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands '(self-insert-command
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



;; (use-package popwin
;;   :config
;;   (global-set-key (kbd "s-j") popwin:keymap)
;;   (push '(compilation-mode :noselect t :position bottom :height 22) popwin:special-display-config)
;;   (push '("*rspec-compilation*" :noselect t :position bottom :height 22) popwin:special-display-config)
;;   (push '("*Go Test*" :noselect t :position bottom :height 22) popwin:special-display-config)
;;   (push '("*vterm" :regexp t :stick t :position bottom :height 24) popwin:special-display-config)
;;   (popwin-mode))

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

;; julia
(use-package julia-mode)
(use-package julia-repl)


;; symbol-overlay
;; homepage: https://github.com/wolray/symbol-overlay
;;
;; 作者的知乎，新插件推荐，高亮symbol同时支持一键跳转 https://zhuanlan.zhihu.com/p/26471685
;; 同时高亮多个symbol https://emacs-china.org/t/package-symbol-overlay-symbol/7706
;;
;; 老王的使用中提到了 https://manateelazycat.github.io/emacs/2022/11/07/how-i-use-emacs.html
;; 用 Emacs 的都少不了 isearch, 但是 isearch 不方便的地方是每次都要手动输入或者 yank 当前 symbol 给 isearch， 同时要批量替换的按键流程也很繁琐。 在使用 symbol-overlay 之前我一直用我自己开发的 lazy-search, 这两个项目的目标都是启动后立即选中光标处的 symbol, 再按单按键比如按 n/p 后， 快速跳转上一个和下一个匹配项， 节省了大量选中当前 symbol 启动 isearch 再粘贴 symbol 的操作时间。 用了 symbol-overlay 后， 发现比我的 lazy-search 实现的更加简洁和强大， 包括搜索后快速按 r 键可以对所有匹配的 symbol 进行快速重命名操作， symbol-overlay 基本上是单文件重构场景下最好用的插件， 强烈推荐大家使用。
;; 被embark替代
;; (use-package symbol-overlay
;;   :defer 2
;;   :config
;;   (setq symbol-overlay-idle-time 0.1)
;;   (global-set-key (kbd "M-i") 'symbol-overlay-put)
;;   (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
;;   (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
;;   (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
;;   (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)
;;   )

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
  :bind (:map wgrep-mode-map
		      ;; Added keybinding to echo Magit behavior
		      ("C-c C-c" . save-buffer)
		      :map grep-mode-map
		      ("e" . wgrep-change-to-wgrep-mode)
		      :map ripgrep-search-mode-map
		      ("e" . wgrep-change-to-wgrep-mode)))

;; (use-package corfu
;;   :demand t
;;   ;; Optionally use TAB for cycling, default is `corfu-complete'.
;;   :bind (:map corfu-map
;; 		      ("<escape>". corfu-quit)
;; 		      ("<return>" . corfu-insert)
;; 		      ("M-d" . corfu-show-documentation)
;; 		      ("M-l" . 'corfu-show-location)
;; 		      ("TAB" . corfu-next)
;; 		      ([tab] . corfu-next)
;; 		      ("S-TAB" . corfu-previous)
;; 		      ([backtab] . corfu-previous))

;;   :custom
;;   ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
;;   ;; want to perform completion
;;   (tab-always-indent 'complete)
;;   (completion-cycle-threshold nil)      ; Always show candidates in menu

;;   (corfu-auto nil)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.25)

;;   ;; (corfu-min-width 80)
;;   ;; (corfu-max-width corfu-min-width)     ; Always have the same width
;;   (corfu-count 14)
;;   (corfu-scroll-margin 4)
;;   (corfu-cycle nil)

;;   ;; (corfu-echo-documentation nil)        ; Already use corfu-doc
;;   (corfu-separator ?\s)                 ; Necessary for use with orderless
;;   (corfu-quit-no-match 'separator)

;;   (corfu-preview-current 'insert)       ; Preview current candidate?
;;   (corfu-preselect-first t)             ; Preselect first candidate?

;;   :init
;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since dabbrev can be used globally (M-/).
;;   (global-corfu-mode))

;; (defun corfu-move-to-minibuffer ()
;;   "Move \"popup\" completion candidates to minibuffer.

;;   Useful if you want a more robust view into the recommend candidates."
;;   (interactive)
;;   (let (completion-cycle-threshold completion-cycling)
;;     (apply #'consult-completion-in-region completion-in-region--data)))
;; (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

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
;; (use-package kind-icon
;;   ;; :after corfu
;;   :custom
;;   (kind-icon-use-icons t)
;;   ;; (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
;;   (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
;;   (kind-icon-blend-frac 0.08)

;;   ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
;;   ;; directory that defaults to the `user-emacs-directory'. Here, I change that
;;   ;; directory to a location appropriate to `no-littering' conventions, a
;;   ;; package which moves directories of other packages to sane locations.
;;   ;; (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

;;   ;; Add hook to reset cache so the icon colors match my theme
;;   ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
;;   ;; the theme using my custom defined command for switching themes. If I don't
;;   ;; do this, then the backgound color will remain the same, meaning it will not
;;   ;; match the background color corresponding to the current theme. Important
;;   ;; since I have a light theme and dark theme I switch between. This has no
;;   ;; function unless you use something similar
;;   (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

;; (use-package corfu-doc
;;   ;; NOTE 2022-02-05: At the time of writing, `corfu-doc' is not yet on melpa
;;   :bind (:map corfu-map
;; 	      ;; This is a manual toggle for the documentation window.
;; 	      ([remap corfu-show-documentation] . corfu-doc-toggle) ; Remap the default doc command
;; 	      ;; Scroll in the documentation window
;; 	      ("M-n" . corfu-doc-scroll-up)
;; 	      ("M-p" . corfu-doc-scroll-down))
;;   :hook (corfu-mode . corfu-doc-mode)
;;   :custom
;;   (corfu-doc-delay 0.1)
;;   (corfu-doc-hide-threshold 10)
;;   (corfu-doc-max-width 60)
;;   (corfu-doc-max-height 10)

;;   ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
;;   ;; extra-safe that this is set when corfu-doc is loaded. I do not want
;;   ;; documentation shown in both the echo area and in the `corfu-doc' popup.
;;   ;; (corfu-echo-documentation nil)
;;   :config
;;   ;; NOTE 2022-02-05: This is optional. Enabling the mode means that every corfu
;;   ;; popup will have corfu-doc already enabled. This isn't desirable for me
;;   ;; since (i) most of the time I do not need to see the documentation and (ii)
;;   ;; when scrolling through many candidates, corfu-doc makes the corfu popup
;;   ;; considerably laggy when there are many candidates. Instead, I rely on
;;   ;; manual toggling via `corfu-doc-toggle'.
;;   (corfu-doc-mode))

;; (use-package cape
;;   :bind (("C-c p p" . completion-at-point)
;; 	 ("C-c p d" . cape-dabbrev)
;; 	 ("C-c p f" . cape-file)
;; 	 ("C-c p s" . cape-symbol)
;; 	 ("C-c p i" . cape-ispell)))

;; Use Company backends as Capfs.
;; (setq-local completion-at-point-functions
;;             (mapcar #'cape-company-to-capf
;;                     (list #'company-files #'company-ispell #'company-dabbrev)))

;; (use-package link-hint
;;   :bind
;;   ("C-c l o" . link-hint-open-link)
;;   ("C-c l c" . link-hint-copy-link))

;; (use-package lin
;;   :config (lin-global-mode 1)
;;   (setq lin-face 'lin-blue))

(use-package pulsar
  :custom
  (pulsar-pulse-functions ; Read the doc string for why not `setq'
   '(recenter-top-bottom
     move-to-window-line-top-bottom
     reposition-window
     bookmark-jump
     other-window
     delete-window
     delete-other-windows
     forward-page
     backward-page
     scroll-up-command
     scroll-down-command
     windmove-right
     windmove-left
     windmove-up
     windmove-down
     windmove-swap-states-right
     windmove-swap-states-left
     windmove-swap-states-up
     windmove-swap-states-down
     tab-new
     tab-close
     tab-next
     org-next-visible-heading
     org-previous-visible-heading
     org-forward-heading-same-level
     org-backward-heading-same-level
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     ace-window
     outline-up-heading))
  :hook
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  ;; integration with the built-in `imenu':
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry)
  :config
  (pulsar-global-mode 1)
  (setq pulsar-face 'pulsar-magenta
	    pulsar-delay 0.05)
  (defun jf/pulse (parg)
    "Pulse the current line.

  If PARG (given as universal prefix), pulse between `point' and `mark'."
    (interactive "P")
    (if (car parg)
	    (pulsar--pulse nil nil (point) (mark))
	  (pulsar-pulse-line)))
  :bind (("C-x l" . jf/pulse)))

(use-package goto-line-preview
  :bind (("M-g g" . goto-line-preview)
         ("M-g M-g" . goto-line-preview)))

;;  Sometimes it's useful to step to the last changes in a buffer.
(use-package goto-last-change
  :bind (("C-x ;" . goto-last-change)))

;; copy from https://qiita.com/tadsan/items/a76768439869f00a4e89
;; (use-package php-mode
;;   :hook ((php-mode . my-php-mode-setup))
;;   :init
;;   (require 'php-mode)
;;   :custom
;;   ;; (php-manual-url 'ja)
;;   (php-mode-coding-style 'psr2)
;;   (php-mode-template-compatibility nil)
;;   :config
;;   (defun my-php-mode-setup ()
;;     "My PHP-mode hook."
;;     (subword-mode 1)
;;     (setq show-trailing-whitespace t)

;;     (setq-local page-delimiter "\\_<\\(class\\|function\\|namespace\\)\\_>.+$")

;;     (require 'flycheck-phpstan)
;;     (flycheck-mode t)
;;     (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
;;     (add-to-list 'flycheck-disabled-checkers 'php-phpcs))
;;   ;; (bind-key "[" (smartchr "[]" "array()" "[[]]") php-mode-map)
;;   ;; (bind-key "]" (smartchr "array " "]" "]]")     php-mode-map)
;;   ;; (bind-key "C-}" 'cedit-barf php-mode-map)
;;   ;; (bind-key "C-)" 'cedit-slurp php-mode-map)
;;   (bind-key "C-c C-c" 'psysh-eval-region         php-mode-map)
;;   (bind-key "<f6>" 'phpunit-current-project      php-mode-map)
;;   (bind-key "C-c C--" 'php-current-class php-mode-map)
;;   (bind-key "C-c C-=" 'php-current-namespace php-mode-map))
(use-package php-mode
  ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
  :mode
  ;; (("[^.][^t][^p][^l]\\.php$" . php-mode))
  (("\\.php\\'" . php-mode)
   ("\\.inc\\'" . php-mode)
   ("\\.module\\'" . php-mode))
  :custom
  (indent-tabs-mode nil)
  (tab-width 2)
  (c-basic-offset 2)
  :hook (php-mode . flycheck)
  :config
  (add-hook 'php-mode-hook
	        #'(lambda ()
	       ;;; PHP-mode settings:
                (setq indent-tabs-mode nil
		              c-basic-offset 4
                      php-template-compatibility nil)

                (php-enable-psr2-coding-style)

	       ;;; PHP_CodeSniffer settings:
                ;; (use-package phpcbf
	            ;; :init
	            ;; (setq phpcbf-executable "~/.composer/vendor/squizlabs/php_codesniffer/scripts/phpcbf"
	            ;; phpcbf-standard "PSR2"))

	       ;;; Company-mode settings:
	            ;; Using :with and company-sort-by-backend-importance makes
	            ;; it so that company-lsp entries will always appear before
	            ;; company-dabbrev-code.
                ;; TODO Add in support for company-gtags/capf
	            (use-package company-php)
	            (ac-php-core-eldoc-setup)
                (setq-local company-dabbrev-char-regexp "\\\`$sw")
                (setq-local company-dabbrev-code-everywhere t)
                ;; (setq-local company-transformers '(company-sort-by-backend-importance))
	            (set (make-local-variable 'company-backends)
		             ;;'((company-ac-php-backend company-dabbrev-code)))
		             ;;'((company-ac-php-backend company-dabbrev-code :separate)))
		             '((company-dabbrev-code company-ac-php-backend)))
	            ;;'((company-ac-php-backend :with company-dabbrev-code)))
                ;; '((company-lsp :with company-dabbrev-code)))

	       ;;; LSP (Language Server Protocol) Settings:
                ;; (add-to-list 'load-path "~/.emacs.d/lsp-php")
                ;; (require 'lsp-php)
	            ;; (custom-set-variables
	            ;; Composer.json detection after Projectile.
	            ;; 	'(lsp-php-workspace-root-detectors (quote (lsp-php-root-projectile lsp-php-root-composer-json lsp-php-root-vcs)))
	            ;; )
                ;; (lsp-php-enable)

	       ;;; Flycheck Settings:
	            (defvar-local flycheck-checker 'php-phpcs)
                (setq-local flycheck-check-syntax-automatically '(save))

	       ;;; Key Bindings:
	            ;; (dumb-jump-mode)
	            ;; (ggtags-mode 1)
	            ;; [J]ump to a function definition (at point)
                (local-set-key (kbd "C-c j") 'ac-php-find-symbol-at-point)
	            ;; (local-set-key (kbd "C-c j") 'dumb-jump-go)
	            ;; (local-set-key (kbd "C-c j") 'ggtags-find-definition)

	            ;; Find [r]eferences (at point)
	            ;; (local-set-key (kbd "C-c r") 'ggtags-find-reference)

                ;; Go [b]ack, after jumping
	            ;; (local-set-key (kbd "C-c b") 'dumb-jump-back)
                (local-set-key (kbd "C-c b") 'ac-php-location-stack-back)
	            ;; (local-set-key (kbd "C-c b") 'ggtags-prev-mark)

                ;; Go [f]orward
                (local-set-key (kbd "C-c f") 'ac-php-location-stack-forward)
	            ;; (local-set-key (kbd "C-c f") 'ggtags-next-mark)

                ;; [S]how a function definition (at point)
                (local-set-key (kbd "C-c s") 'ac-php-show-tip)
	            ;; (local-set-key (kbd "C-c q") 'dumb-jump-quick-look)

                ;; Re[m]ake the tags (after a source has changed)
                (local-set-key (kbd "C-c m") 'ac-php-remake-tags)

                ;; Show [p]roject info
                (local-set-key (kbd "C-c p") 'ac-php-show-cur-project-info)

	            ;; Bring up [i]menu
	            (local-set-key (kbd "C-c i") 'helm-imenu)))
  )


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

(use-package flycheck-phpstan
  :ensure t
  :hook ((php-mode) . flycheck-mode)
  :commands (flycheck-mode)
  :config
  (setq-default phpstan-executable 'docker)
  )

;; (use-package explain-pause-mode
;;   ;; :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
;;   :ensure t
;;   :config
;;   (explain-pause-mode))

;; (use-package pyim
;;   :demand t
;;   :bind (:map pyim-mode-map
;;               ("," . pyim-previous-page)
;;               ("." . pyim-next-page))
;;   :config
;;   (setq pyim-page-tooltip 'minibuffer
;;         pyim-default-scheme 'quanpin
;;         pyim-page-style 'two-lines
;;         pyim-page-length 8
;;         pyim-cloudim 'google)
;;   (global-set-key (kbd "C-\\") 'toggle-input-method)
;;   ;; vertico search pinyin
;;   (defun pyim-orderless-regexp (orig-func component)
;;     (let ((result (funcall orig-func component)))
;;       (pyim-cregexp-build result)))
;;   (advice-add 'orderless-regexp :around #'pyim-orderless-regexp))

;; (use-package pyim-basedict
;;   :after pyim
;;   :config
;;   (pyim-basedict-enable))
;; 中文输入法
;; (use-package rime
;;   :custom (default-input-method "rime")
;;   :config
;;   (setq rime-show-candidate 'posframe)
;;   (setq rime-disable-predicates '(rime-predicate-after-alphabet-char-p
;; 				                  rime-predicate-ace-window-p
;; 				                  rime-predicate-hydra-p
;; 				                  rime-predicate-in-code-string-p
;; 				                  rime-predicate-tex-math-or-command-p)))

(use-package edebug-x
  :ensure t
  :defer t
  :commands (edebug-x-mode)
  ;; :init (setq edebug-x-stop-point-overlay t)
  :config
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
  :ensure nil
  :defer t
  :diminish syntax-subword-mode
  :init
  (add-hook 'prog-mode-hook 'syntax-subword-mode)
  (add-hook 'org-mode-hook 'syntax-subword-mode)
  (add-hook 'text-mode-hook 'syntax-subword-mode)
  )

(use-package mmm-mode
  :ensure t
  :hook ((mmm-mode . company-mode)
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
  :defer t
  :init (setq gdb-many-windows t
              gdb-show-main t))

;; expand-region

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
;; Dash is a documentation manager for Mac OS that provides a single place to quickly search over
;; 200 documentation sets
;; (use-package dash-docs
;;   :config
;;   (setq
;;    dash-docs-enable-debugging init-file-debug
;;    dash-docs-min-length 2
;;    dash-docs-browser-func #'eww)
;;   (setq dash-docs-docsets-path (no-littering-expand-var-file-name "docsets"))
;;   (setq installed-langs (dash-docs-installed-docsets))
;;   ;;figure out to convert spaces into underscores when installing the docs
;;   (setq docset-langs '("Rust" "Emacs_Lisp" "JavaScript" "C" "Bash" "Vim" "SQLite" "PostgreSQL" "LaTeX" "Docker" "C++" "HTML" "SVG" "CSS"))
;;   (dolist (lang docset-langs)
;; 	  (when (null (member lang installed-langs))
;; 	    (dash-docs-install-docset lang))))


;; (use-package centered-cursor-mode
;;   :demand
;;   :config
;;   ;; Optional, enables centered-cursor-mode in all buffers.
;;   (global-centered-cursor-mode))

;; transpose spit windows
(use-package transpose-frame
  :bind ("C-x %". #'transpose-frame))

(use-package syntax-subword
  :ensure t
  :init
  (global-syntax-subword-mode)
  (setq syntax-subword-skip-spaces nil))

;; 上下移动行/块
(use-package drag-stuff
  :bind (("<M-up>". drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

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
(use-package avy
  :bind (("C-'" . avy-goto-char-timer) ;; Control + 单引号
         ;; 复用上一次搜索
         ("C-c C-j" . avy-resume))
  :config
  (setq avy-background t ;; 打关键字时给匹配结果加一个灰背景，更醒目
        avy-all-windows t ;; 搜索所有 window，即所有「可视范围」
        avy-timeout-seconds 0.3)) ;; 「关键字输入完毕」信号的触发时间

;; buffer 内正则替换
;; 渐进式可视化
(use-package anzu
  :ensure t)
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

;; (use-package winum
;;   :ensure t
;;   :config
;;   (setq winum-auto-setup-mode-line nil)
;;   (define-key winum-keymap (kbd "C-`") 'winum-select-window-by-number)
;;   (define-key winum-keymap (kbd "C-²") 'winum-select-window-by-number)
;;   (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
;;   (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
;;   (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
;;   (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
;;   (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
;;   (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
;;   (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
;;   (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
;;   (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
;;   (winum-mode))

;; hydra 更接近于「功能菜单」：弹出一个「常用功能列表」.
;; 你可以用连续击键来连续触发若干个函数。
(use-package hydra
  :bind (("C-c m" . hydra-magit/body)
         ("C-c o" . hydra-org/body)
         ))

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

(use-package explain-pause-mode
  :ensure t
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  :defer 2
  :config
  (setq explain-pause-top-auto-refresh-interval 0.1)
  (explain-pause-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ace-window
  :ensure t
  :defer t
  :delight ace-window-mode
  :bind ("C-x x j" . ace-window)
  :init (setq aw-background nil)
  :custom
  (aw-scope 'frame)
  (aw-ignore-on t)
  (aw-dispatch-when-more-than 3)
  (aw-keys '(?h ?j ?k ?l ?u ?i ?o ?p))
  (aw-ignored-buffers '("*NeoTree*"))
  )

;; kubernetes
(use-package kele
  :ensure t
  ;; :straight t
  :config
  (kele-mode 1))

;; paredit-mode 启用自动补全括号
;; 再一次paredit-mode 禁用自动补全括号
(use-package paredit
  :ensure t
  :config
  (define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-[") 'paredit-backward-slurp-sexp)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'nrepl-mode-hook 'paredit-mode)
  )

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

(provide 'init-config-packages)
;;;; init-config-packages ends here
