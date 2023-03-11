;;; init-vterm.el --- Summary vterm file -*- lexical-binding: t -*-

;;; Commentary:
;; vterm

;;; Code:

;; copy from https://emacs-china.org/t/vterm-zsh/20497
;;; Terminal
(use-package vterm
  :when (memq window-system '(mac ns x pgtk))
  :bind (:map vterm-mode-map
              ("C-y" . vterm-yank)
              ("M-y" . vterm-yank-pop)
              ("C-k" . vterm-send-C-k-and-kill)
              ;; 回车启动和禁用vterm copy mode
              ;; ("<return>" . vterm-copy-mode)
              )
  :custom-face
  (vterm-color-black ((t (:foreground "#0f0f0f" :background "#707880"))))
  ;; copy from https://github.com/doomemacs/themes/blob/ae18b84e01496c4ebd572cad00a89516af089a94/doom-themes-base.el#L234
  ;; ;; vterm
  ;; (vterm               :foreground fg)
  ;; (vterm-color-black   :background base0   :foreground base0)
  ;; (vterm-color-red     :background red     :foreground red)
  ;; (vterm-color-green   :background green   :foreground green)
  ;; (vterm-color-yellow  :background yellow  :foreground yellow)
  ;; (vterm-color-blue    :background blue    :foreground blue)
  ;; (vterm-color-magenta :background magenta :foreground magenta)
  ;; (vterm-color-cyan    :background cyan    :foreground cyan)
  ;; (vterm-color-white   :background base8   :foreground base8)
  ;; copy from https://erickgnavar.github.io/emacs-config/
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
  (vterm-always-compile-module t)
  (vterm-buffer-name-string "*my-own-vterm*")
  (vterm-use-vterm-prompt-detection-method t)
  (vterm-max-scrollback 10000)
  (vterm-copy-exclude-prompt t)
  (vterm-shell "zsh")
  (vterm-kill-buffer-on-exit t)
  ;; 在 terminal 中使用 C-c C-t 进入「选择」模式（类似 Tmux 里的 C-b [ ）
  (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  :config
  ;; TODO disable c-] shortcut
  ;; shell 退出时 kill 掉这个 buffer
  ;; (setq vterm-kill-buffer-on-exit t)
  ;; 使用 M-x vterm 新建一个 terminal
  ;; 在 terminal 中使用 C-c C-t 进入「选择」模式（类似 Tmux 里的 C-b [ ）
  ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  ;; (setq vterm-max-scrollback 99999)
  ;; (setq vterm-always-compile-module t)
  (defun vterm-send-C-k-and-kill ()
    "Send `C-k' to libvterm, and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (vterm-end-of-line))
    (vterm-send-key "k" nil nil t))
  (defun turn-off-chrome ()
    ;; in vterm mode, C-h m , check what mode is on
    ;; Enabled minor modes: All-The-Icons-Completion Anzu Async-Bytecomp-Package
    ;; Auto-Compile-On-Load Auto-Compile-On-Save Auto-Composition Auto-Compression
    ;; Auto-Dim-Other-Buffers Auto-Encryption Beacon Blink-Cursor Bufler-Workspace
    ;; Centaur-Tabs Column-Number Company Company-Box Delete-Selection Desktop-Save
    ;; Diff-Hl-Flydiff Dired-Async Diredfl-Global Dirvish-Override-Dired Display-Time
    ;; Doom-Modeline Electric-Indent Explain-Pause Eyebrowse File-Name-Shadow Font-Lock
    ;; Gcmh Global-Aggressive-Indent Global-Anzu Global-Auto-Revert Global-Company
    ;; Global-Diff-Hl Global-Display-Line-Numbers Global-Eldoc Global-Emojify
    ;; Global-Flycheck Global-Font-Lock Global-Highlight-Thing Global-Lentic
    ;; Global-Pangu-Spacing Global-So-Long Global-Superword Global-Syntax-Subword
    ;; Global-Whitespace-Cleanup Google-This Highlight-Thing Hydra-Posframe Kele
    ;; Keycast-Mode-Line Keyfreq Keyfreq-Autosave Lentic Line-Number Marginalia
    ;; Menu-Bar Mouse-Wheel Nameframe-Projectile Nyan Org-Pretty-Tags-Global
    ;; Override-Global Pangu-Spacing Pdf-Occur-Global Projectile Pulsar Pulsar-Global
    ;; Recentf Recursion-Indicator Reverse-Im Save-Place Savehist Shell-Dirtrack
    ;; Show-Paren Size-Indication Straight-Package-Neutering Straight-Use-Package
    ;; Super-Save Superword Syntax-Subword Transient-Mark Treemacs-Filewatch
    ;; Treemacs-Follow Treemacs-Fringe-Indicator Treemacs-Git Ue-Global Vertico
    ;; Vertico-Buffer Vertico-Multiform Which-Function Which-Key-Posframe
    ;; Whitespace-Cleanup Winner Zoom
    ;; vterm 模式下禁用hl-line-mode
    (hl-line-mode -1)
    ;; vterm 模式下禁用display-line-numbers-mode
    (display-line-numbers-mode -1)
    ;; vterm 模式下禁用emojify-mode
    (emojify-mode -1)
    ;; vterm 模式下禁用flycheck
    (flycheck-mode -1)
    ;; vterm 模式下禁用which-key
    (which-key-mode -1)
    (super-save-mode -1)
    (all-the-icons-completion-mode -1)
    ;; (so-long-mode -1)
    (which-function-mode -1)
    ;; (which-key-posframe-mode -1)
    (aggressive-indent-mode -1)
    (recentf-mode -1)
    ;; (kele-mode -1)
    (setq-local global-hl-line-mode nil)
    (turn-off-smartparens-strict-mode)
    (turn-off-smartparens-mode)
    (load-theme 'moe-dark t)
    (corfu-echo-mode -1)
    (corfu-history-mode -1)
    (corfu-popupinfo-mode -1)
    )
  :hook
  (vterm-mode . turn-off-chrome)
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

(provide 'init-vterm)
;;; init-vterm.el ends here
