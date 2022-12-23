;; copy from https://www.reddit.com/r/emacs/comments/iu0euj/getting_modern_multiple_cursors_in_emacs/
(use-package multiple-cursors
  :ensure   t
  :bind (("H-SPC" . set-rectangular-region-anchor)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)
         ))

(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))

;; copy from [Error when running magit-status: run-hooks: Wrong number of arguments](https://github.com/magit/magit/issues/3837)
(use-package transient
  :init
  (setq transient-history nil))

(use-package display-line-numbers
  :defer
  :config
  ;; Set absolute line numbers.  A value of "relative" is also useful.
  (setq display-line-numbers-type t)
  (define-minor-mode prot/display-line-numbers-mode
    "Toggle `display-line-numbers-mode' and `hl-line-mode'."
    :init-value nil
    :global nil
    (if prot/display-line-numbers-mode
        (progn
          (display-line-numbers-mode 1)
          (hl-line-mode 1))
      (display-line-numbers-mode -1)
      (hl-line-mode -1)))
  :bind ("<f7>" . prot/display-line-numbers-mode))


(use-package ispell
  :config
  (setq-default ispell-program-name "aspell")
  (ispell-change-dictionary "american" t))

;; https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/
(use-package olivetti
  :ensure
  :defer
  :diminish
  :config
  (setq olivetti-body-width 0.65)
  (setq olivetti-minimum-body-width 72)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  (define-minor-mode prot/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters.
Fringes are disabled.  The modeline is hidden, except for
`prog-mode' buffers (see `prot/hidden-mode-line-mode').  The
default typeface is set to a proportionately-spaced family,
except for programming modes (see `prot/variable-pitch-mode').
The cursor becomes a blinking bar, per `prot/cursor-type-mode'."
    :init-value nil
    :global nil
    (if prot/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          (prot/variable-pitch-mode 1)
          (prot/cursor-type-mode 1)
          (unless (derived-mode-p 'prog-mode)
            (prot/hidden-mode-line-mode 1)))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (prot/variable-pitch-mode -1)
      (prot/cursor-type-mode -1)
      (unless (derived-mode-p 'prog-mode)
        (prot/hidden-mode-line-mode -1))))
  :bind ("C-c o" . prot/olivetti-mode))

;; copy from [Saving persistent undo to a single directory, alist format](https://emacs.stackexchange.com/questions/26993/saving-persistent-undo-to-a-single-directory-alist-format)
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-visualizer-timestamps t))

;; copy from [Playing nicely with linum](https://www.emacswiki.org/emacs/UndoTree)
(defun undo-tree-visualizer-update-linum (&rest args)
  (linum-update undo-tree-visualizer-parent-buffer))
(advice-add 'undo-tree-visualize-undo :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-redo :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-undo-to-x :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-redo-to-x :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualizer-mouse-set :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualizer-set :after #'undo-tree-visualizer-update-linum)

;; copy from https://emacs-china.org/t/vterm-zsh/20497
;;; Terminal
(use-package vterm
  :when (memq window-system '(mac ns x pgtk))
  :bind (:map vterm-mode-map
              ("C-y" . vterm-yank)
              ("M-y" . vterm-yank-pop)
              ("C-k" . vterm-send-C-k-and-kill))
  :init
  (setq vterm-shell "zsh")
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 99999)
  (setq vterm-always-compile-module t)
  (defun vterm-send-C-k-and-kill ()
    "Send `C-k' to libvterm, and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (vterm-end-of-line))
    (vterm-send-key "k" nil nil t)))

(use-package vterm-toggle
  :when (memq window-system '(mac ns x pgtk))
  :bind (([f8] . vterm-toggle)
         ([f9] . vterm-compile)
         :map vterm-mode-map
         ([f8] . vterm-toggle)
         ([(control return)] . vterm-toggle-insert-cd))
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
  :commands prot/hidden-mode-line-mode
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  :config
  ;; 默认情况下，Emacs 为每个打开的文件创建一些临时的文件，这会搞乱我们的目录，不需要它。
  ;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
  ;; as a buffer is unsaved, backups create copies once, when the file is first
  ;; written, and never again until it is killed and reopened. This is better
  ;; suited to version control, and I don't want world-readable copies of
  ;; potentially sensitive material floating around our filesystem.
  (setq create-lockfiles nil
        make-backup-files nil
        ;; But in case the user does enable it, some sensible defaults:
        version-control t     ; number each backup file
        backup-by-copying t   ; instead of renaming current file (clobbers links)
        delete-old-versions t ; clean up after itself
        kept-old-versions 5
        kept-new-versions 5)
  ;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
  ;; Use `recover-file' or `recover-session' to recover them.
  (setq auto-save-default t
        ;; Don't auto-disable auto-save after deleting big chunks. This defeats
        ;; the purpose of a failsafe. This adds the risk of losing the data we
        ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
        auto-save-include-big-deletions t
        auto-save-file-name-transforms
        (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                    ;; Prefix tramp autosaves to prevent conflicts with local ones
                    (concat auto-save-list-file-prefix "tramp-\\2") t)
              (list ".*" auto-save-list-file-prefix t)))
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

  (define-minor-mode prot/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if prot/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))
  (define-minor-mode prot/hidden-mode-line-mode
    "Toggle modeline visibility in the current buffer."
    :init-value nil
    :global nil
    (if prot/hidden-mode-line-mode
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format)
      (force-mode-line-update)))
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


  ;; C-c l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.
  :bind ("C-c L" . prot/scroll-centre-cursor-mode)
  )

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)


;; copy from https://www.danielde.dev/blog/emacs-for-swift-development
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
(use-package swift-mode
  :bind (("C-c l" . print-swift-var-under-point)))

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
  )

;;  rainbow-delimiters 可以将对称的括号用同一种颜色标记出来。
;; parens
(use-package smartparens
  :diminish nil
  :config
  (sp-use-smartparens-bindings))
(use-package smartparens-config
  :diminish nil
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(use-package paren
  :config
  (setq show-paren-delay 0.1
        show-paren-when-point-in-periphery t))
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

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
(use-package ef-themes
  :defer t
  :init (load-theme 'ef-winter t))

;; 让 .emacs.d 更干净
;; no littering, keep .emacs.d clean
(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (set 'recentf-exclude
         '(no-littering-var-directory
           no-littering-etc-directory
           (expand-file-name "elpa" user-emacs-directory)
           (expand-file-name "cache" user-emacs-directory))))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))


(use-package indent-guide
  :config
  (indent-guide-global-mode)
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
  (org-babel-tmux-terminal "xterm")
  (org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e"))
  ;; Finally, if your tmux is not in your $PATH for whatever reason, you
  ;; may set the path to the tmux binary as follows:
  (org-babel-tmux-location "/usr/bin/tmux"))

;; copy from https://emacs.stackexchange.com/questions/31872/how-to-update-packages-installed-with-use-package
;; With that setup, packages will be updated every 4 days, and the old packages will be removed.
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

(provide 'init-config-packages)
