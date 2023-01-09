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

(use-package vundo
  :bind ("C-x u" . vundo)
  :commands (vundo)
  :hook ((vundo-mode . my/vundo-setup))
  :init
  (progn
    (setq vundo-window-max-height 5))
  :config
  (progn
    (setq vundo-glyph-alist vundo-unicode-symbols)
    ;; Take less on-screen space.
    (setq vundo-compact-display t)
    ;; Better contrasting highlight.
    (custom-set-faces
     '(vundo-node ((t (:foreground "#808080"))))
     '(vundo-stem ((t (:foreground "#808080"))))
     '(vundo-highlight ((t (:foreground "#FFFF00")))))

    (defun my/vundo-setup ()
      "Remove mode-line and header-line."
      (setq mode-line-format nil)
      (setq header-line-format nil))
    ))

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
    (vterm-send-key "k" nil nil t))
  ;; copy from https://erickgnavar.github.io/emacs-config/
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
  (vterm-always-compile-module t)
  )

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
  :ensure t
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


;; copy from https://www.patrickdelliott.com/emacs.d/
(use-package emacs
  :init
  (setq user-full-name "Patrick D. Elliott") ;; my details
  (setq user-mail-address "patrick.d.elliott@gmail.com")

  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short

  (setq indent-tabs-mode nil) ;; no tabs

  (setq make-backup-files nil) ;; keep everything under vc
  (setq auto-save-default nil)

  ;; keep backup and save files in a dedicated directory
  (setq backup-directory-alist
        `((".*" . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups") t)))

  (setq create-lockfiles nil) ;; no need to create lockfiles

  (set-charset-priority 'unicode) ;; utf8 in every nook and cranny
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything

  ;; Don't persist a custom file
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings

  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  ;; FIXME currently using tempel in org-mode triggers this warning
  ;; (setq warning-suppress-types (append warning-suppress-types '((org-element-cache))))

  (show-paren-mode t)

  ;; Hide commands in M-x which don't work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p))


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
(use-package all-the-icons)

;; https://www.emacswiki.org/emacs/KeyCast
;; copy from https://book.emacs-china.org/#org737719a
;; ;;modeline上显示我的所有的按键和执行的命令
(use-package keycast
  :config
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  (keycast-mode))

;; 这里的执行顺序非常重要，doom-modeline-mode 的激活时机一定要在设置global-mode-string 之后‘
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 15))
  :config
  (setq doom-modeline-project-detection 'project))

;; M-x all-the-icons-install-fonts
;; copy from [Why do I have Chinese/Mandarin characters in my mode-line and e-shell out of the blue? How do I fix this?](https://emacs.stackexchange.com/questions/73397/why-do-i-have-chinese-mandarin-characters-in-my-mode-line-and-e-shell-out-of-the)

;; copy from https://quant67.com/post/emcas/init-config.html
;; 让 Emacs 识别文件在项目里
;;projectile 提供了这个功能。 C-c c-p 会列举它的快捷键，其中包括在项目中搜索，切换项目等。
;;(use-package projectile
;;  :diminish projectile-mode
;;  :init
;;  (setq projectile-keymap-prefix (kbd "C-c C-p"))
;;  :config
;;  (projectile-mode +1)
;;  (setq projectile-enable-caching t)
;;  (setq-default projectile-mode-line-prefix " Proj")
;;  (projectile-global-mode))


;; copy from https://quant67.com/post/emcas/init-config.html
;; F8 侧边打开项目目录
;;(use-package neotree
;;  :config
;;  ;; f8 to view tree strucure of folder
;;  (defun neotree-project-dir ()
;;    "Open NeoTree using the git root."
;;    (interactive)
;;    (let ((project-dir (projectile-project-root))
;;          (file-name (buffer-file-name)))
;;      (neotree-toggle)
;;      (if project-dir
;;          (if (neo-global--window-exists-p)
;;              (progn
;;                (neotree-dir project-dir)
;;                (neotree-find file-name)))
;;        (message "Could not find git project root."))))
;;  (global-set-key [f8] 'neotree-project-dir)
;;  ;; switch with projectile
;;  (use-package projectile)
;;  (setq projectile-switch-project-action 'neotree-projectile-action))

;; copy from [Highlight current active window](https://stackoverflow.com/questions/33195122/highlight-current-active-window)
(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))


(use-package all-the-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-subtree
  :ensure t
  :after dired
  ;;:config
  ;;(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
  )

;; Share clipoard with OS
(use-package pbcopy
  :ensure t)

;; XML formatter
(reformatter-define xml-format
  :program "xmlformat"
  :group 'xml)

(with-eval-after-load 'nxml-mode
  (define-key nxml-mode-map (kbd "C-c C-f") 'xml-format-buffer))


(reformatter-define dart-format
  :program "dart"
  :args '("format")
  :group 'dart)

(defun my/dart-run-file ()
  "Execute the code of the current file."
  (interactive)
  (compile (format "dart %s" (buffer-file-name))))

(use-package dart-mode
  :ensure t
  :if (or (executable-find "dart") (executable-find "flutter"))
  :bind (:map dart-mode-map
              ("C-c C-f" . dart-format-buffer)
              ("C-c C-c" . my/dart-run-file))
  :config
  (evil-leader/set-key-for-mode 'dart-mode "d" 'xref-find-definitions))

(defun my/flutter-goto-logs-buffer()
  "Go to buffer logs buffer."
  (interactive)
  (let ((buffer (get-buffer flutter-buffer-name)))
    (unless buffer
      (user-error "flutter is not running."))
    (switch-to-buffer buffer)
    (goto-line (point-max))))

(use-package flutter
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-c C-r" . #'flutter-run-or-hot-reload)
              ("C-c C-l" . #'my/flutter-goto-logs-buffer))
  :hook (dart-mode . flutter-test-mode)
  :custom
  ;; sdk path will be the parent-parent directory of flutter cli
  (flutter-sdk-path (directory-file-name
                     (file-name-directory
                      (directory-file-name
                       (file-name-directory (executable-find "flutter")))))))

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
(defun my/org-tree-slide-setup ()
  (org-display-inline-images)
  (hide-mode-line-mode 1))

(defun my/org-tree-slide-end ()
  (org-display-inline-images)
  (hide-mode-line-mode 0))

(use-package org-tree-slide
  :ensure t
  :defer t
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

(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))

(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-show-staged-changes nil)
  ;; for some reason the :hook form doesn't work so we have to use :init
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  :config
  (global-diff-hl-mode))

(use-package gist
  :ensure t
  :defer t)

(use-package linkode
  :ensure t
  :defer t)

(use-package git-timemachine)

;; Cursor
(use-package beacon
  :ensure t
  :custom
  (beacon-color "orange")
  :config
  (beacon-mode 1))

(use-package dash
  :defer t)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  ;;(lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

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
  :ensure nil
  :hook (after-init . global-hl-line-mode))

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
  :ensure nil
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
(use-package subword
  :hook (after-init . global-subword-mode))

;; 如果你想要一个足够简单的注释与反注释功能，那么自带的newcomment就可以做到。
;; 当用户选中区间时，在对应区间上注释或者反注释
;; 如果当前行是空的，那么会插入一个注释并且将它对齐 (偷懒，直接调用了comment-dwim)
;; 其他情况则对当前行注释或者反注释
(use-package newcomment
  :ensure nil
  :bind ([remap comment-dwim] . #'comment-or-uncomment)
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


;; RET 后仅保留一个 dired buffer
;; For Emacs 28
(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t))



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
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;; (global-linum-mode 1)
;; (setq linum-format "%3d ")
;; (add-hook 'prog-mode-hook 'linum-mode)
(use-package linum
  :init
  (progn
    (global-linum-mode t)
    (setq linum-format "%4d  ")
    (set-face-background 'linum nil)
    ))

;; copy from [极简Emacs开发环境配置](https://huadeyu.tech/tools/emacs-setup-notes.html)
;; Json
(use-package json-mode)
;; Yaml
(use-package yaml-mode)
;; Dockfile
(use-package dockerfile-mode)
;; Protobuf
(use-package protobuf-mode)

(use-package flycheck-rust)
(use-package flycheck)

(use-package rustic
  :ensure
  :init
  (setq rustic-treesitter-derive t)
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
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

  ;; comment to disable rustfmt on save
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-lsp-client 'lsp-mode)
  ;;(setq rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;;(use-package eglot
;;  :ensure t
;;  :config
;;  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
;;  (add-hook 'rustic-mode-hook 'eglot-ensure)
;;  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))




(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-enable-snippet t)
  (lsp-keep-workspace-alive t)
  (lsp-enable-xref t)
  (lsp-enable-imenu t)
  (lsp-enable-completion-at-point nil)
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
  :config
  ;; copy from [极简Emacs开发环境配置](https://huadeyu.tech/tools/emacs-setup-notes.html)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'html-mode-hook #'lsp)
  ;;(add-hook 'js-mode-hook #'lsp)
  ;;(add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'json-mode-hook #'lsp)
  (add-hook 'yaml-mode-hook #'lsp)
  (add-hook 'dockerfile-mode-hook #'lsp)
  (add-hook 'shell-mode-hook #'lsp)
  (add-hook 'css-mode-hook #'lsp)
  (add-hook 'lua-mode-hook 'lsp)
  ;; copy from https://sagot.dev/en/articles/emacs-typescript/
  (add-hook 'typescript-mode-hook 'lsp-deferred)
  ;;(add-hook 'javascript-mode-hook 'lsp-deferred)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
                    :major-modes '(python-mode)
                    :server-id 'pyls))
  (setq company-minimum-prefix-length 1
	    company-idle-delay 0.500) ;; default is 0.2
  ;;(require 'lsp-clients)

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
  (setq lsp-clients-lua-language-server-install-dir "/usr/local/Cellar/lua-language-server/3.6.4/"
	    lsp-clients-lua-language-server-bin (f-join lsp-clients-lua-language-server-install-dir "bin/lua-language-server")
	    lsp-clients-lua-language-server-main-location (f-join lsp-clients-lua-language-server-install-dir "libexec/main.lua")
	    lsp-lua-workspace-max-preload 8192
	    lsp-lua-workspace-preload-file-size 1024
	    )
  )

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure
  :config

  (global-company-mode 1)
  (setq company-idle-delay 0.01)
  (setq company-minimum-prefix-length 1)
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  ;;(:map company-mode-map
  ;;      ("<tab>". tab-indent-or-complete)
  ;;      ("TAB". tab-indent-or-complete))
  )

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


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Create / cleanup rust scratch projects quickly

(use-package rust-playground :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	       :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))


;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
(add-hook 'rust-mode-hook (lambda ()
                            (flycheck-rust-setup)
                            (lsp)
                            (flycheck-mode)
			                (yas-minor-mode)
                            ))

;;(require 'rust-mode)
(use-package rust-mode :ensure)

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
(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))
(add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

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

;; 增强 minibuffer 补全：vertico 和 Orderless, 垂直补全
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; minibuffer 模糊匹配
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

(provide 'init-config-packages)
