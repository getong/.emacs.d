;;  -*- lexical-binding: t; -*-

(use-package corfu
  :ensure t
  :bind
  ((:map corfu-map
         ([tab] . #'corfu-next)
         ("C-n" . #'corfu-next)
         ("C-p" . #'corfu-previous)
         ("<escape>" . #'corfu-quit)
         ("<return>" . #'corfu-insert)
         ("H-SPC" . #'corfu-insert-separator)
         ;; "SPC" #'corfu-insert-separator ; Use when `corfu-quit-at-boundary' is non-nil
         ("M-d" . #'corfu-show-documentation)
         ("C-g" . #'corfu-quit)
         ("M-l" . #'corfu-show-location)))
  :custom
  ;; works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle t)

  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first nil)        ; Preselect first candidate?

  ;; Other
  (corfu-echo-documentation nil)        ; Already use corfu-doc
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)

  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

  ;; Setup lsp to use corfu for lsp completion
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  (after-init . global-corfu-mode)
  (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  ;;(after-init . (lambda ()
  ;;                (company-mode -1)))
  )

(use-package corfu-echo
  :ensure corfu
  :hook (corfu-mode . corfu-echo-mode))

(use-package corfu-popupinfo
  :ensure corfu
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package corfu-history
  ;; 记录最近一次补全 在下一次出现同样的触发按键时排第一位
  :ensure corfu
  :after savehist
  :config
  ;; NOTE: should be unconditionally depend on `savehist`?
  ;; (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  )

;; kind-icon 已经增加了一个命令，M-x kind-icon-preview-all 执行就可以预览全部预设的图标，并自动下载全部图标到 ~/.emacs.d/.cache/svg-lib 文件夹。
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  ;; (svg-lib-icons-dir "~/env/emacs/cache/")
  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  (kind-icon-mapping
   '((array "a" :icon "code-brackets" :face font-lock-variable-name-face)
     (boolean "b" :icon "circle-half-full" :face font-lock-builtin-face)
     (class "c" :icon "view-grid-plus-outline" :face font-lock-type-face)
     (color "#" :icon "palette" :face success)
     (constant "co" :icon "pause-circle" :face font-lock-constant-face)
     (constructor "cn" :icon "table-column-plus-after" :face font-lock-function-name-face)
     (enum "e" :icon "format-list-bulleted-square" :face font-lock-builtin-face)
     (enum-member "em" :icon "format-list-checks" :face font-lock-builtin-face)
     (event "ev" :icon "lightning-bolt-outline" :face font-lock-warning-face)
     (field "fd" :icon "application-braces-outline" :face font-lock-variable-name-face)
     (file "f" :icon "file" :face font-lock-string-face)
     (folder "d" :icon "folder" :face font-lock-doc-face)
     (function "f" :icon "lambda" :face font-lock-function-name-face)
     (interface "if" :icon "video-input-component" :face font-lock-type-face)
     (keyword "kw" :icon "image-filter-center-focus" :face font-lock-keyword-face)
     (macro "mc" :icon "sigma" :face font-lock-keyword-face)
     (method "m" :icon "lambda" :face font-lock-function-name-face)
     (module "{" :icon "view-module" :face font-lock-preprocessor-face)
     (numeric "nu" :icon "numeric" :face font-lock-builtin-face)
     (operator "op" :icon "plus-circle-outline" :face font-lock-comment-delimiter-face)
     (param "pa" :icon "cog" :face default)
     (property "pr" :icon "tune-vertical" :face font-lock-variable-name-face)
     (reference "rf" :icon "bookmark-box-multiple" :face font-lock-variable-name-face)
     (snippet "S" :icon "text-short" :face font-lock-string-face)
     (string "s" :icon "sticker-text-outline" :face font-lock-string-face)
     (struct "%" :icon "code-braces" :face font-lock-variable-name-face)
     (t "." :icon "crosshairs-question" :face shadow)
     (text "tx" :icon "script-text-outline" :face shadow)
     (type-parameter "tp" :icon "format-list-bulleted-type" :face font-lock-type-face)
     (unit "u" :icon "ruler-square" :face shadow)
     (value "v" :icon "numeric-1-box-multiple-outline" :face font-lock-builtin-face)
     (variable "va" :icon "adjust" :face font-lock-variable-name-face)))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))


(use-package cape
  :bind
  (("C-c p p" . completion-at-point) ;; capf
   ("C-c p t" . complete-tag)        ;; etags
   ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
   ("C-c p h" . cape-history)
   ("C-c p f" . cape-file)
   ("C-c p k" . cape-keyword)
   ("C-c p s" . cape-symbol)
   ("C-c p a" . cape-abbrev)
   ("C-c p i" . cape-ispell)
   ("C-c p l" . cape-line)
   ("C-c p w" . cape-dict)
   ("C-c p \\" . cape-tex)
   ("C-c p _" . cape-tex)
   ("C-c p ^" . cape-tex)
   ("C-c p &" . cape-sgml)
   ("C-c p r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package tabnine-capf
  :after cape
  :straight (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh"))
  :hook (kill-emacs . tabnine-capf-kill-process)
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)

  (unless (file-exists-p (no-littering-expand-var-file-name "tabnine-capf/tabnine-binaries"))
    (tabnine-capf-install-binary))
  )

(use-package tempel
  :straight t
  :defer 10
  :custom
  ;; (tempel-path "~/.dotfiles/Emacs/templates")
  (tempel-path (no-littering-expand-var-file-name "templ-templates"))
  :hook ((prog-mode text-mode) . tempel-setup-capf)
  :bind (("M-+" . tempel-insert) ;; Alternative tempel-expand
         :map tempel-map
         ([remap keyboard-escape-quit] . tempel-done)
         ("TAB" . tempel-next)
         ("<backtab>" . tempel-previous)
         :map corfu-map
         ("C-M-i" . tempel-expand))
  ;; :map tempel-map
  ;; ("M-]" . tempel-next)
  ;; ("M-[" . tempel-previous))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions))))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  :ensure t
  ;; Swap M-/ and C-M-/
  ;;
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


(use-package tmux-capf
  :straight (:host github :repo "theFool32/tmux-capf" :files ("*.el" "*.sh"))
  :after cape
  :commands tmux-capf)

(provide 'init-corfu)
