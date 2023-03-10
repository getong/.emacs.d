;;; init.el -*- lexical-binding: t; -*-
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-<return>" . xah-open-in-external-app)
              ("W" . dired-copy-path)
              )
  :config
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))
  ;; Enable the disabled dired commands
  (put 'dired-find-alternate-file 'disabled nil)

  ;; open files via external program based on file types, See:
  ;; https://emacs.stackexchange.com/questions/3105/how-to-use-an-external-program-as-the-default-way-to-open-pdfs-from-emacs
  (defun xdg-open (filename)
    (interactive "fFilename: ")
    (let ((process-connection-type))
      (start-process "" nil (cond ((eq system-type 'gnu/linux) "xdg-open")
                                  ((eq system-type 'darwin) "open")
                                  ((eq system-type 'windows-nt) "start")
                                  (t "")) (expand-file-name filename))))
  ;; open files via external program when using find-file
  (defun find-file-auto (orig-fun &rest args)
    (let ((filename (car args)))
      (if (cl-find-if
           (lambda (regexp) (string-match regexp filename))
           '(
             ;; "\\.html?\\'"
             "\\.xlsx?\\'"
             "\\.pptx?\\'"
             "\\.docx?\\'"
             "\\.mp4\\'"
             "\\.app\\'"
             ))
          (xdg-open filename)
        (apply orig-fun args))))
  (advice-add 'find-file :around 'find-file-auto)

  (defun dired-copy-path ()
    "In dired, copy file path to kill-buffer.
At 2nd time it copy current directory to kill-buffer."
    (interactive)
    (let (path)
      (setq path (dired-file-name-at-point))
      (if (string= path (current-kill 0 1)) (setq path (dired-current-directory)))
      (message path)
      (kill-new path)))

  (defun xah-open-in-external-app (&optional @fname)
    "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
    (interactive)
    (let* (
           ($file-list
            (if @fname
                (progn (list @fname))
              (if (or (string-equal major-mode "dired-mode")
                      (string-equal major-mode "dirvish-mode"))
                  (dired-get-marked-files)
                (list (buffer-file-name)))))
           ($do-it-p (if (<= (length $file-list) 5)
                         t
                       (y-or-n-p "Open more than 5 files? "))))
      (when $do-it-p
        (cond
         ((string-equal system-type "windows-nt")
          (mapc
           (lambda ($fpath)
             (w32-shell-execute "open" $fpath)) $file-list))
         ((string-equal system-type "darwin")
          (mapc
           (lambda ($fpath)
             (shell-command
              (concat "open " (shell-quote-argument $fpath))))  $file-list))
         ((string-equal system-type "gnu/linux")
          (mapc
           (lambda ($fpath) (let ((process-connection-type nil))
                              (start-process "" nil "xdg-open" $fpath))) $file-list))))))
  :custom
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-bind-vm nil)
  (dired-bind-man nil)
  (dired-bind-info nil)
  (dired-clean-up-buffers-too t)
  (dired-recursive-copies 'always)
  (dired-omit-verbose nil)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-AFhlv"))

(use-package dired-aux
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c +" . dired-create-empty-file))
  :config
  ;; with the help of `evil-collection', P is bound to `dired-do-print'.
  (define-advice dired-do-print (:override (&optional _))
    "Show/hide dotfiles."
    (interactive)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
        (progn
          (setq-local dired-dotfiles-show-p nil)
          (dired-mark-files-regexp "^\\.")
          (dired-do-kill-lines))
      (revert-buffer)
      (setq-local dired-dotfiles-show-p t)))
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :init
  (setq dired-guess-shell-alist-user `((,(rx "."
                                             (or
                                              ;; Videos
                                              "mp4" "avi" "mkv" "flv" "ogv" "ogg" "mov"
                                              ;; Music
                                              "wav" "mp3" "flac"
                                              ;; Images
                                              "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
                                              ;; Docs
                                              "pdf" "md" "djvu" "ps" "eps" "doc" "docx" "xls" "xlsx" "ppt" "pptx")
                                             string-end)
                                        ,(cond ((eq system-type 'gnu/linux) "xdg-open")
                                               ((eq system-type 'darwin) "open")
                                               ((eq system-type 'windows-nt) "start")
                                               (t "")))))
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files (rx string-start
                        (or ".DS_Store"
                            ".cache"
                            ".vscode"
                            ".ccls-cache" ".clangd")
                        string-end))
  ;; Dont prompt about killing buffer visiting delete file
  (dired-clean-confirm-killing-deleted-buffers nil)
  )


(use-package diredfl
  :ensure t
  :commands diredfl-global-mode
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and preview as well
   (dirvish-directory-view-mode . diredfl-mode))
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

(use-package all-the-icons
  :ensure t
  :when (display-graphic-p)
  :commands all-the-icons-install-fonts
  )

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package all-the-icons-completion
  :ensure t
  :hook ((after-init . all-the-icons-completion-mode)
         (marginalia-mode . all-the-icons-completion-marginalia-setup))
  )

(use-package dirvish
  :ensure t
  :hook (after-init . dirvish-override-dired-mode)
  :bind (
         ("C-c f" . dirvish-fd)
         ("C-x d" . dirvish)
         :map dired-mode-map
         ("a"   . dirvish-quick-access)
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
         ("M-j" . dirvish-fd-jump)
         ("TAB" . dirvish-toggle-subtree)
         ("SPC" . dirvish-show-history)
         ("r"   . dirvish-roam)
         ("b"   . dirvish-goto-bookmark)
         ("f"   . dirvish-file-info-menu)
         ("M-n" . dirvish-go-forward-history)
         ("M-p" . dirvish-go-backward-history)
         ("M-f" . dirvish-toggle-fullscreen)
         ([remap dired-sort-toggle-or-edit] . dirvish-quicksort)
         ([remap dired-do-redisplay] . dirvish-ls-switches-menu)
         ([remap dired-summary] . dirvish-dispatch)
         ([remap dired-do-copy] . dirvish-yank-menu)
         ([remap mode-line-other-buffer] . dirvish-other-buffer))
  :after (diredfl all-the-icons)
  :config
  (dirvish-peek-mode)
  (setq dirvish-hide-details t)
  ;; open mp4 file via external program which is mpv here.
  (add-to-list 'mailcap-mime-extensions '(".mp4" . "video/mp4"))
  ;; (add-to-list 'dirvish-open-with-programs '(
  ;;                                            (("html") . ("open" "%f"))
  ;;                                            (("xlsx") . ("open" "%f"))
  ;;                                            (("pptx") . ("open" "%f"))
  ;;                                            (("docx") . ("open" "%f"))
  ;;                                            (("md")   . ("open" "%f"))
  ;;                                            ))
  (dirvish-define-preview exa (file)
    "Use `exa' to generate directory preview."
    :require ("exa") ; tell Dirvish to check if we have the executable
    (when (file-directory-p file) ; we only interest in directories here
      `(shell . ("exa" "-al" "--color=always" "--icons"
                 "--group-directories-first" ,file))))

  (add-to-list 'dirvish-preview-dispatchers 'exa)

  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))
  ;; 不预览epub文件
  (setq dirvish-preview-dispatchers (remove 'epub dirvish-preview-dispatchers))
  ;; 异步读取含 10000 个以上文件的文件夹
  (setq dirvish-async-listing-threshold 10000
        dirvish-cache-dir (no-littering-expand-var-file-name "dirvish" )
        ;; 高亮当前文件
        dirvish-hide-cursor t
        dired-filter-revert 'always
        dirvish-reuse-session t
        dirvish-depth 0
        dirvish-header-line-format
        '(:left (path) :right (free-space))
        ;; hide the parent directory
        ;; dirvish-default-layout '(0 0.4 0.6)
        dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index))
        dirvish-attributes '(all-the-icons collapse file-time file-size subtree-state vc-state git-msg)
        delete-by-moving-to-trash t
        dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"
        dirvish-subtree-always-show-state t
        dirvish-side-width 25
        ;; make header line span all panes
        dirvish-use-header-line 'global
        dirvish-side-window-parameters nil
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        ;; don't hide any files
        dired-omit-files nil
        )
  (set-face-attribute 'dirvish-hl-line nil
                      :foreground (face-attribute 'diredfl-flag-mark :foreground)
                      :background (face-attribute 'diredfl-flag-mark :background))
  :custom
  (dirvish-menu-bookmarks '(("h" "~/"             "Home")
                            ("d" "~/Downloads/"   "Downloads")
                            ;; ("t" "~/.local/share/Trash/files/" "TrashCan")
                            ))
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("a" "~/"             "Home")
     ("b" "~/Downloads/"   "Downloads")
     ("c" "~/vbox_share/"  "vbox_share")
     ("d" "~/other_project/"  "other_project")
     ))
  (dirvish-mode-line-format '(:left
                              (sort file-time " " file-size symlink) ; it's ok to place string inside
                              :right
                              ;; For `dired-filter' users, replace `omit' with `filter' segment defined below
                              (omit yank index)))
  (dirvish-attributes '(subtree-state
                        file-size
                        vc-state
                        git-msg
                        ;; all-the-icons
                        ))
  )

(use-package dirvish-side :ensure dirvish :after dirvish)
(use-package dirvish-vc :ensure dirvish :after (magit dirvish))
(use-package dirvish-extras :ensure dirvish :after dirvish)

(use-package embark
  :ensure t
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-'" . embark-act)
         :map minibuffer-local-map
         :map minibuffer-local-completion-map
         ("TAB" . minibuffer-force-complete)
         :map embark-file-map
         ("E" . consult-file-externally)      ; Open file externally, or `we' in Ranger
         ("O" . consult-directory-externally) ; Open directory externally
         (:map minibuffer-mode-map
               ("M-o" . embark-export)
               ("M-." . embark-act)
               )
         (("C-." . embark-act)         ;; pick some comfortable binding
          ("C-;" . embark-dwim)        ;; good alternative: M-.
          ("C-h B" . embark-bindings))  ;; alternative for `describe-bindings'
         )
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)

  ;; open directory
  (defun consult-directory-externally (file)
    "Open directory externally using the default application of the system."
    (interactive "fOpen externally: ")
    (if (and (eq system-type 'windows-nt)
             (fboundp 'w32-shell-execute))
        (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
                                                                                 (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
      (call-process (pcase system-type
                      ('darwin "open")
                      ('cygwin "cygstart")
                      (_ "xdg-open"))
                    nil 0 nil
                    (file-name-directory (expand-file-name file)))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-dired)
;;; init-dired.el ends here
