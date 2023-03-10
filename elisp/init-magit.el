;; https://github.com/Abuelodelanada/pepe-emacs-config/blob/301a42b030f4774f831de30657215ba2b489d823/use-package.el
(use-package magit
  ;;:straight '(magit :type git :host github :repo "magit/magit")
  :ensure t
  :bind (("C-x g" . magit-status))
  :custom-face
  (magit-branch-local ((t (:foreground "orange"))))
  (magit-branch-remote ((t (:foreground "#D90F5A"))))
  (magit-diff-removed ((t (:foreground "orange red"))))
  (magit-diff-removed-highlight ((t (:foreground "orange red"))))
  (magit-filename ((t (:foreground "#F34739" :weight normal))))
  (magit-hash ((t (:foreground "#FF6E27"))))
  (magit-log-author ((t (:foreground "orange"))))
  (magit-log-date ((t (:foreground "#FF6E27"))))
  (magit-log-graph ((t (:foreground "#75715E"))))
  (magit-section-heading ((t (:foreground "#FF6E27" :weight bold))))
  (magit-tag ((t (:foreground "orange" :weight bold))))
  (magit-diff-added
   ((t (:extend t :background "gray20" :foreground "green"))))
  (magit-diff-added-highlight
   (
    (t
     (:extend
      t
      :background "gray20"
      :foreground "green"
      :weight bold))))
  (magit-diff-context ((t (:extend t :foreground "blue"))))
  (magit-diff-context-highlight
   ((t (:extend t :background "grey20" :foreground "grey70"))))
  (magit-diff-removed
   ((t (:extend t :background "gray20" :foreground "red"))))
  (magit-diff-removed-highlight
   (
    (t
     (:extend
      t
      :background "gray20"
      :foreground "red"
      :weight bold))))
  (magit-section-highlight ((t (:background "gray20"))))
  ;; :config
  ;; (add-hook 'magit-process-find-password-functions
  ;;           'magit-process-password-auth-source)
  :init
  ;; enable magit ask password prompt
  (setenv "LANG" "en_US.UTF-8")
  (set-locale-environment "en_US.UTF-8")
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  ;; load theme after magit
  (add-hook 'magit-mode-hook
            (lambda ()
              ;; (load-theme 'doom-one t t)
              (load-theme 'moe-dark t t)
              ))
  ;; (add-hook 'magit-mode-hook 'my-inhibit-global-linum-mode)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  :config
  (with-eval-after-load 'magit
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

(use-package magit-process
  :ensure magit
  :after magit
  :config
  (setq magit-process-extreme-logging t)
  )

(use-package magit-git
  :ensure magit
  :after magit
  :config
  (setq magit-git-debug t)
  )


(use-package magit-gitflow
  :after magit)

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :bind ((:map diff-hl-command-map
               ("SPC" . diff-hl-mark-hunk)))
  :hook ((after-init . global-diff-hl-mode)
         (diff-hl-mode . diff-hl-flydiff-mode))
  :init (setq diff-hl-side 'left
              diff-hl-draw-borders nil
              diff-hl-show-staged-changes nil)
  :config
  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; show git commit info
(use-package magit-stats
  :ensure t
  :defer t)

;; Access Git forges from Magit
(use-package forge
  :ensure t
  :after magit
  :demand t
  :defines (forge-database-connector forge-topic-list-columns)
  :custom-face
  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
  :init
  (setq forge-database-connector (if (and (require 'emacsql-sqlite-builtin nil t)
                                          (functionp 'emacsql-sqlite-builtin)
                                          (functionp 'sqlite-open))
                                     'sqlite-builtin
                                   'sqlite)
        forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 60 t nil title  nil)
          ("State" 6 t nil state nil)
          ("Updated" 10 t nil updated nil)))
  :config
  (setq auth-sources '("~/.authinfo"))
  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-authored-pullreqs nil 'append)
  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-requested-reviews nil 'append)
  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues nil 'append)
  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-authored-issues nil 'append)
  )

;; A git blame plugin for emacs
(use-package blamer
  :ensure t
  :bind (("C-c s" . blamer-show-commit-info)
         ("C-c i" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t)))
  :config
  (global-blamer-mode 1))

(provide 'init-magit)
;;; init-magit.el ends here
