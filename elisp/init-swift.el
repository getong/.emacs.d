;;; init-swift.el --- Summary swift file -*- lexical-binding: t -*-

;;; Commentary:
;; swift

;;; Code:

;; Copy from https://www.danielde.dev/blog/emacs-for-swift-development
(use-package swift-mode
  :bind (("C-c l" . print-swift-var-under-point))
  :config
  (add-to-list 'projectile-project-root-files
               "Package.swift")
  (add-to-list 'lsp-file-watch-ignored-directories
               "[/\\\\]\\.build\\'")
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

  )

(provide 'init-swift)
;;; init-swift.el ends here
