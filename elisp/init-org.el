;;; -*- coding: utf-8; lexical-binding: t -*-

;; Configure Org and turn on minor modes for org-mode.
(use-package org
  :ensure t
  :mode
  ("\\.org\\'" . org-mode)

  :config
  ;; My Org entries get refiled a lot. I sometimes get into a
  ;; situation where I am looking at a notes.org file on two
  ;; computers, and I don't remember which one is the newest.
  ;;
  ;; How I solve that problem is a log every event that happens with
  ;; an entry. When I see two entries with the same heading, I can
  ;; look at their log and tell which one is newest.
  ;;
  ;; org-log-into-drawer tells Org to put the log data into a LOGBOOK drawer.
  (setq org-log-into-drawer "LOGBOOK")
  ;;
  ;; The LOGBOOK is for change events that happen with the
  ;; entry. Notes about the task itself are within the body.
  ;;
  ;; These variables tell Org what and how we want to log events. All
  ;; these things are important enough that I give myself the option
  ;; of attaching a note describing the reason why the entry changed.
  ;;
  ;;
  (setq org-log-done 'note)
  (setq org-log-reschedule 'note)
  (setq org-log-redeadline 'note)
  (setq org-log-refile 'note)
  (setq org-log-repeat 'note)
  ;; disable toc export
  (setq org-export-with-toc nil)
  ;;
  ;; org-log-note-headings are templates for the log message written
  ;; to the LOGBOOK
  (setq org-log-note-headings
        '((done . "CLOSING NOTE %t")
          (state . "State %-12s from %-12S %t")
          (note . "Note taken on %t")
          (reschedule . "Rescheduled from %S to %s on %t")
          (delschedule . "Delete scheduled, was %S on %t")
          (redeadline . "Redeadlined from %S to %s on %t")
          (deldeadline . "Delete deadline, was %S on %t")
          (refile . "Refiled on %t")
          (clock-out . "")))

  :hook
  (org-mode . (lambda ()
                (rainbow-mode 1)
                ;; electric-pair-mode makes using the <src snippet
                ;; annoying.  It was useful in editting lisp code but
                ;; I can call org-src-special using C-c ' to edit src
                ;; blocks in their native major mode
                (electric-pair-local-mode -1)
                ;; org-indent-mode hides the extra leading *'s on
                ;; headings. I initially didn't like this mode because
                ;; I want to see the actual characters, but for
                ;; complex org files, it cleans up the visual clutter
                (org-indent-mode 1)
                (auto-fill-mode 1)))

  (before-save . (lambda ()
                   (org-update-all-dblocks)))
  )

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

;; Org fancy Priorities
(use-package org-fancy-priorities
  :diminish
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

(use-package org-gtd
  :ensure t
  :after org
  :demand t
  :init
  (setq org-gtd-update-ack "2.1.0")
  (defun douo/org-gtd-archive ()
    "Process GTD inbox item as a reference item without jump to inbox."
    (interactive)
    (with-org-gtd-context (org-archive-subtree))
    )
  :custom
  (org-gtd-directory douo/gtd-home)
  :bind
  (("C-c c" . org-gtd-capture)
   ("C-c e" . org-gtd-engage)
   ("C-c p" . org-gtd-process-inbox)
   ("C-c n" . org-gtd-show-all-next)
   ("C-c s" . org-gtd-show-stuck-projects)
   :map org-gtd-process-map
   ("C-c C" . org-gtd-choose)
   :map org-mode-map
   ("C-c d a" . douo/org-gtd-archive)
   )
  )

(provide 'init-org)
;;; init-org ends here
