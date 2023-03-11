;;; init.el --- Summary Emacs Startup File -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs Startup File

;;; Code:

(setq use-package-compute-statistics t)
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; (require 'cl-lib)

;;(require 'init-package)

;;; move customize-set-variable out of init.el
;; (setq custom-file "~/.emacs.d/custom.el")
;; (unless (file-exists-p custom-file)  ;; 如果该文件不存在
;;   (write-region "" nil custom-file)) ;; 写入一个空内容，相当于 touch 一下它
;; (load custom-file)

;;(require 'init-company)

(require 'init-config-packages)

(require 'init-corfu)

(require 'init-rust)

(require 'init-org)

(require 'init-flycheck)

(require 'init-latex)

(require 'init-lsp)

(require 'init-flutter)

(require 'init-magit)

(require 'init-lua)

(require 'init-cpp)

(require 'init-lisp)

(require 'init-font)

(require 'init-macro)

(require 'init-kotlin)

(require 'init-swift)

(require 'init-uml)

(require 'init-ox)

(require 'init-dired)

(require 'init-julia)

(require 'init-php)

(require 'init-vterm)

(require 'init-python)

(require 'init-leetcode)
;; (require 'init-misc)

;; (setq debug-on-quit t)

;; copy from https://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)



;; brew install pyqt
;; check eaf pdf
;;(unless (file-exists-p (expand-file-name "straight/repos/emacs-application-framework/app/pdf-viewer" user-emacs-directory))
;;  (eaf-install-and-update))

;; (put 'erase-buffer 'disabled nil)

(message (emacs-init-time))

(provide 'init)
;;; init.el ends here
