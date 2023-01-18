;;; init.el -*- lexical-binding: t; -*-

(setq use-package-compute-statistics t)
(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'cl-lib)

(require 'init-package)

;;; move customize-set-variable out of init.el
;;(setq custom-file "~/.emacs.d/custom.el")
;;(unless (file-exists-p custom-file)  ;; 如果该文件不存在
;;  (write-region "" nil custom-file)) ;; 写入一个空内容，相当于 touch 一下它
;;(load custom-file)


(require 'init-config-packages)


(require 'init-misc)

(require 'init-cpp)

(require 'init-font)

(require 'init-macro)

(message (emacs-init-time))
