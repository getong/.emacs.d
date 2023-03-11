;;; init-leetcode.el --- Summary leetcode -*- lexical-binding: t -*-

;;; Commentary:
;; leetcode

;;; Code:

(use-package leetcode
  :defer t
  :commands (leetcode)
  :config
  (url-debug t)
  (setq leetcode-prefer-language "rust"))

(provide 'init-leetcode)
;;; init-leetcode.el ends here
