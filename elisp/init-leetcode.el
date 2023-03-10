;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package leetcode
  :defer t
  :commands (leetcode)
  :config
  (url-debug t)
  (setq leetcode-prefer-language "rust"))

(provide 'init-leetcode)
;;; init-leetcode.el ends here
