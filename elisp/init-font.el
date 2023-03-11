;;; init-font.el --- Summary init font -*- lexical-binding: t -*-

;;; Commentary:
;; init font

;;; Code:

;; copy from https://quant67.com/post/emcas/init-config.html
;; default
;;(set-face-attribute 'default nil :font (font-spec :family "Source Code Pro"
;; :size 14))

(when (eq system-type 'darwin)
  ;; (set-frame-font "EB Garamond 12 Italic 20" nil t)
  (set-frame-font "Jetbrains Mono" nil t)
  ;;(setq fonts '("EB Garamond 12 Italic"  "SF Mono" "冬青黑体简体中文"))
  ;;(set-fontset-font t 'han "LXGW WenKai Mono")
  ;;(set-fontset-font t 'unicode "LXGW WenKai Mono")
  ;;(set-fontset-font t 'unicode-bmp "LXGW WenKai Mono")
  ;; (set-face-attribute 'default nil :font  "EB Garamond 12 Italic 20")
  (set-face-attribute 'default nil :font  "Jetbrains Mono")
  (set-fontset-font "fontset-default"
                    'han (font-spec :family "LXGW WenKai Mono"
                                    :size 12))
  (set-fontset-font "fontset-default"
                    'unicode (font-spec :family "LXGW WenKai Mono"
                                        :size 12))
  (set-fontset-font "fontset-default"
                    'unicode-bmp (font-spec :family "LXGW WenKai Mono"
                                            :size 12))
  ;;(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
  )

(when (eq system-type 'gnu/linux)
  ;; (set-frame-font "EB Garamond 12 Italic 20" nil t)
  (set-frame-font "Jetbrains Mono" nil t)
  ;;(setq fonts '("EB Garamond 12 Italic"  "SF Mono" "冬青黑体简体中文"))
  ;;(set-fontset-font t 'han "LXGW WenKai Mono")
  ;;(set-fontset-font t 'unicode "LXGW WenKai Mono")
  ;;(set-fontset-font t 'unicode-bmp "LXGW WenKai Mono")
  ;; (set-face-attribute 'default nil :font  "EB Garamond 12 Italic 20")
  (set-face-attribute 'default nil :font  "Jetbrains Mono")
  (set-fontset-font "fontset-default"
                    'han (font-spec :family "LXGW WenKai Mono"
                                    :size 12))
  (set-fontset-font "fontset-default"
                    'unicode (font-spec :family "LXGW WenKai Mono"
                                        :size 12))
  (set-fontset-font "fontset-default"
                    'unicode-bmp (font-spec :family "LXGW WenKai Mono"
                                            :size 12))
  ;;(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
  )


(provide 'init-font)
;;; init-font.el ends here
