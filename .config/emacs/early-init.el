;; early-init --- My personal early-init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my personal Emacs early-init file.
;; It is crafted with love, care, and stupidity.
;; Use at your own risk.

;;; Code:

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Disable package as I use straight
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
