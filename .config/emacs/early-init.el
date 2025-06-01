;; early-init --- My personal early-init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my personal Emacs early-init file.
;; It is crafted with love, care, and stupidity.
;; Use at your own risk.

;;; Code:

(setq package-enable-at-startup nil)
(setq package-archives nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(when (not (eq system-type 'android))
  (push '(menu-bar-lines . 0)   default-frame-alist)
  (push '(tool-bar-lines . 0)   default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))
(push '(background-color . "#000000") default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Android - Termux integration
(when (eq system-type 'android)
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
                         (getenv "PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path))

(when (fboundp 'load-path-filter-cache-directory-files)
  (setq load-path-filter-function #'load-path-filter-cache-directory-files))

(provide 'early-init)
;;; early-init.el ends here
