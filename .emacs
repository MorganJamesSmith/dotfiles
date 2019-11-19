;;; .emacs --- Emacs configuration file by Morgan Smith
;;

;; Add and enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Get use-package
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/use-package"))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t) ; Always download all my packages

;; Backups
(defvar backup-directory "~/.backups")
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))
(setq
 make-backup-files t    ; backup a file the first time it is saved
 backup-directory-alist `((".*" . ,backup-directory)) ; save backup files in ~/.backups
 backup-by-copying t    ; copy the current file into backup directory
 version-control t      ; version numbers for backup files
 delete-old-versions t  ; delete unnecessary versions
 kept-old-versions 6    ; oldest versions to keep when a new numbered backup is made (default: 2)
 kept-new-versions 9    ; newest versions to keep when a new numbered backup is made (default: 2)
 auto-save-default t    ; auto-save every buffer that visits a file
 auto-save-timeout 20   ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
 )

(setq debug-on-error t)

(require 'cl)

(server-start)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(blink-cursor-mode -1)

(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)

(setq tramp-default-method "ssh")


(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq-default display-line-numbers 'relative)

;; Whitespace configurations
(setq show-trailing-whitespace t
      mode-require-final-newline t)
(setq-default tab-width 8
              indent-tabs-mode nil)

(defun revert-buffer-no-confirm (&optional force-reverting)
    "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
    (interactive "P")
    ;;(message "force-reverting value is %s" force-reverting)
    (if (or force-reverting (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified")))


(defun compiler ()
  "Saves the current file, then runs the compiler command on the
  current file. Then the buffer is reloaded from the file"
  (interactive)
  (save-buffer)
  (shell-command
   (concat "compiler " buffer-file-name))
  (revert-buffer-no-confirm))

(defun opout ()
  (interactive)
  (shell-command
   (concat "opout " buffer-file-name)))

(use-package vterm)

;; Programming stuff
(use-package flycheck)

(use-package xcscope
  :config
  (cscope-setup))

(use-package smartparens
  :init
  (require 'dash)
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t))

(use-package magit)

(use-package diff-hl
  :config (global-diff-hl-mode))

;; Python stuff
(use-package elpy
  :config
  (setq python-shell-interpreter "/usr/bin/python3"
        python-shell-interpreter-args "-i"
        elpy-rpc-python-command "/usr/bin/python3")
  (elpy-enable))
(use-package blacken)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package undo-tree)

;; Pretty stuff
(use-package nyan-mode
  :config
  (nyan-mode)
  (nyan-start-animation))

(use-package powerline
  :config
  (powerline-default-theme))

(load-theme 'tsdh-dark)


;; Keybinding stuff
(use-package evil
  :config
  (defun leader (key)
    (kbd (concat "SPC " key)))

  (evil-define-key 'normal doc-view-mode-map "]" 'doc-view-next-page)
  (evil-define-key 'normal doc-view-mode-map "[" 'doc-view-previous-page)
  (evil-define-key 'normal doc-view-mode-map "-" 'doc-view-shrink)
  (evil-define-key 'normal doc-view-mode-map "=" 'doc-view-enlarge)
  (evil-define-key 'normal doc-view-mode-map "+" 'doc-view-enlarge)

  (evil-define-key 'motion vc-annotate-mode-map "]" 'vc-annotate-show-log-revision-at-line)
  (evil-define-key 'motion vc-annotate-mode-map "[" 'vc-annotate-show-diff-revision-at-line)

  (evil-define-key 'normal ledger-mode-map (leader "r") 'ledger-report)

  (evil-define-key 'normal 'global (kbd "M-j") 'evil-scroll-line-down)
  (evil-define-key 'normal 'global (kbd "M-k") 'evil-scroll-line-up)
  (evil-define-key 'normal 'global (kbd "M-J") 'text-scale-decrease)
  (evil-define-key 'normal 'global (kbd "M-K") 'text-scale-increase)

  (evil-define-key 'visual 'global (leader "c") 'comment-or-uncomment-region)
  (evil-define-key 'normal 'global (leader "q") 'compiler)
  (evil-define-key 'normal 'global (leader "w") 'opout)
  (evil-define-key 'normal 'global (leader "TAB") 'whitespace-mode)
  (evil-define-key 'normal 'global (leader "o") 'ispell)
  (evil-define-key 'normal 'global (leader "e") 'find-file)
  (evil-define-key 'normal 'global (leader "j") 'vertigo-jump-down)
  (evil-define-key 'normal 'global (leader "k") 'vertigo-jump-up)

  (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
  (setq evil-emacs-state-modes '(calc-mode))
  (setq evil-insert-state-modes '(vterm-mode))
  (setq evil-motion-state-modes (set-difference (set-difference evil-motion-state-modes evil-emacs-state-modes) evil-insert-state-modes))

  (evil-mode 1))

(use-package vertigo
  :init (setq vertigo-cut-off 9))


(use-package ledger-mode
  :config
  (setq ledger-reports
    '(("mon" "%(binary) -f %(ledger-file) bal -p \"this month\"")
      ("bal" "%(binary) -f %(ledger-file) bal")
      ("reg" "%(binary) -f %(ledger-file) reg")
      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
      ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  (evil-define-key 'normal ledger-mode-map "]" 'ledger-navigate-next-xact-or-directive)
  (evil-define-key 'normal ledger-mode-map "[" 'ledger-navigate-prev-xact-or-directive))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (evil-define-key 'normal nov-mode-map "]" 'nov-next-document)
  (evil-define-key 'normal nov-mode-map "[" 'nov-previous-document))

(use-package fzf
  :init
  (if (not (eq 0 (shell-command "command -v fzf")))
    (error "fzf is not installed!")))

(provide '.emacs)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vterm blacken flycheck xcscope vertigo smartparens nyan-mode nov multi-term magit ledger-mode fzf evil-leader elpy diff-hl airline-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
