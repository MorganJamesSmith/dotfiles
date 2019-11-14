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

(require 'cl)

(server-start)

(menu-bar-mode 0)
(tool-bar-mode 0)

(blink-cursor-mode 0)

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

(use-package xcscope
  :config
  (cscope-setup)
  :ensure t)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :ensure t)

(use-package vertigo
  :init (setq vertigo-cut-off 9)
  :ensure t)

(use-package smartparens
  :init (require 'dash)
  :config
  (smartparens-global-mode t)
  :ensure t)

(use-package undo-tree
  :ensure t)

(use-package magit
  :ensure t)

;; I can't figure out why this isn't working
;; but it's super important so I'll leave it
(use-package nyan-mode
  :config
  (progn
    (nyan-mode)
    (nyan-start-animation))
  :ensure t)

(use-package powerline
  :config
  (powerline-default-theme)
  :ensure t)


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
  (setq evil-motion-state-modes (set-difference evil-motion-state-modes evil-emacs-state-modes))

  (evil-mode 1)
  :ensure t)

(use-package airline-themes
  :ensure t)

(use-package diff-hl
  :config (global-diff-hl-mode)
  :ensure t)

(use-package ledger-mode
  :init
  (evil-define-key 'normal ledger-mode-map "]" 'ledger-navigate-next-xact-or-directive)
  (evil-define-key 'normal ledger-mode-map "[" 'ledger-navigate-prev-xact-or-directive)
  :ensure t)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (evil-define-key 'normal nov-mode-map "]" 'nov-next-document)
  (evil-define-key 'normal nov-mode-map "[" 'nov-previous-document)
  :ensure t)

(use-package fzf
  :init
  (if (not (eq 0 (shell-command "command -v fzf")))
      (error "fzf is not installed!"))
  :ensure t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(c-basic-offset 4)
 '(custom-enabled-themes (quote (airline-molokai tsdh-dark)))
 '(custom-safe-themes
   (quote
    ("73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" default)))
 '(display-line-numbers (quote relative))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(initial-scratch-message nil)
 '(ledger-reports
   (quote
    (("mon" "%(binary) -f %(ledger-file) bal -p \"this month\"")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
 '(package-selected-packages
   (quote
    (nyan-mode magit smartparens fzf nov ledger-mode company xcscope vertigo evil-leader diff-hl airline-themes powerline evil)))
 '(scroll-bar-mode nil)
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)

;;; .emacs ends here
