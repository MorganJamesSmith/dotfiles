;;; .emacs --- Emacs configuration file by Morgan Smith
;;

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/use-package")
  (require 'use-package))

(require 'use-package)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(server-start)

(menu-bar-mode 0)
(tool-bar-mode 0)

(blink-cursor-mode 0)

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

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<tab>" 'whitespace-mode
    "o"     'ispell
    "c"     'company-complete
    "e"     'find-file
    "j"     'vertigo-jump-down
    "k"     'vertigo-jump-up)
  :ensure t)

(use-package evil
  :config (evil-mode 1)
  :ensure t)

(use-package powerline
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
  (evil-define-key 'normal nov-mode-map "L" 'nov-next-document)
  (evil-define-key 'normal nov-mode-map "[" 'nov-previous-document)
  (evil-define-key 'normal nov-mode-map "H" 'nov-previous-document)
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
 '(package-selected-packages
   (quote
    (nov ledger-mode company xcscope vertigo evil-leader diff-hl airline-themes powerline evil)))
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
