;;; init.el --- My personal init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my personal Emacs init file.
;; It is crafted with love, care, and stupidity.
;; Use at your own risk.

;;; Code:

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; straight package manager setup
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(customize-set-variable 'straight-use-package-by-default t)

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

;; Add type validation to customize-set-variable function
(use-package validate
  :config
  (defun validate-value-variable (symbol value &optional _comment)
    "Validate the SYMBOL can be set to VALUE by checking SYMBOL's type."
    (validate-value value (custom-variable-type symbol)))
  (advice-add #'customize-set-variable :before #'validate-value-variable))

(customize-set-variable 'user-full-name "Morgan Smith")

(defun expand-create-directory-name (dir &optional default-dir)
  "Return DIR as a directory and create DIR if it doesn't already exist.
If DIR is relative, it will be relative to DEFAULT-DIR
If DEFAULT-DIR isn't provided, DIR is relative to ~"
  (unless default-dir
    (setq default-dir "~"))
  (let ((directory (file-name-as-directory (expand-file-name dir default-dir))))
    (unless (file-exists-p directory)
      (make-directory directory))
    directory))

;;; Optimization Section Begins

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;; Display the bare minimum at startup
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-echo-area-message user-login-name)
(customize-set-variable 'inhibit-default-init t)
(customize-set-variable 'initial-major-mode 'fundamental-mode)
(customize-set-variable 'initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Disable bidirectional text rendering
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Do not render cursors or regions in non-focused windows.
(customize-set-variable 'cursor-in-non-selected-windows nil)
(customize-set-variable 'highlight-nonselected-windows nil)

(customize-set-variable 'fast-but-imprecise-scrolling t)

(customize-set-variable 'frame-inhibit-implied-resize t)

(customize-set-variable 'auto-mode-case-fold nil)

(customize-set-variable 'create-lockfiles nil) ; Only matters on multi-user systems
;;; Optimization Section Ends

(use-package delight)

(use-package erc
  :straight nil
  :custom
  (erc-server "irc.freenode.net")
  (erc-port 6667)
  (erc-nick "butterypancake")
  (erc-user-full-name user-full-name)
  (erc-anonymous-login t)
  (erc-log-channels-directory (expand-create-directory-name "erc-logs" user-emacs-directory))
  (erc-save-buffer-on-part t)
  :config
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'keep-place))

(use-package debbugs)

;;; Pretty Visuals Section Begins
(use-package modus-vivendi-theme
  :custom
  (custom-safe-themes t "All themes are now safe")
  (custom-enabled-themes '(modus-vivendi) "Pretty cool dark theme"))

(global-hl-line-mode t)

(customize-set-variable 'echo-keystrokes 0.02)

;; I dislike gui stuff
(customize-set-variable 'use-file-dialog nil)
(customize-set-variable 'use-dialog-box nil)
(customize-set-variable 'visible-bell t)

(blink-cursor-mode 0)
;;; Pretty Visuals Section Ends


;;; Sensible Default Section Begins
(global-auto-revert-mode t)

;; Replace the info command with something more useful
(global-set-key (kbd "C-h i") 'info-display-manual)

(fset #'yes-or-no-p #'y-or-n-p)

(use-package gnus
  :straight nil
  :custom
  (gnus-init-file (expand-file-name "gnus" user-emacs-directory))
  (gnus-home-directory (expand-create-directory-name "gnus-files" user-emacs-directory))
  (gnus-directory (expand-create-directory-name "News" gnus-home-directory))
  (mail-source-directory (expand-create-directory-name "Mail" gnus-home-directory)))

(use-package youtube-dl
  :custom
  (youtube-dl-directory (expand-create-directory-name (getenv "XDG_DOWNLOAD_DIR"))))

;; Use only encrypted authinfo
(customize-set-variable 'auth-sources `(,(expand-file-name "authinfo.gpg" user-emacs-directory)))
(customize-set-variable 'auth-source-gpg-encrypt-to '("Morgan.J.Smith@outlook.com"))
;;; Sensible Default Section Ends

(use-package auth-source-pass
  :straight nil
  :custom (auth-source-pass-filename (expand-file-name "password-store" (getenv "XDG_DATA_HOME")))
  :config (auth-source-pass-enable))

(use-package auth-source-xoauth2
  :straight (auth-source-xoauth2 :type git :host github :repo "ccrusius/auth-source-xoauth2")
  :after smtpmail
  :config
  (defun my-xoauth2-get-secrets (host user port)
    (when (string= user (auth-source-pass-get 'secret "email/work/address"))
      (list
       :token-url "https://accounts.google.com/o/oauth2/token"
       :client-id (auth-source-pass-get 'secret "email/work/client-id")
       :client-secret (auth-source-pass-get 'secret "email/work/client-secret")
       :refresh-token (auth-source-pass-get 'secret "email/work/refresh-token"))))
  (setq auth-source-xoauth2-creds 'my-xoauth2-get-secrets)

  (add-to-list 'smtpmail-auth-supported 'xoauth2)
  (auth-source-xoauth2-enable))

(use-package flyspell
  :straight nil
  :hook
  ((prog-mode . flyspell-prog-mode)
   ((text-mode text-mode message-mode) . flyspell-mode)))

(setq disabled-command-function nil)

(use-package vlf)

;; No mouse gang
(mouse-avoidance-mode 'banish)

(use-package org
  :straight nil
  :custom (org-log-done 'time))

(use-package gdb-mi
  :straight nil
  :custom (gdb-many-windows t))

(use-package pinentry
  :custom
  (epg-pinentry-mode 'loopback)
  (epg-gpg-home-directory (getenv "GNUPGHOME"))
  :config
  (setenv "INSIDE_EMACS" emacs-version)
  (pinentry-start))

(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'executable)
  (plantuml-indent-level 4)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal plantuml-mode-map (leader "c") #'plantuml-preview))
  :mode ("\\.uml\\'" . plantuml-mode))

(use-package flycheck-plantuml
  :after (flycheck plantuml)
  :config (flycheck-plantuml-setup))

;; Backups and auto-saves and deleting
(use-package files
  :straight nil
  :custom
  (backup-directory-alist `((".*" . ,(expand-create-directory-name "backups" user-emacs-directory))))
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))
  (trash-directory (expand-create-directory-name "trash" user-emacs-directory))
  (make-backup-files t)
  (backup-by-copying t)
  (version-control t)
  (vc-make-backup-files t)
  (delete-old-versions -1)
  (auto-save-default t)
  (auto-save-timeout 20)
  (auto-save-interval 200)
  (delete-by-moving-to-trash t))

(customize-set-variable 'custom-file (expand-file-name "custom-garbage" trash-directory) "Goodbye Custom")

(customize-set-variable 'load-prefer-newer t)
(use-package auto-compile
  :config (auto-compile-on-load-mode))

(use-package ledger-mode
  :custom
  (ledger-reports
    '(("mon" "%(binary) -f %(ledger-file) bal -p \"this month\"")
      ("last mon" "%(binary) -f %(ledger-file) bal -p \"last month\"")
      ("bal" "%(binary) -f %(ledger-file) bal")
      ("reg" "%(binary) -f %(ledger-file) reg")
      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
      ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  :mode ("\\.ledger\\'" . ledger-mode))

(use-package evil-ledger
  :after (ledger-mode evil)
  :config
  (evil-define-key 'normal ledger-mode-map (leader "c") #'ledger-report))

(use-package flycheck-ledger
  :after (ledger-mode flycheck))

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :config (pdf-tools-install t))

(use-package nov
  :custom (nov-text-width 80)
  :mode ("\\.epub\\'" . nov-mode))

(use-package eshell
  :straight nil
  :custom
  (eshell-history-size nil "Pull history size from environment variables")
  (eshell-history-file-name nil "Pull history file from environment variables")
  :config
  (setenv "PAGER" (executable-find "cat")))

(use-package dired
  :straight nil
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-listing-switches "-aFhl")
  :hook (dired-mode . dired-hide-details-mode))

(use-package all-the-icons
  :custom (inhibit-compacting-font-caches t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :delight)

(use-package persistent-scratch
  :config (persistent-scratch-setup-default)
  :delight)

(use-package tramp
  :straight nil
  :custom (tramp-default-method "ssh"))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :delight)

(use-package minibuffer
  :straight nil
  :custom
  (read-buffer-completion-ignore-case t)
  (completion-cycle-threshold 3))

;;; Programming Section Begins
(use-package xcscope
  :config (cscope-setup))

;; Save all buffers on compile automatically
(customize-set-variable 'compilation-ask-about-save nil)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(customize-set-variable 'c-basic-offset 4)
(semantic-mode 1)

;; TODO: add evil bindings
(use-package ascii-table
  :commands ascii-table)

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :custom (highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package flycheck
  :custom (flycheck-emacs-lisp-load-path 'inherit)
  :init (global-flycheck-mode))

(use-package eldoc-eval
  :config (eldoc-in-minibuffer-mode 1)
  :delight eldoc-mode)
;;; Programming Section Ends


;;; Modeline Section Begins
(use-package time
  :straight nil
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-day-and-date t)
  :config
  (display-time-mode))

(use-package fancy-battery
  :init (setq fancy-battery-show-percentage t)
  :config (fancy-battery-mode))

(column-number-mode)
(line-number-mode)

(use-package doom-modeline
  :custom
  (doom-modeline-gnus t)
  (doom-modeline-gnus-excluded-groups '("nnimap+morganjsmith:emacs"))
  (doom-modeline-irc nil)
  :init (doom-modeline-mode 1))
;;; Modeline Section Ends


;;; Whitespace Section Begins
(customize-set-variable 'tab-width 4)
(customize-set-variable 'indent-tabs-mode nil)

(use-package ws-butler
  :config (ws-butler-global-mode)
  :delight)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'message-mode-hook 'turn-on-auto-fill)
;;; Whitespace Section Ends


;;; GNU/Linux Section Begins
(when IS-LINUX
  (use-package vterm
    :commands (vterm vterm-other-window)))
;;; GNU/Linux Section Ends


;;; GNU/Linux and BSD Section Begins
(when (or IS-LINUX IS-BSD)

(use-package transmission
  :commands 'transmission)

(use-package exwm
  :custom
  (exwm-workspace-show-all-buffers t)
  (exwm-layout-show-all-buffers t)
  (exwm-manage-force-tiling t)
  (exwm-workspace-number 9)

  (exwm-input-global-keys
   `(
     ;; Lock
     ([?\s-x] . ,(lambda () (interactive) (shell-command "slock")))
     ;; Music/Media bindings
     ([?\s-p] . ,(lambda () (interactive) (shell-command "mpc toggle")))
     (,(kbd "<s-up>") . ,(lambda () (interactive) (shell-command "amixer set Master 5%+")))
     (,(kbd "<s-down>") . ,(lambda () (interactive) (shell-command "amixer set Master 5%-")))
     (,(kbd "<s-right>") . ,(lambda () (interactive) (shell-command "mpc next")))
     (,(kbd "<s-left>") . ,(lambda () (interactive) (shell-command "mpc prev")))
     ;; Reset (to line-mode).
     ([?\s-r] . exwm-reset)
     ;; Switch focus.
     ([?\s-j] . other-window)
     ([?\s-k] . ,(lambda () (interactive) (other-window -1)))
     ;; Split window.
     ([?\s-\\] . split-window-horizontally)
     ([?\s-\-] . split-window-vertically)
     ;; eshell
     (,(kbd "<s-return>") . eshell)
     ;; Close window (not killing it, just getting it out of view)
     ([?\s-q] . ,(lambda () (interactive) (if (< 1 (count-windows))
                                              (delete-window)
                                            (switch-to-next-buffer))))
     ;; Launch application.
     ([?\s-d] . ,(lambda (command)
                   (interactive (list (read-shell-command "$ ")))
                   (start-process-shell-command command nil command)))))
  :init
  (defun exwm-monitor-update ()
    ;; Update monitor order and add bindings for switching between them
    ;; monitor 0 is the first monitor and is attached to the monitor
    ;; whose X coordinate is 0. Other monitors are attached arbitrarily
    (let ((monitor-number 1)
          (value nil))

      (dolist (monitor (display-monitor-attributes-list) value)
        (push (alist-get 'name monitor) value)
        ;; Monitor 0 if geometry == 0, else just use an incremented number
        (if (equal (car (alist-get 'geometry monitor)) 0)
            (push 0 value)
          (progn
            (push monitor-number value)
            (setq monitor-number (1+ monitor-number)))))

      (setq exwm-randr-workspace-monitor-plist value)

      (mapcar (lambda (binding)
                (exwm-input-set-key (car binding) (cdr binding)))
              ;; Switch to certain workspace.
              (mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          ,(lambda ()
                             (interactive)
                             (exwm-workspace-switch-create (1- i)))))
                      (number-sequence 1 9)))))
  :config
  (require 'exwm-config)

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
    (lambda ()
      (exwm-workspace-rename-buffer exwm-class-name)))

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (require 'exwm-randr)

  (exwm-monitor-update)

  (add-hook 'exwm-randr-screen-change-hook #'exwm-monitor-update)

  (exwm-randr-enable)
  (exwm-config-ido)
  (exwm-enable)))
;;; GNU/Linux and BSD Section Ends


;;; Parens Section Begins
(use-package paren
  :straight nil
  :custom
  (show-paren-delay 0)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-1-face   ((t (:foreground "white"))))
  (rainbow-delimiters-depth-2-face   ((t (:foreground "cyan"))))
  (rainbow-delimiters-depth-3-face   ((t (:foreground "yellow"))))
  (rainbow-delimiters-depth-4-face   ((t (:foreground "green"))))
  (rainbow-delimiters-depth-5-face   ((t (:foreground "orange"))))
  (rainbow-delimiters-depth-6-face   ((t (:foreground "purple"))))
  (rainbow-delimiters-depth-7-face   ((t (:foreground "white"))))
  (rainbow-delimiters-depth-8-face   ((t (:foreground "cyan"))))
  (rainbow-delimiters-depth-9-face   ((t (:foreground "yellow"))))
  (rainbow-delimiters-unmatched-face ((t (:foreground "red")))))
;;; Parens Section Ends


;;; VC/Diffs Section Begins
(use-package magit
  :custom
  (magit-no-confirm '(safe-with-wip))
  (magit-wip-merge-branch t)
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask)
  (magit-auto-revert-immediately t)
  :config
  (magit-wip-mode 1)

  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source)
  :delight magit-wip-mode)

(use-package magit-repos
  :straight nil
  :after magit
  :commands magit-list-repositories
  :custom
  (magit-repository-directories
   `(("~/repos" . 1)
     (,(expand-create-directory-name "straight/repos" user-emacs-directory) . 1)
     ("~" . 0))))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package ediff
  :straight nil
  :commands ediff
  :custom
  (ediff-diff-options "-w")
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))
;;; VC/Diffs Section Ends


;;; EWW Section Begins
(use-package shr
  :straight nil
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (shr-use-colors nil)
  (shr-max-image-proportion 0.5))

(use-package shrface
  :straight (shrface :type git :host github :repo "chenyanming/shrface")
  :config
  (require 'shrface)
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t)
  (with-eval-after-load 'eww
    (add-hook 'eww-after-render-hook #'shrface-mode)
    (with-eval-after-load 'evil
      (evil-define-key '(normal) eww-mode-map
        (kbd "<tab>") 'org-cycle
        (kbd "<S-tab>") 'org-shifttab
        (kbd "C-j") 'outline-next-visible-heading
        (kbd "C-k") 'outline-previous-visible-heading)))
  (with-eval-after-load 'nov
    ;; reset nov-shr-rendering-functions, in case the list get bigger and bigger
    (setq nov-shr-rendering-functions '((img . nov-render-img)
                                        (title . nov-render-title)))
    (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
    (add-hook 'nov-mode-hook 'shrface-mode)
    (with-eval-after-load 'evil
      (evil-define-key '(normal) nov-mode-map
        (kbd "<tab>") 'org-cycle
        (kbd "<S-tab>") 'org-shifttab
        (kbd "C-j") 'outline-next-visible-heading
        (kbd "C-k") 'outline-previous-visible-heading))))

(use-package shr-tag-pre-highlight
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))
;;; EWW Section Ends


;;; Auto-complete/Hints Section Begins
(use-package ido
  :straight nil
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  :config
  (ido-mode 1))

(use-package company
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.3)
  (company-show-numbers t)
  :config
  (global-company-mode)
  :delight)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :hook (global-company-mode . company-quickhelp-mode))

(use-package which-key
  :custom (which-key-idle-secondary-delay 0.05)
  :config (which-key-mode)
  :delight)
;;; Auto-complete/Hints Section Ends

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)))

;;; Evil Section Begins
(defun leader (key)
  "Add the leader key on front of KEY."
  (kbd (concat "SPC " key)))

(use-package evil
  :custom
  (evil-ex-complete-emacs-commands t)
  (evil-want-integration t)
  (evil-want-keybinding nil)

  :config
  (evil-mode t)
  (evil-define-key '(normal insert) 'global (kbd "M-j") #'evil-scroll-line-down)
  (evil-define-key '(normal insert) 'global (kbd "M-k") #'evil-scroll-line-up)
  (evil-define-key '(normal insert) 'global (kbd "M-J") #'text-scale-decrease)
  (evil-define-key '(normal insert) 'global (kbd "M-K") #'text-scale-increase)

  (evil-define-key 'normal 'global (kbd "C-u") #'evil-scroll-up)
  (evil-define-key 'normal 'global (kbd "C-d") #'evil-scroll-down)

  (evil-define-key 'visual 'global (leader "c") #'comment-or-uncomment-region)

  (evil-define-key '(normal visual) 'global (leader "o") #'ispell)

  (evil-define-key 'normal 'global (leader "TAB") #'whitespace-mode)
  (evil-define-key 'normal 'global (leader "c")   #'compile)
  (evil-define-key 'normal 'global (leader "g")   #'magit-status)
  (evil-define-key 'normal 'global (leader "e") (lambda () (interactive) (find-file (locate-user-emacs-file "init.el")))))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-magit
  :after (evil magit))
;;; Evil Section Ends

(use-package gcmh
  :config (gcmh-mode t)
  :delight)

(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ascii characters."))))

(defun root-edit ()
  "Open current file as root."
  (interactive)
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name (buffer-file-name)))))
    (find-file tramp-file-name)))


(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (when completion-in-region-mode
           (completion-in-region-mode -1))
         (let ((debug-on-quit nil))
           (signal 'quit nil)))))

(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

(provide 'init.el)
;;; init.el ends here
