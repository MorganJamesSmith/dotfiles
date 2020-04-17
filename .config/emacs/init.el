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
;;; Optimization Section Ends

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

(use-package delight)

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

;;; Pretty Visuals Section Ends


;;; Sensible Default Section Begins
(global-auto-revert-mode t)

;; Replace the info command with something more useful
(global-set-key (kbd "C-h i") 'info-display-manual)

(fset #'yes-or-no-p #'y-or-n-p)

;; Use only encrypted authinfo
(customize-set-variable 'auth-sources `(,(expand-file-name "authinfo.gpg" user-emacs-directory)))
;;; Sensible Default Section Ends


(use-package flyspell
  :straight nil
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))

(customize-set-variable 'browse-url-browser-function 'eww-browse-url)

(setq disabled-command-function nil)

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
  (eval-after-load 'evil
    (evil-define-key 'normal plantuml-mode-map (leader "c") #'plantuml-preview))
  :mode ("\\.uml\\'" . plantuml-mode))

;; Backups and auto-saves and deleting
(let ((backup-directory (expand-file-name "backups" user-emacs-directory))
      (auto-save-directory (expand-file-name "auto-save-list" user-emacs-directory))
      (recycle-bin-directory (expand-file-name "trash" user-emacs-directory)))
  (unless (file-exists-p backup-directory)
    (make-directory backup-directory t))
  (unless (file-exists-p auto-save-directory)
    (make-directory auto-save-directory t))
  (unless (file-exists-p recycle-bin-directory)
    (make-directory recycle-bin-directory t))
  (customize-set-variable 'make-backup-files t)
  (customize-set-variable 'backup-directory-alist `((".*" . ,backup-directory)))
  (customize-set-variable 'backup-by-copying t)
  (customize-set-variable 'version-control t)
  (customize-set-variable 'vc-make-backup-files t)
  (customize-set-variable 'delete-old-versions -1)
  (customize-set-variable 'auto-save-default t)
  (customize-set-variable 'auto-save-timeout 20)
  (customize-set-variable 'auto-save-interval 200)
  (customize-set-variable 'auto-save-file-name-transforms `((".*" ,(file-name-as-directory auto-save-directory) t)))
  (customize-set-variable 'delete-by-moving-to-trash t)
  (customize-set-variable 'trash-directory recycle-bin-directory))

(customize-set-variable 'custom-file (expand-file-name "./custom-garbage" trash-directory) "Goodbye Custom")

(customize-set-variable 'load-prefer-newer t)
(use-package auto-compile
  :config (auto-compile-on-load-mode))

(use-package ledger-mode
  :config
  (eval-after-load 'evil
    (evil-define-key 'normal ledger-mode-map (leader "c") #'ledger-report))
  :custom
  (ledger-reports
    '(("mon" "%(binary) -f %(ledger-file) bal -p \"this month\"")
      ("last mon" "%(binary) -f %(ledger-file) bal -p \"last month\"")
      ("bal" "%(binary) -f %(ledger-file) bal")
      ("reg" "%(binary) -f %(ledger-file) reg")
      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
      ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  :mode ("\\.ledger\\'" . ledger-mode))

(use-package pdf-tools
  :config (pdf-tools-install))

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
  :config
  (cscope-setup))

;; Save all buffers on compile automatically
(customize-set-variable 'compilation-ask-about-save nil)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(customize-set-variable 'c-basic-offset 4)
(semantic-mode 1)

;; TODO: add evil bindings
(use-package ascii-table)

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :custom (highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package flycheck
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
;;; Modeline Section Ends


;;; Whitespace Section Begins
(customize-set-variable 'tab-width 4)
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'sentence-end-double-space nil)
(customize-set-variable 'require-final-newline t)

(use-package ws-butler
  :config (ws-butler-global-mode)
  :delight)
;;; Whitespace Section Ends


;;; GNU/Linux Section Begins
(when IS-LINUX
  (use-package vterm))
;;; GNU/Linux Section Ends


;;; GNU/Linux and BSD Section Begins
(when (or IS-LINUX IS-BSD)

(use-package transmission
  :commands 'transmission)

(use-package exwm
  :custom
  (exwm-workspace-show-all-buffers t)
  (exwm-layout-show-all-buffers t)
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
            'magit-process-password-auth-source))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package ediff
  :straight nil
  :commands 'ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))
;;; VC/Diffs Section Ends


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
  :hook (eshell-mode . (lambda ()
                         (set (make-local-variable 'company-backends)
                              '((company-capf)))))
  :delight)

(use-package which-key
  :custom (which-key-idle-secondary-delay 0.05)
  :config (which-key-mode)
  :delight)

(use-package pcomplete-extension
  :functions pcomplete/doas
  :config (require 'pcomplete-extension)
  (fset #'pcomplete/doas #'pcomplete/sudo))
;;; Auto-complete/Hints Section Ends


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
  :after evil
  :config (require 'evil-magit))
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
