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
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Disable bidirectional text rendering
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Do not render cursors or regions in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq frame-inhibit-implied-resize t)

(setq auto-mode-case-fold nil)
;;; Optimization Section Ends


;; Enable MELPA
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

; Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(use-package delight)

;;; Pretty Visuals Section Begins
(use-package modus-vivendi-theme
  :config (load-theme 'modus-vivendi t))

(setq-default indicate-buffer-boundaries nil
              indicate-empty-lines nil)

(global-hl-line-mode t)

(setq echo-keystrokes 0.02)

;; I dislike gui stuff
(setq use-file-dialog nil
      use-dialog-box nil
      visible-bell t)

;;; Pretty Visuals Section Ends


;;; Sensible Default Section Begins
(global-auto-revert-mode t)

(fset #'yes-or-no-p #'y-or-n-p)

;; Use only encrypted authinfo
(setq auth-sources `(,(expand-file-name "authinfo.gpg" user-emacs-directory)))
;;; Sensible Default Section Ends


(use-package flyspell
  :ensure nil
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))

(setq browse-url-browser-function 'eww-browse-url)

(setq disabled-command-function nil)

(use-package pinentry
  :config
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start))

(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'executable
        plantuml-indent-level 4)
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
  (setq make-backup-files t
        backup-directory-alist `((".*" . ,backup-directory))
        backup-by-copying t
        version-control t
        vc-make-backup-files t
        delete-old-versions -1
        auto-save-default t
        auto-save-timeout 20
        auto-save-interval 200
        auto-save-file-name-transforms `((".*" ,(file-name-as-directory auto-save-directory) t))
        delete-by-moving-to-trash t
        trash-directory recycle-bin-directory))

(setq custom-file (expand-file-name "./custom-garbage" trash-directory)) ; I don't use custom

(setq load-prefer-newer t)
(use-package auto-compile
  :config (auto-compile-on-load-mode))

(use-package ledger-mode
  :config
  (eval-after-load 'evil
    (evil-define-key 'normal ledger-mode-map (leader "c") #'ledger-report))

  (setq ledger-reports
    '(("mon" "%(binary) -f %(ledger-file) bal -p \"this month\"")
      ("last mon" "%(binary) -f %(ledger-file) bal -p \"last month\"")
      ("bal" "%(binary) -f %(ledger-file) bal")
      ("reg" "%(binary) -f %(ledger-file) reg")
      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
      ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  :mode ("\\.ledger\\'" . ledger-mode))

(use-package nov
  :init (setq nov-text-width 80)
  :mode ("\\.epub\\'" . nov-mode))

(use-package eshell
  :ensure nil
  :config (setq eshell-history-size nil
                eshell-history-file-name nil))

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-aFhl")
  :hook (dired-mode . dired-hide-details-mode))

(use-package all-the-icons
  :init (setq inhibit-compacting-font-caches t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :delight)

(use-package tramp
  :ensure nil
  :config (setq tramp-default-method "ssh"))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :delight)

(use-package minibuffer
  :ensure nil
  :config
  (setq read-buffer-completion-ignore-case t)
  (setq completion-cycle-threshold 3))

;;; Programming Section Begins
(use-package xcscope
  :config
  (cscope-setup))

;; Save all buffers on compile automatically
(setq compilation-ask-about-save nil)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq-default c-basic-offset 4)
(semantic-mode 1)

;; TODO: add evil bindings
(use-package ascii-table)

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package eldoc-eval
  :config (eldoc-in-minibuffer-mode 1)
  :delight eldoc-mode)
;;; Programming Section Ends


;;; Modeline Section Begins
(use-package time
  :ensure nil
  :config
  (setq display-time-default-load-average nil
        display-time-24hr-format t
        display-time-day-and-date t)
  (display-time-mode))

(use-package fancy-battery
  :init (setq fancy-battery-show-percentage t)
  :config (fancy-battery-mode))

(column-number-mode)
(line-number-mode)
;;; Modeline Section Ends


;;; Whitespace Section Begins
(setq-default tab-width 4
              indent-tabs-mode nil
              sentence-end-double-space nil)
(setq require-final-newline t)

(use-package ws-butler
  :config (ws-butler-global-mode)
  :delight)
;;; Whitespace Section Ends


;;; Windows Section Begins
(when IS-WINDOWS

; cygwin support
(let* ((cygwin-root "c:/cygwin64")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (file-readable-p cygwin-root)

    (setq null-device "/dev/null")

    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))

    ;; NT-Emacs assumes a Windows shell. Change to bash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)

    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)))

(setq w32-lwindow-modifier 'super)
(w32-register-hot-key [s-j])
(w32-register-hot-key [s-k])
(global-set-key (kbd "s-j") 'other-frame)
(global-set-key (kbd "s-k") 'other-frame)
)
;;; Windows Section Ends


;;; GNU/Linux Section Begins
(when IS-LINUX
  (use-package vterm))
;;; GNU/Linux Section Ends


;;; GNU/Linux and BSD Section Begins
(when (or IS-LINUX IS-BSD)

(use-package transmission
  :commands 'transmission)

(use-package exwm
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

      (require 'exwm-randr)
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
  (setq exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
    (lambda ()
      (exwm-workspace-rename-buffer exwm-class-name)))
  (setq exwm-input-global-keys
    `(
    ;; Lock
    ([?\s-x] . ,(lambda () (interactive) (shell-command "slock")))
    ;; Music/Media bindings
    ([?\s-p] . ,(lambda () (interactive) (shell-command "mpc toggle")))
    (,(kbd "<s-up>") . ,(lambda () (interactive) (shell-command "amixer set Master 5%+")))
    (,(kbd "<s-down>") . ,(lambda () (interactive) (shell-command "amixer set Master 5%-")))
    (,(kbd "<s-right>") . ,(lambda () (interactive) (shell-command "mpc next")))
    (,(kbd "<s-left>") . ,(lambda () (interactive) (shell-command "mpc prev")))
    ;; buffer switching
    (,(kbd "<s-tab>") . ,(lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer)))))
    ([?\s-.] . switch-to-next-buffer)
    ([?\s-,] . switch-to-prev-buffer)
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
(require 'paren)
(setq show-paren-delay 0
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(show-paren-mode t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :init
  (require 'rainbow-delimiters)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "white")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "cyan")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "yellow")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "green")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "orange")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "purple")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "white")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "cyan")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "yellow")
  (set-face-foreground 'rainbow-delimiters-unmatched-face "red"))
;;; Parens Section Ends


;;; VC/Diffs Section Begins
(use-package magit)

(use-package diff-hl
            :config (global-diff-hl-mode))

(use-package ediff
  :ensure nil
  :commands 'ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))
;;; VC/Diffs Section Ends


;;; Auto-complete/Hints Section Begins
(require 'ido)
(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)

(use-package company
  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-idle-delay 0.3
        company-show-numbers t)

  :hook (eshell-mode . (lambda ()
                         (set (make-local-variable 'company-backends)
                              '((company-capf)))))
  :delight)

(use-package which-key
  :config
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
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
  :init
  (setq evil-ex-complete-emacs-commands t
        evil-want-integration t
        evil-want-keybinding nil)

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
  (evil-define-key 'normal 'global (leader "c") #'compile)
  (evil-define-key 'normal 'global (leader "g") #'magit-status)
  (evil-define-key 'normal 'global (leader "e") (lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el")))))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-magit
  :after evil
  :config (require 'evil-magit))
;;; Evil Section Ends

(use-package gcmh
  :config (gcmh-mode t))

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
