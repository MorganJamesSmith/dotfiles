;;; .emacs --- Emacs configuration file by Morgan Smith
;;

;; Add and enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;; Packages cloned from version control
(defun git-package (package-name url)
  "Clone a git repo to ~/.emacs.d/PACKAGE-NAME and add it to the load path"
  (unless (file-exists-p (concat "~/.emacs.d/" package-name))
    (shell-command (concat "git clone " url " ~/.emacs.d/" package-name)))
  (eval-when-compile
    (add-to-list 'load-path (concat "~/.emacs.d/" package-name))))

;; use-package
(git-package "use-package" "https://github.com/jwiegley/use-package")
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t) ; Always download all my packages

;; youtube-dl
(git-package "youtube-dl" "https://github.com/skeeto/youtube-dl-emacs.git")
(require 'youtube-dl)

;; nuke-buffers
(git-package "nuke-buffers" "https://github.com/davep/nuke-buffers.el.git")
(require 'nuke-buffers)
(push "vterm" nuke-buffers-ignore)


;; Backups
(defvar backup-directory "~/.backups")
(unless (file-exists-p backup-directory)
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

(setq custom-file "/dev/null") ; I don't like custom

(setq browse-url-browser-function 'eww-browse-url)

(require 'cl)

(server-start)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

(show-paren-mode t)

;; Time
(setq display-time-default-load-average nil
      display-time-24hr-format t
      display-time-day-and-date t)
(display-time-mode)

;; Battery
(use-package fancy-battery
  :init (setq fancy-battery-show-percentage t)
  :config (fancy-battery-mode))

(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)

(setq tramp-default-method "ssh")


(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq-default display-line-numbers 'relative)

(global-hl-line-mode t)
(global-auto-revert-mode t)

(when (display-graphic-p)
  (tooltip-mode -1))

;; This alias doesn't have the usual overhead of an alias
;; This is because I manually tell the byte-compiler to inline it
(defalias 'yes-or-no-p 'y-or-n-p)
(put 'yes-or-no-p 'byte-optimizer 'byte-compile-inline-expand)


;; Whitespace configurations
(setq-default tab-width 8
              indent-tabs-mode nil)
(setq require-final-newline t)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(setq-default sentence-end-double-space nil)

(set-terminal-coding-system 'us-ascii)
(set-keyboard-coding-system 'us-ascii)
(prefer-coding-system 'us-ascii)

(use-package vterm)

(use-package exwm
  :config
  (require 'exwm-config)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
    (lambda ()
      (exwm-workspace-rename-buffer exwm-class-name)))
  (setq exwm-input-global-keys
    `(
    ;; Lock
    ([?\s-x] . (lambda () (interactive) (shell-command "slock")))
    ;; Music/Media bindings
    ([?\s-p] . (lambda () (interactive) (shell-command "mpc toggle")))
    (,(kbd "<s-up>") . (lambda () (interactive) (shell-command "pulsemixer --change-volume +5 --get-volume")))
    (,(kbd "<s-down>") . (lambda () (interactive) (shell-command "pulsemixer --change-volume -5 --get-volume")))
    (,(kbd "<s-right>") . (lambda () (interactive) (shell-command "mpc next")))
    (,(kbd "<s-left>") . (lambda () (interactive) (shell-command "mpc prev")))
    ;; buffer switching
    (,(kbd "<s-tab>") . (lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) t))))
    ([?\s-.] . switch-to-next-buffer)
    ([?\s-,] . switch-to-prev-buffer)
    ;; Reset (to line-mode).
    ([?\s-r] . exwm-reset)
    ;; Switch workspace.
    ([?\s-w] . exwm-workspace-switch)
    ;; Switch focus.
    ([?\s-j] . other-window)
    ([?\s-k] . (lambda () (interactive) (other-window -1)))
    ;; vterm
    (,(kbd "<s-return>") . (lambda () (interactive) (if (get-buffer "vterm") (switch-to-buffer "vterm") (vterm))))
    ;; Close winow
    ([?\s-q] . (lambda () (interactive) (if (< 1 (count-windows))
                                       (delete-window)
                                     (exwm-workspace-delete))))
    ;; Launch application.
    ([?\s-d] . (lambda (command)
                 (interactive (list (read-shell-command "$ ")))
                   (start-process-shell-command command nil command)))
    ;; Switch to certain workspace.
    ,@(mapcar (lambda (i)
                `(,(kbd (format "s-%d" i)) .
                   (lambda ()
                   (interactive)
                   (exwm-workspace-switch-create ,i))))
        (number-sequence 0 9))))

  (exwm-config-ido)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (require 'exwm-randr)
  (let ((monitor-number 0)
        (value nil))

    (dolist (monitor (display-monitor-attributes-list) value)
      (push (alist-get 'name monitor) value)
      (push monitor-number value)
      (setq monitor-number (1+ monitor-number)))

    (setq exwm-randr-workspace-output-plist value))
  (exwm-randr-enable)

  (exwm-enable))

;; Programming stuff
(use-package flycheck)

(use-package xcscope
  :config (cscope-setup))

(use-package smartparens
  :init
  (require 'dash)
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t))

(use-package rainbow-delimiters
  :hook 'lisp-mode-hook
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
  (set-face-foreground 'rainbow-delimiters-unmatched-face "red")
  :config (rainbow-delimiters-mode-enable))

(use-package magit)

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package undo-tree)

;; Pretty stuff
(use-package nyan-mode
  :config (nyan-mode))

(use-package powerline
  :config (powerline-default-theme))

(load-theme 'tsdh-dark)


;; Keybinding stuff
(use-package evil
  :init
  (setq evil-ex-complete-emacs-commands t
        evil-want-integration t
        evil-want-keybinding nil)

  :config
  (evil-mode 1)
  (defun leader (key)
    (kbd (concat "SPC " key)))

  (evil-define-key 'normal 'global (kbd "M-j") 'evil-scroll-line-down)
  (evil-define-key 'normal 'global (kbd "M-k") 'evil-scroll-line-up)
  (evil-define-key 'normal 'global (kbd "M-J") 'text-scale-decrease)
  (evil-define-key 'normal 'global (kbd "M-K") 'text-scale-increase)

  (evil-define-key 'visual 'global (leader "c") 'comment-or-uncomment-region)

  (evil-define-key 'normal 'global (leader "TAB") 'whitespace-mode)
  (evil-define-key 'normal 'global (leader "o") 'ispell))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-magit
  :after evil-collection
  :config (require 'evil-magit))

(use-package vertigo
  :init
  (setq vertigo-cut-off 9)
  (evil-define-key 'normal 'global (leader "j") 'vertigo-jump-down)
  (evil-define-key 'normal 'global (leader "k") 'vertigo-jump-up))


(use-package ledger-mode
  :config
  (evil-define-key 'normal ledger-mode-map (leader "r") 'ledger-report)

  (setq ledger-reports
    '(("mon" "%(binary) -f %(ledger-file) bal -p \"this month\"")
      ("bal" "%(binary) -f %(ledger-file) bal")
      ("reg" "%(binary) -f %(ledger-file) reg")
      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
      ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package fzf
  :if (eq 0 (shell-command "command -v fzf &> /dev/null")))

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

(provide '.emacs)
;;; .emacs ends here
