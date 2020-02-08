;;; .emacs --- Emacs configuration file by Morgan Smith
;;

(setq debug-on-error t)

;; Determine if we're connected to the internet
(setq internet-up-p
    (call-process "ping" nil nil nil "-c" "1" "-W" "1" "www.fsf.org"))

;; Enable MELPA
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(require 'use-package-ensure)
;; Download all my packages given I have internet
(if internet-up-p (setq use-package-always-ensure t))


(setq load-prefer-newer t)
(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; Packages cloned from version control
(defun git-package (package-name url &optional update internet-up)
  "Clone a git repo to ~/.emacs.d/PACKAGE-NAME and add it to the load path
If UPDATE is non-nil, a git pull will be performed"
  (let ((package-path (expand-file-name(concat "~/.emacs.d/" package-name))))
    (catch 'unable-to-get-package
      (unless (file-exists-p package-path)
        (if internet-up
            (shell-command (concat "git clone " url " " package-path))
          (throw 'unable-to-get-package)))
      (add-to-list 'load-path package-path)
      (if (and update internet-up)
          (shell-command (concat "cd " package-path "; git pull"))))))

;; youtube-dl
(git-package "youtube-dl" "https://github.com/skeeto/youtube-dl-emacs.git" t internet-up-p)
(require 'youtube-dl)

;; nuke-buffers
(git-package "nuke-buffers" "https://github.com/davep/nuke-buffers.el.git" t internet-up-p)
(require 'nuke-buffers)
(push "*eshell*" nuke-buffers-ignore)


;; Backups and auto-saves
(let ((backup-directory (expand-file-name "~/.emacs.d/backups"))
      (auto-save-directory (expand-file-name "~/.emacs.d/auto-save-list")))
  (unless (file-exists-p backup-directory)
    (make-directory backup-directory t))
  (unless (file-exists-p auto-save-directory)
    (make-directory auto-save-directory t))
  (setq make-backup-files t
        backup-directory-alist `((".*" . ,backup-directory))
        backup-by-copying t
        version-control t
        vc-make-backup-files t
        delete-old-versions -1
        auto-save-default t
        auto-save-timeout 20
        auto-save-interval 200
        auto-save-file-name-transforms `((".*" ,auto-save-directory t))))

(setq browse-url-browser-function 'eww-browse-url)

(server-start)

;; Visual stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

(setq show-paren-delay 0)
(show-paren-mode t)

(use-package all-the-icons
  :init (setq inhibit-compacting-font-caches t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

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
(column-number-mode)
(line-number-mode)

(global-hl-line-mode t)
(global-auto-revert-mode t)

(when (display-graphic-p)
  (tooltip-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq epg-pinentry-mode 'loopback)

;; Whitespace configurations
(setq-default tab-width 8
              indent-tabs-mode nil)
(setq require-final-newline t)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(setq-default sentence-end-double-space nil)

;; Windows Specific Configurations
(if (string= system-type "windows-nt")
    (progn

(setq custom-file "nul") ; I don't like custom
))
;; End of Windows Specific Configurations


;; GNU/Linux Specific Configurations
(if (string= system-type "gnu/linux")
    (progn

(setq custom-file "/dev/null") ; I don't like custom

(use-package transmission)

(use-package exwm
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
    ;; Split window.
    ([?\s-\\] . split-window-horizontally)
    ([?\s-\-] . split-window-vertically)
    ;; eshell
    (,(kbd "<s-return>") . eshell)
    ;; Close window (not killing it, just getting it out of view)
    ([?\s-q] . (lambda () (interactive) (if (< 1 (count-windows))
                                       (delete-window)
                                       (switch-to-next-buffer))))
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

))
;; End of GNU/Linux Specific Configurations

(use-package smartparens
  :init
  (require 'dash)
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (sp-use-smartparens-bindings))

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

(use-package magit)

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package eshell
  :ensure nil
  :init (add-hook 'eshell-mode-hook (lambda () (setq display-line-numbers nil))))

(use-package pcomplete-extension
  :config (require 'pcomplete-extension))

(use-package company
  :config (global-company-mode)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-capf)))))

  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-idle-delay 0.1)
  ;; Use builtin faces instead of ugly ones set by company
  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview :weight bold))))
   '(company-tooltip
     ((t (:inherit popup-face))))
   '(company-scrollbar-bg
     ((t (:inherit popup-scroll-bar-background-face))))
   '(company-scrollbar-fg
     ((t (:inherit popup-scroll-bar-foreground-face))))
   '(company-tooltip-selection
     ((t (:inherit popup-menu-selection-face))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection))))))


(use-package undo-tree
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

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

  (evil-define-key '(normal insert) 'global (kbd "M-j") 'evil-scroll-line-down)
  (evil-define-key '(normal insert) 'global (kbd "M-k") 'evil-scroll-line-up)
  (evil-define-key '(normal insert) 'global (kbd "M-J") 'text-scale-decrease)
  (evil-define-key '(normal insert) 'global (kbd "M-K") 'text-scale-increase)

  (evil-define-key 'visual 'global (leader "c") 'comment-or-uncomment-region)

  (evil-define-key 'normal 'global (leader "TAB") 'whitespace-mode)
  (evil-define-key 'normal 'global (leader "o") 'ispell)
  (evil-define-key 'normal 'global (leader "g") 'magit-status)
  (evil-define-key 'normal 'global (leader "e") (lambda () (interactive) (find-file (expand-file-name "~/.emacs")))))

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
  :init (setq nov-text-width 80)
  :mode ("\\.epub\\'" . nov-mode))

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
  (interactive)
  ;; open current file as root
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name (buffer-file-name)))))
    (find-file tramp-file-name)))

(provide '.emacs)
;;; .emacs ends here
