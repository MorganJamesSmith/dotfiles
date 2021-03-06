;;; init.el --- My personal init file -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Morgan Smith

;;; Commentary:

;; This is my personal Emacs init file.
;; It is crafted with love, care, and stupidity.
;; Use at your own risk.

;;; Code:

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; This is non-nil for instances of Emacs started from within an
;; instance of Emacs
(defconst IS-INSIDE-EMACS   (getenv "INSIDE_EMACS"))

(customize-set-variable 'user-full-name "Morgan Smith")
(customize-set-variable 'user-mail-address "Morgan.J.Smith@outlook.com")

(defun create-directory (dir &optional default-dir)
  "Return DIR as a directory and create DIR if it doesn't already exist.
If DIR is relative, it will be relative to DEFAULT-DIR
If DEFAULT-DIR isn't provided, DIR is relative to ~"
  (unless default-dir
    (setq default-dir "~"))
  (let ((directory (file-name-as-directory (expand-file-name dir default-dir))))
    (unless (file-exists-p directory)
      (make-directory directory t))
    directory))

;; Load all packages upfront
(customize-set-variable 'use-package-always-demand t)

;; Don't be so in my face with issues
(customize-set-variable 'warning-minimum-level :emergency)

;;; Make buffers appear where I want them to
(unless (boundp 'shell-command-buffer-name-async)
  (setq shell-command-buffer-name-async "*Async Shell Command*"))
(customize-set-variable
 'display-buffer-alist
 (list (list shell-command-buffer-name-async   #'display-buffer-no-window)
       (list "*compilation*"                   #'display-buffer-no-window)))
(customize-set-variable 'async-shell-command-buffer 'new-buffer)

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
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Garbage collect at 100Mb
(customize-set-variable 'gc-cons-threshold 100000000)

;; Do not render cursors or regions in non-focused windows.
(customize-set-variable 'cursor-in-non-selected-windows nil)
(customize-set-variable 'highlight-nonselected-windows nil)

(customize-set-variable 'fast-but-imprecise-scrolling t)

(customize-set-variable 'frame-inhibit-implied-resize t)

(customize-set-variable 'auto-mode-case-fold nil)

;; Only matters on multi-user systems
(customize-set-variable 'create-lockfiles nil)

(global-so-long-mode)
;;; Optimization Section Ends


;;; Sensible Defaults Section Begins
(global-auto-revert-mode t)
(customize-set-variable 'revert-without-query '(".*"))

;; Replace the info command with something more useful
(global-set-key (kbd "C-h i") 'info-display-manual)

;; Move gnus folders to the `user-emacs-directory'
(use-package gnus
  :custom
  (gnus-init-file (expand-file-name "gnus" user-emacs-directory))
  (gnus-home-directory (create-directory "gnus-files" user-emacs-directory))
  (gnus-directory (create-directory "News" gnus-home-directory))
  (mail-source-directory (create-directory "Mail" gnus-home-directory)))

;; Use only encrypted authinfo
(customize-set-variable
 'auth-sources
 `((:source ,(expand-file-name "authinfo.gpg" user-emacs-directory))))

(customize-set-variable
 'auth-source-save-behavior t)

(customize-set-variable
 'auth-source-gpg-encrypt-to (list user-mail-address))

(customize-set-variable 'text-quoting-style 'grave)

;; Date should always be big to small (year/month/day)
(customize-set-variable 'calendar-date-style 'iso)

(customize-set-variable 'fill-column 79)
;;; Sensible Defaults Section Ends


;;; Terrible Defaults Section Starts
(customize-set-variable 'use-short-answers t)

(customize-set-variable 'enable-local-variables :all)
;;; Terrible Defaults Section Ends


;;; Pretty Visuals Section Begins
(use-package modus-vivendi-theme
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-completions 'opinionated)
  (modus-themes-headings '((t . rainbow)))
  (modus-themes-mode-line '3d)
  (modus-themes-org-blocks 'rainbow)
  (modus-themes-paren-match 'intense-bold)
  (modus-themes-prompts 'intense)
  (modus-themes-scale-5 2.5)
  (modus-themes-scale-headings t)
  (modus-themes-slanted-constructs t)
  (modus-themes-variable-pitch-headings t)
  :config
  (load-theme 'modus-vivendi t))

;; I dislike gui stuff
(customize-set-variable 'use-file-dialog nil)
(customize-set-variable 'use-dialog-box nil)
(customize-set-variable 'visible-bell t)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
;;; Pretty Visuals Section Ends


;;; Modeline Section Begins
(use-package time
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-day-and-date t)
  :config
  (display-time-mode))

(customize-set-variable 'battery-mode-line-format "[%L %p%%]")
(display-battery-mode)
(size-indication-mode)
(column-number-mode)

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (list left
          (list (format (format "%%%ds" available-width) ""))
          right)))

(customize-set-variable
 'mode-line-format
 '((:eval
    (simple-mode-line-render
     '(""
       "%e"
       mode-line-front-space
       mode-line-mule-info
       mode-line-client
       mode-line-modified
       mode-line-remote
       evil-mode-line-tag
       "%b %n "
       mode-line-position
       mode-line-process
       (:eval
        (when (and (boundp 'text-scale-mode-amount)
                   (boundp 'text-scale-mode-lighter)
                   (not (eq text-scale-mode-amount 0)))
          (concat " [" text-scale-mode-lighter "]"))))

     '(""
       appt-mode-string
       " "
       org-mode-line-string
       " "
       battery-mode-line-string
       " "
       display-time-string
       " "
       mode-line-end-spaces)))))


(with-eval-after-load 'org-clock
  (add-hook
   'org-clock-out-hook
   (lambda ()
     (setq org-mode-line-string ""))))
;;; Modeline Section Ends


;; Unbind keys I don't use
(dolist (key '("\C-z"      ; Suspend frame
               "\C-x\C-z"  ; Suspend frame
               "\C-x\C-d"  ; List directory
               "\M-o"))    ; Facemenu stuff
  (global-unset-key key))

;;; Evil Section Begins
(defvar leader "SPC"
  "Key to use as leader.")

(defun leader (key)
  "Add the leader key on front of KEY."
  (kbd (concat leader " " key)))

(use-package evil
  :custom
  (evil-want-keybinding nil) ;; Needed for evil-collection
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)

  :config

  ;; `my-intercept-mode' is used to override default evil bindings
  (defvar my-intercept-mode-map (make-sparse-keymap)
    "High precedence keymap.")

  (define-minor-mode my-intercept-mode
    "Global minor mode for higher precedence evil keybindings."
    :global t)

  (my-intercept-mode)

  (dolist (state '(normal visual insert))
    (evil-make-intercept-map
     (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
     state))

  (evil-define-key '(normal motion visual) 'my-intercept-mode-map
    (kbd leader) nil
    (leader "TAB") #'whitespace-mode
    (leader "a")   (lambda ()
                     (interactive)
                     (if (get-buffer "*Org Agenda*")
                         (progn
                           (switch-to-buffer "*Org Agenda*")
                           (org-agenda-redo))
                       (org-agenda nil "o"))
                     (goto-char (point-min))
                     (org-agenda-to-appt t))
    (leader "c")   #'compile
    (leader "d")   #'kill-this-buffer
    (leader "e")   (lambda () (interactive)
                     (find-file (locate-user-emacs-file "init.el")))
    (leader "g")   #'magit-status
    (leader "i")   (lambda () (interactive)
                     (find-file (expand-file-name "inbox.org" org-directory)))
    (leader "o")   #'ispell
    (leader "s")   #'save-buffer
    (leader "t")   #'org-babel-tangle
    (leader "w")   #'eww-list-bookmarks
    (kbd "g h") #'counsel-org-goto
    (kbd "g H") #'counsel-org-goto-all

    (kbd "M-j") #'evil-scroll-line-down
    (kbd "M-k") #'evil-scroll-line-up
    (kbd "M-J") (lambda () (interactive) (text-scale-decrease 1)
                  (plist-put org-format-latex-options
                             ':scale (max (+ 2 text-scale-mode-amount) 1)))
    (kbd "M-K") (lambda () (interactive) (text-scale-increase 1)
                  (plist-put org-format-latex-options
                             ':scale (max (+ 2 text-scale-mode-amount) 1))))

  (evil-define-key '(insert) 'org-mode-map
    (kbd "M-L") #'org-shiftmetaright
    (kbd "M-l") #'org-metaright
    (kbd "M-H") #'org-shiftmetaleft
    (kbd "M-h") #'org-metaleft)

  (evil-mode t))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-goggles
  :config (evil-goggles-mode))
;;; Evil Section Ends


;;; Org Section Begins
(use-package org
  :custom
  (org-directory "~/documents/")
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-preview-latex-image-directory "~/.cache/org-preview-latex/")
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-duration-format 'h:mm)
  (org-log-done 'time)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-src-window-setup 'current-window)
  (org-html-postamble nil)
  (org-todo-keywords
   '((sequence "TODO" "DONE")
     (sequence "HABIT" "DONE")
     (sequence "DAYOF" "DONE")))
  :config
  (push 'org-habit org-modules)
  (push "SHOWFROMTIME" org-default-properties)
  (push "SHOWFROMDATE" org-default-properties)
  (org-indent-mode -1))

(use-package org-agenda
  :custom
  (calendar-holidays
   '((holiday-fixed 02 15 "Winter Break")
     (holiday-fixed 02 16 "Winter Break")
     (holiday-fixed 02 17 "Winter Break")
     (holiday-fixed 02 18 "Winter Break")
     (holiday-fixed 02 19 "Winter Break")
     (holiday-fixed 04 02 "Statutory holiday")))

  ;; Optimization
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-inhibit-startup t)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-ignore-drawer-properties '(effort appt stats category))

  (org-agenda-sticky t)
  (org-agenda-format-date "%F %A")
  (org-agenda-show-outline-path nil)
  (org-agenda-block-separator nil)
  (org-agenda-scheduled-leaders '("" ""))
  (org-agenda-deadline-leaders '("" ""))
  (org-agenda-start-on-weekday nil)
  (org-agenda-time-grid nil)
  (org-agenda-time-leading-zero t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-todo-keyword-format "")

  (org-agenda-files
   `(
     ,(expand-file-name "agenda/daily.org" org-directory)
     ,(expand-file-name "agenda/events.org" org-directory)
     ,(expand-file-name "agenda/timetracking.org" org-directory)
     ,(expand-file-name "agenda/todo.org" org-directory)
     ,@(directory-files-recursively
        "~/school" "\\`index\\.org\\'")))

  (org-agenda-custom-commands
   '(("o" "My Agenda"
      ((todo
        "TODO|DAYOF"
        ((org-agenda-overriding-header "Todo:")
         (org-agenda-prefix-format "%?T%s")
         (org-agenda-todo-ignore-deadlines 'future)
         (org-agenda-todo-ignore-scheduled 'future)
         (org-agenda-todo-ignore-timestamp 'future)
         (org-agenda-skip-function
          'org-agenda-skip-entry-before-SHOWFROMTIME-property)))
       (todo
        "HABIT"
        ((org-agenda-overriding-header "Today's Habits:")
         (org-agenda-prefix-format "")
         (org-agenda-todo-ignore-scheduled 'future)
         (org-agenda-skip-function
          'org-agenda-skip-entry-before-SHOWFROMTIME-property)))
       (agenda
        ""
        ((org-agenda-overriding-header "Schedule:")
         (org-agenda-prefix-format "    %-12t| %?-12:c %s")
         (org-agenda-span 60)
         (org-deadline-warning-days 0)
         (org-agenda-skip-function
          '(or
            (org-agenda-skip-entry-if 'todo '("DONE" "HABIT" "DAYOF" "LECTURE"))
            (org-agenda-skip-entry-before-SHOWFROMDATE-property)))))
       (agenda
        ""
        ((org-agenda-overriding-header "Time Tracking:")
         (org-agenda-prefix-format "%-18s | %-11t | ")
         (org-agenda-show-all-dates nil)
         (org-agenda-show-log 'clockcheck)))))))
  :config
  (defun dairy-last-work-day-of-month (&optional mark)
    "Used to schedule an item for the last work day of the month"
    (with-no-warnings (defvar date) (defvar entry))
    (let* ((dayname (calendar-day-of-week date))
           (day (calendar-extract-day date))
           (month (calendar-extract-month date))
           (year (calendar-extract-year date))
           (lastday (calendar-last-day-of-month month year)))
      (or (and (= day lastday) (memq dayname '(1 2 3 4 5)))
          (and (>= day (- lastday 2)) (= dayname 5)))))

  (defun org-agenda-skip-entry-before-SHOWFROMTIME-property ()
    "Skip entry if :SHOWFROMTIME: property is set and time of day is before it."
    (org-back-to-heading t)
    (let ((show-from-time-string
           (org-entry-get (point) "SHOWFROMTIME" 'inherit)))
      (when show-from-time-string
        (let* ((decoded-time-to-minutes
                (lambda (time)
                  (+ (decoded-time-minute time)
                     (* 60 (decoded-time-hour time)))))
               (show-from-time-minutes
                (funcall decoded-time-to-minutes
                         (parse-time-string show-from-time-string)))
               (currenttime-minutes
                (funcall decoded-time-to-minutes (decode-time))))
	      (when (< currenttime-minutes show-from-time-minutes)
            (org-entry-end-position))))))

  (defun org-agenda-skip-entry-before-SHOWFROMDATE-property ()
    "Skip entry if :SHOWFROMDATE: property is set and is before
the current date."
    (org-back-to-heading t)
    (let ((date-string
           (org-entry-get (point) "SHOWFROMDATE" 'inherit)))
      (when date-string
	    (when (< (time-to-days (current-time)) (org-time-string-to-absolute date-string))
            (org-entry-end-position))))))


(use-package org-clock
  :after org
  :custom
  (org-agenda-clock-consistency-checks '(:max-gap "0:00"))
  (org-clock-continuously t)
  (org-clock-display-default-range 'untilnow)
  (org-clock-in-resume t)
  (org-clock-mode-line-total 'current)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-out-switch-to-state #'org-clock-out-state)
  (org-clock-persist t)
  (org-clock-persist-query-resume nil)
  (org-clock-report-include-clocking-task t)
  (org-log-note-clock-out t)
  :config
  (defun org-clock-out-state (state)
    (if (string= state "HABIT")
        "DONE"
      state))

  (org-clock-persistence-insinuate))

(use-package org-contacts
  :custom
  (org-contacts-files
   (list (expand-file-name "contactlist.org" org-directory))))

(use-package evil-org
  :custom (evil-org-special-o/O nil)
  :hook (org-mode . evil-org-mode))

(use-package evil-org-agenda
  :config (evil-org-agenda-set-keys))

;; (use-package org-superstar
;;   :if (display-graphic-p)
;;   :hook (org-mode . org-superstar-mode))

(use-package org-drill
  :custom
  (org-drill-hide-item-headings-p t)
  (org-drill-save-buffers-after-drill-sessions-p nil)
  (org-drill-add-random-noise-to-intervals-p t)
  (org-drill-leech-method nil))
;;; Org Section Ends


;;; Ledger Mode Section Begins
(use-package ledger-mode)

(use-package flycheck-ledger
  :after (ledger-mode flycheck))
;;; Ledger Mode Section Ends


;;; Programming Section Begins
(use-package xcscope
  :config (cscope-setup))

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error))
  :hook (prog-mode . flymake-mode))

(use-package flycheck
  :custom (flycheck-emacs-lisp-load-path 'inherit)
  :init (global-flycheck-mode))

;; Save all buffers on compile automatically
(customize-set-variable 'compilation-ask-about-save nil)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(customize-set-variable 'c-basic-offset 4)

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode))

(use-package debbugs
  :custom
  (debbugs-gnu-default-packages '("emacs" "guix" "guix-patches")))

(use-package gdb-mi
  :custom (gdb-many-windows t))

;; Guix development
(use-package geiser
  :custom
  (geiser-active-implementations '(guile))
  (geiser-repl-history-filename
   (expand-file-name "geiser_history" user-emacs-directory))
  :config
  (with-eval-after-load 'geiser-guile
    (eval-when-compile
      (require 'geiser-guile))
    (add-to-list 'geiser-guile-load-path "~/src/guix")))

(use-package guix
  :hook (scheme-mode . guix-devel-mode))

(use-package flycheck-guile
  :after (flycheck geiser))

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "~/src/guix/etc/snippets")
  (yas-global-mode 1))
;;; Programming Section Ends


;;; VC/Diffs Section Begins
(use-package magit
  :hook (after-save . magit-after-save-refresh-status)
  :custom
  (magit-section-initial-visibility-alist
   '((stashes . hide) (untracked . hide)))
  (magit-auto-revert-immediately t)
  (magit-diff-refine-hunk t)
  (magit-log-margin-show-committer-date t)
  (magit-no-confirm '(safe-with-wip))
  (magit-save-repository-buffers 'dontask)
  (magit-wip-merge-branch t)
  (magit-diff-paint-whitespace nil)
  :config
  (magit-wip-mode 1)
  (add-to-list 'magit-process-find-password-functions
               'magit-process-password-auth-source))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package ediff
  :commands ediff
  :custom
  (ediff-diff-options "-w"))

(use-package ediff-wind
  :after ediff
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))
;;; VC/Diffs Section Ends


;;; Parens Section Begins
(use-package paren
  :custom
  (show-paren-delay 0)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;;; Parens Section Ends


;;; Whitespace Section Begins
(customize-set-variable 'tab-width 4)
(customize-set-variable 'indent-tabs-mode nil)
(electric-indent-mode 1)

(use-package ws-butler
  :config (ws-butler-global-mode)
  :custom (ws-butler-global-exempt-modes '(eshell-mode)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'message-mode-hook 'turn-on-auto-fill)
;;; Whitespace Section Ends


;;; Auto-complete/Hints Section Begins
(use-package ido
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-auto-merge-work-directories-length -1)
  :config
  (ido-mode 1))

(use-package company
  :custom
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  (company-tooltip-limit 20)
  :config
  (global-company-mode))

(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode))

(use-package which-key
  :custom (which-key-idle-secondary-delay 0.05)
  :config (which-key-mode))
;;; Auto-complete/Hints Section Ends


;;; EWW Section Begins
 (use-package eww
   :custom
   (eww-use-browse-url "\\`\\(gemini\\|gopher\\|mailto\\):"))

(use-package shr
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (shr-use-colors nil)
  (shr-max-image-proportion 0.5))

(use-package elpher)

(use-package browse-url
  :custom
  (browse-url-handlers
   '(("\\`\\(gemini\\|gopher\\)://" .
      (lambda (host-or-url &rest _) (elpher-go host-or-url))))))
;;; EWW Section Ends


;;; auth Section Begins
(use-package auth-source-pass
  :custom
  (auth-source-pass-filename
   (expand-file-name "password-store" (getenv "XDG_DATA_HOME"))))

(use-package pinentry
  :if (not IS-INSIDE-EMACS)
  :custom
  (epg-pinentry-mode 'loopback)
  (epg-gpg-home-directory (getenv "GNUPGHOME"))
  :config
  (setenv "INSIDE_EMACS" emacs-version)
  (pinentry-start))
;;; auth Section Ends

(use-package literate-calc-mode)

(use-package erc
  :custom
  (erc-nick "morgansmith")
  (erc-user-full-name "Morgan Smith")
  (erc-server "irc.freenode.net")
  (erc-port 6667)
  (erc-anonymous-login t)
  (erc-prompt-for-nickserv-password nil)
  (erc-log-channels-directory
   (create-directory "erc-logs" user-emacs-directory))
  (erc-save-buffer-on-part t)
  (erc-header-line-format nil)
  (erc-autojoin-timing 'ident)
  (erc-kill-buffer-on-part t)
  :config
  (add-to-list 'erc-modules 'keep-place)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))

(use-package flyspell
  :hook
  ((prog-mode . flyspell-prog-mode)
   ((text-mode text-mode message-mode) . flyspell-mode)))

;; Backups and auto-saves and deleting
(use-package files
  :hook (before-save . backup-buffer)
  :custom
  (backup-directory-alist
   `((".*" . ,(create-directory "backups" user-emacs-directory))))
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))
  (trash-directory (create-directory "trash" user-emacs-directory))
  (make-backup-files t)
  (backup-by-copying t)
  (version-control t)
  (vc-make-backup-files t)
  (delete-old-versions -1)
  (auto-save-default t)
  (auto-save-timeout 20)
  (auto-save-interval 200)
  (delete-by-moving-to-trash t))

(customize-set-variable
 'custom-file (expand-file-name "custom-garbage" trash-directory))

(use-package eshell
  ;; Save command history when commands are entered
  :hook (eshell-pre-command . eshell-save-some-history)
  :custom
  (eshell-banner-message "")
  (eshell-history-size nil "Pull history size from environment variables")
  (eshell-history-file-name nil "Pull history file from environment variables")
  (eshell-hist-ignoredups 'erase)
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setenv "PAGER" (executable-find "cat")))

(use-package eshell-syntax-highlighting
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-listing-switches
   "--all --file-type --group-directories-first -l --si --sort=version"))

(use-package dired-x
  :custom
  (dired-guess-shell-alist-user
   `((,(regexp-opt '(".amv" ".avi" ".flv" ".mkv" ".mov" ".mp4" ".webm" ".m4v")) "mpv")
     (,(regexp-opt '(".pdf")) "pdftotext"))))

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (remote-file-name-inhibit-cache nil)
  (tramp-completion-reread-directory-timeout nil)
  (tramp-use-ssh-controlmaster-options nil "Use system settings")
  (vc-ignore-dir-regexp
   (format "%s\\|%s"
           vc-ignore-dir-regexp
           tramp-file-name-regexp)))

(use-package tramp-sh
  :config
  ;; guix system bin
  (add-to-list 'tramp-remote-path "/run/current-system/profile/bin"))

(use-package minibuffer
  :custom
  (read-buffer-completion-ignore-case t))

(use-package exwm
  :if (and (display-graphic-p) (not IS-INSIDE-EMACS) (or IS-LINUX IS-BSD))
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
     ([?\s-?] . ,(lambda () (interactive) (shell-command "mpc status")))
     (,(kbd "<s-up>") . ,(lambda () (interactive) (shell-command "amixer set Master 5%+")))
     (,(kbd "<s-down>") . ,(lambda () (interactive) (shell-command "amixer set Master 5%-")))
     (,(kbd "<s-right>") . ,(lambda () (interactive) (shell-command "mpc next")))
     (,(kbd "<s-left>") . ,(lambda () (interactive) (shell-command "mpc prev")))
     ;; Char mode
     ([?\s-i] . exwm-input-release-keyboard)
     ;; Reset (to line-mode).
     ([?\s-r] . exwm-reset)
     ;; Fullscreen
     ([?\s-f] . exwm-layout-set-fullscreen)
     ;; Switch focus.
     ([?\s-j] . other-window)
     ([?\s-k] . ,(lambda () (interactive) (other-window -1)))
     ;; Split window.
     ([?\s-\\] . split-window-horizontally)
     ([?\s-\-] . split-window-vertically)
     ;; eshell
     (,(kbd "<s-return>") . eshell)
     ;; Launch application.
     ([?\s-d] . ,(lambda (command)
                   (interactive (list (read-shell-command "$ ")))
                   (start-process-shell-command command nil command)))
     ;; Switch monitor
     ([?\s-l] . ,(lambda () (interactive)
                   (exwm-workspace-switch
                    (% (1+ (exwm-workspace--position (selected-frame)))
                       (/ (cl-list-length exwm-randr-workspace-monitor-plist) 2)))))
     ([?\s-h] . ,(lambda () (interactive)
                   (let ((num-monitors (/ (cl-list-length exwm-randr-workspace-monitor-plist) 2)))
                     (exwm-workspace-switch
                      (% (1- (+ (exwm-workspace--position (selected-frame)) num-monitors))
                         num-monitors)))))
     ;; Switch to workspace
     ,@(mapcar (lambda (i)
                 `(,(kbd (format "s-%d" i)) .
                   (lambda ()
                     (interactive)
                     (exwm-workspace-switch ,(1- i)))))
               (number-sequence 1 9))))
  :config
  (require 'exwm-config)

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
    (lambda ()
      (exwm-workspace-rename-buffer exwm-class-name)))

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (require 'exwm-randr)

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

      (customize-set-variable 'exwm-randr-workspace-monitor-plist value)))

  (add-hook 'exwm-randr-screen-change-hook #'exwm-monitor-update)
  (add-hook 'exwm-init-hook #'exwm-monitor-update)

  (exwm-randr-enable)
  (exwm-config-ido)
  (exwm-enable)

  ;; Tell gpg what screen to use for pinentry
  (shell-command "gpg-connect-agent \"UPDATESTARTUPTTY\" /bye"))

(use-package bluetooth)

(use-package nginx-mode)

(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'executable)
  (plantuml-indent-level 4)
  (org-plantuml-exec-mode 'plantuml)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal plantuml-mode-map (leader "c") #'plantuml-preview))
  :mode ("\\.uml\\'" . plantuml-mode))

(use-package pdf-tools
  :config (pdf-tools-install))

(use-package nov
  :custom (nov-text-width 80)
  :mode ("\\.epub\\'" . nov-mode))

(use-package djvu)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))

(use-package youtube-dl
  :custom
  (youtube-dl-directory (create-directory (getenv "XDG_DOWNLOAD_DIR"))))

(use-package transmission
  :if (executable-find "transmission-daemon")
  :commands transmission transmission-add
  :custom
  (transmission-refresh-modes '(transmission-mode
                                transmission-files-mode
                                transmission-info-mode
                                transmission-peers-mode)))

(use-package counsel)

(use-package time-stamp
  :custom
  (time-stamp-format "%Y-%02m-%02d %3a %02H:%02M")
  :config
  (add-hook 'before-save-hook 'time-stamp))

(defun download-file (&optional file-link)
  "Downloads a file.
Uses either `youtube-dl' or `transmission'.  Downloads either
FILE-LINK, the URL at current point, or the URL in the clipboard"
  (interactive)
  (let* ((item (or file-link
                  (thing-at-point 'url)
                  (when interprogram-paste-function
                    (funcall interprogram-paste-function))))
        (link (url-encode-url item)))
    (unless item
          (error "No link provided"))
    (cond ((string-match "^magnet" link) (transmission-add link))
          ((youtube-dl-item-id (youtube-dl link)) nil)
          ((string-match ".mp4$" link)
           (start-process
            "wget"
            (generate-new-buffer "wget")
            "wget"
            (concat "--directory-prefix=" (getenv "XDG_DOWNLOAD_DIR"))
            "--progress=dot:mega"
            link))
          ((string-match "^youtube-dl" item)
           (async-shell-command link))
          (t (error "Can't download link: %S" link)))
    (message "Downloading link: %S" link)))

(global-set-key (kbd "C-c d") #'download-file)


(use-package auth-source-xoauth2
  :after smtpmail
  :config
  (defun my-xoauth2-get-secrets (_host user _port)
    (when (string= user (auth-source-pass-get "address" "email/work"))
      (list
       :token-url "https://accounts.google.com/o/oauth2/token"
       :client-id (auth-source-pass-get "client-id" "email/work")
       :client-secret (auth-source-pass-get "client-secret" "email/work")
       :refresh-token (auth-source-pass-get "refresh-token" "email/work"))))
  (setq auth-source-xoauth2-creds 'my-xoauth2-get-secrets)

  (eval-when-compile
    (require 'smtpmail))
  (add-to-list 'smtpmail-auth-supported 'xoauth2)
  (auth-source-xoauth2-enable))

(use-package typit
  :custom-face
  (typit-current-word ((t (:inherit bold)))))

(use-package disk-usage
  :config
  (defun disk-usage-filter-proc (path _attributes)
    (not (string-match "\\(^/proc\\|:/proc\\)" path)))
  (add-to-list 'disk-usage-available-filters 'disk-usage-filter-proc)
  (add-to-list 'disk-usage-default-filters 'disk-usage-filter-proc))

(provide 'init.el)
;;; init.el ends here
