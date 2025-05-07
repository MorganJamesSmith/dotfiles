;;; init.el --- My personal init file -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Morgan Smith

;;; Commentary:

;; This is my personal Emacs init file.
;; It is crafted with love, care, and stupidity.
;; Use at your own risk.

;;; Code:

;; TODO: re-enable once packages update
(setopt warning-suppress-log-types '((files missing-lexbind-cookie)))

(require 'xdg)
(require 'emacs-secrets "/home/pancake/documents/configs/private/emacs-secrets.el")

(defconst EXTERNAL-PACKAGES? (not (eq system-type 'android)))

(setopt use-package-always-demand t)

(setopt source-directory "~/src/emacs/emacs")

;; This is non-nil for instances of Emacs started from within an
;; instance of Emacs
(defconst IS-INSIDE-EMACS (getenv "INSIDE_EMACS"))
(setenv "INSIDE_EMACS" emacs-version)

(setopt user-full-name "Morgan Smith")
(setopt user-mail-address "Morgan.J.Smith@outlook.com")

(defun create-directory (dir &optional default-dir)
  "Return DIR as a directory and create DIR if it doesn't already exist.
If DIR is relative, it will be relative to DEFAULT-DIR
If DEFAULT-DIR isn't provided, DIR is relative to ~"
  (let ((directory (file-name-as-directory
                    (expand-file-name dir (or default-dir "~")))))
    (unless (file-exists-p directory)
      (make-directory directory t))
    directory))

(defvar last-unlock-gpg 0)
(defun unlock-gpg (&rest _ignore)
  "Using Emacs pinentry can cause a stalemate so call this before using GPG."
  (interactive)
  ;; cache for 5 minutes since GPG caches for 10 minutes
  (unless (time-less-p (time-since last-unlock-gpg) (seconds-to-time 300))
    (setq last-unlock-gpg (current-time))
    (let ((process
           (make-process
            :name "unlock gpg"
            :command '("gpg" "--dry-run" "--sign" "/dev/null"))))
      (while (process-live-p process)
        (thread-yield)
        (sit-for 0.01)))))

(defun network-connectivityp ()
  "Do we currently have networking?"
  (eq 0 (call-process "nm-online" nil nil nil "--exit" "--timeout=0")))

(defun assert-network-connectivity ()
  "Throw error when there is no networking."
  (unless (network-connectivityp)
    (user-error "Not currently connected to the network")))

(setopt async-shell-command-buffer 'new-buffer)

;;; Optimization Section Begins
(setopt normal-erase-is-backspace nil)

;; Display the bare minimum at startup
(setopt inhibit-startup-screen t)
(setopt inhibit-startup-echo-area-message user-login-name)
(setopt inhibit-default-init t)
(setopt initial-major-mode 'fundamental-mode)
(setopt initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Not entirely sure what this does.  Does it affect the system clipboard?
(setopt select-active-regions nil)

;; Only matters on multi-user systems
(setopt create-lockfiles nil)

(global-so-long-mode)
;;; Optimization Section Ends


;;; Sensible Defaults Section Begins
(global-auto-revert-mode t)
(setopt global-auto-revert-non-file-buffers t)
(setopt revert-without-query '("."))
(setopt auto-revert-avoid-polling t)
(setopt dired-auto-revert-buffer t)

(setopt shell-kill-buffer-on-exit t)

(setopt fit-window-to-buffer-horizontally t)

(setopt view-read-only t)

(setopt savehist-additional-variables '(register-alist kill-ring))
(savehist-mode 1)
(recentf-mode)

(setopt help-enable-variable-value-editing t)

;; Use ibuffer
;; TODO: make this respect global-auto-revert-non-file-buffers
;; TODO: make this not jump my cursor around on refresh when window not active
;; (add-hook 'ibuffer-hook 'ibuffer-auto-mode) ;; auto-revert ibuffer
(keymap-global-set "C-x C-b" #'ibuffer)

(setopt read-mail-command 'gnus)
(setopt mail-user-agent 'gnus-user-agent)

(use-package sendmail
  :custom
  (sendmail-program "msmtp")
  (send-mail-function #'sendmail-send-it))

(use-package message
  :custom
  (message-sendmail-envelope-from 'header)
  :config
  (add-hook 'message-send-hook 'unlock-gpg))

;; Move gnus folders to the `user-emacs-directory'
(use-package gnus
  :defines gnus-home-directory
  :custom
  (gnus-init-file (expand-file-name "gnus" user-emacs-directory))
  (gnus-home-directory (create-directory "gnus-files" user-emacs-directory))
  (gnus-directory (create-directory "News" gnus-home-directory))
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)
  :config
  (add-hook 'emacs-startup-hook 'gnus-read-init-file))
(setopt mail-source-directory (create-directory "Mail" gnus-home-directory))

(setopt mm-uu-hide-markers nil)
(setopt mml-attach-file-at-the-end t)

(use-package imenu
  :custom
  (imenu-flatten 'prefix)
  (imenu-space-replacement nil)
  (imenu-auto-rescan t)
  (imenu-auto-rescan-maxout 50000000))

(setopt Man-notify-method 'aggressive)

;; Use only encrypted authinfo
(setopt auth-sources `((:source ,(expand-file-name "authinfo.gpg" user-emacs-directory)))
        auth-source-save-behavior t
        auth-source-gpg-encrypt-to (list user-mail-address))

(setopt browse-url-default-scheme "https")

;; save after every modification
(setopt bookmark-save-flag 0)

(setopt text-quoting-style 'grave)

;; Date should always be big to small (year/month/day)
(setopt calendar-date-style 'iso)

(setopt fill-column 79)

(setopt next-error-message-highlight t)

(setopt read-minibuffer-restore-windows nil)

(setopt translate-upper-case-key-bindings nil)

(setopt minibuffer-default-prompt-format " [%s]")
(minibuffer-electric-default-mode)
(setopt extended-command-suggest-shorter nil)
(setopt kill-do-not-save-duplicates t)
(setopt kill-read-only-ok t)
(setopt save-interprogram-paste-before-kill t)
(setopt kill-whole-line t)

(use-package info
  :hook (Info-mode . visual-line-mode)
  :bind
  (:map Info-mode-map
        ("<remap> <scroll-up>" . Info-scroll-up)))

(setopt visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;;; Sensible Defaults Section Ends


;;; Terrible Defaults Section Starts
(setopt use-short-answers t)

(setopt enable-local-variables :all)

(setopt org-link-elisp-confirm-function nil)

(setopt disabled-command-function nil)

;; Why would you want to leave?
(keymap-global-unset "C-x C-c")
;;; Terrible Defaults Section Ends


;;; Pretty Visuals Section Begins
(use-package emacs  ;; built-in themes can't be loaded
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-prompts '(bold))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-headings '((0 . (2.5))
                           (agenda-structure . (1.5))))
  :config
  (load-theme 'modus-vivendi t))

(setopt proced-enable-color-flag t)

;; I dislike gui stuff
(setopt visible-bell t)
(setopt use-file-dialog nil)
(setopt use-dialog-box nil)
(blink-cursor-mode -1)
;; Ok I need these on android
(when (not (eq system-type 'android))
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))
;;; Pretty Visuals Section Ends

(when (eq system-type 'android)
  (keymap-global-set
   "<volume-up>"
   (lambda ()
     (interactive)
     (frame-toggle-on-screen-keyboard
      nil
      (>= 50 (frame-height)))))

  (keymap-global-set "<volume-down>" #'scroll-up))

;;; Modeline/Tab Bar Section Begins
(setopt display-time-24hr-format t)
(setopt display-time-day-and-date t)
(display-time-mode)

(setopt battery-mode-line-format "[%b%p%%, %r] ")
(display-battery-mode)
(size-indication-mode)
(column-number-mode)

(setopt mode-line-compact 'long)

(setopt tab-bar-format '(tab-bar-format-global)
        tab-bar-auto-width nil)
(tab-bar-mode)
;;; Modeline/Tab Bar Section Ends


;;; Org Section Begins
(setopt org-fast-tag-selection-single-key t)

(use-package ox-html
  :custom
  (org-html-preamble nil)
  (org-html-postamble nil)
  (org-html-head-include-default-style nil)
  (org-html-meta-tags nil))

(use-package org
  :functions org-insert-timestamp
  :custom
  (org-directory "~/documents/")
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-preview-latex-image-directory
   (file-name-as-directory (expand-file-name "org-preview-latex" (xdg-cache-home))))
  (org-refile-targets
   `((,(expand-file-name "agenda/todo.org" org-directory) .
      (:regexp . "refile target"))
     (nil . (:maxlevel . 2))))
  (org-outline-path-complete-in-steps nil)
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-duration-format 'h:mm)
  (org-log-done 'time)
  (org-edit-src-content-indentation 0)
  (org-use-speed-commands t) ; org-speed-commands
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-src-window-setup 'current-window)
  (org-read-date-popup-calendar nil)
  (org-special-ctrl-a/e t)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-enforce-todo-dependencies t)
  (org-todo-keywords
   '((sequence "TODO" "|" "DONE" "FAILED")
     (sequence "WAITINGFOR" "DONE")
     (sequence "HABIT" "DONE")
     (sequence "PROJECT" "DONE")))
  :config
  (push 'org-habit org-modules)

  ;; I keep accidentally archiving stuff
  (keymap-unset org-mode-map "C-c C-x C-s")

  ;; My custom patch
  (when (fboundp 'org-tags-sort-hierarchy)
    (setopt org-tags-sort-function #'org-tags-sort-hierarchy))
  :bind
  (;; Useful for more then just org
   ("C-c ." . org-timestamp)
   ("C-c !" . org-timestamp-inactive)

   (:map org-mode-map
         ("M-p" . org-metaup)
         ("M-n" . org-metadown))))

(setopt outline-minor-mode-cycle t)

(use-package org-indent
  ;; If this every gets accidentally loaded, immediately disable it
  :defer t
  :functions org-indent-mode
  :config (org-indent-mode -1))

;; For when I use org-babel to create images
(autoload 'org-link-preview-refresh "ol")
(add-hook 'org-babel-after-execute-hook #'org-link-preview-refresh)

(use-package holidays
  :custom
  (calendar-holidays
   '((holiday-fixed 1 1 "New Year's Day (National Holiday)")
     (holiday-fixed 2 2 "Groundhog Day")
     (holiday-fixed 2 14 "Valentine's Day")
     (holiday-float 2 1 3 "Family Day (Ontario Holiday)")
     (holiday-fixed 3 17 "Saint Patrick's Day")
     (holiday-fixed 4 1 "April Fool's Day")
     (holiday-fixed 4 6 "Tartan Day")
     (holiday-easter-etc -2 "Good Friday (National Holiday)")
     (holiday-easter-etc 0 "Easter Sunday")
     (holiday-easter-etc 1 "Easter Monday (Federal Holiday)")
     (holiday-fixed 4 22 "Earth Day")
     (holiday-float 5 1 -1 "Victoria Day (Federal and Ontario Holiday)" 25)
     (holiday-float 5 0 2 "Mother's Day")
     (holiday-float 6 0 3 "Father's Day")
     (holiday-fixed 7 1 "Canada Day (National Holiday)")
     (holiday-float 8 1 1 "Civic Holiday (Federal Holiday)")
     (holiday-float 9 1 1 "Labour Day (National Holiday)")
     (holiday-fixed 9 30 "National Day for Truth and Reconciliation (Federal Holiday)")
     (holiday-float 10 1 2 "Thanksgiving (Federal and Ontario Holiday)")
     (holiday-fixed 11 11 "Remembrance Day (Federal Holiday)")
     (holiday-fixed 12 25 "Christmas Day (National Holiday)")
     (holiday-fixed 12 26 "Boxing Day (Federal and Ontario Holiday)"))))

(use-package org-agenda
  :bind
  (("C-c a" . (lambda () (interactive) (org-agenda nil "o")))
   ("C-c l" . (lambda () (interactive) (org-agenda nil "l")))
   ("C-c t" . (lambda () (interactive) (org-agenda nil "p"))))
  :custom
  (org-agenda-sticky t)
  (org-agenda-prefix-format " ")
  (org-agenda-format-date "%F %a     (W%V)")
  (org-agenda-show-outline-path nil)
  (org-agenda-block-separator "")
  (org-agenda-scheduled-leaders '("" ""))
  (org-agenda-deadline-leaders '("" "" ""))
  (org-agenda-start-on-weekday nil)
  (org-agenda-use-time-grid nil)
  (org-agenda-time-leading-zero t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-todo-keyword-format "")
  (org-agenda-remove-tags t)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-remove-timeranges-from-blocks t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-scheduled-repeats-after-deadline t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-sorting-strategy '(time-up priority-down tag-up todo-state-up category-keep))
  (org-stuck-projects '("TODO=\"PROJECT\"" ("TODO") nil ""))

  (org-agenda-files
   (list
    (expand-file-name "contacts.org" org-directory)
    (expand-file-name "agenda/daily.org" org-directory)
    (expand-file-name "agenda/events.org" org-directory)
    (expand-file-name "agenda/timetracking.org" org-directory)
    (expand-file-name "agenda/todo.org" org-directory)))

  (org-agenda-custom-commands
   '(("l" "Clocking for all time"
      ((agenda
        ""
        ((org-agenda-log-mode-items '(clock))
         (org-agenda-start-day "2019-05-05")
         (org-agenda-entry-types '())
         (org-agenda-span 2000)
         (org-agenda-overriding-header "Time Tracking:")
         (org-agenda-prefix-format "%-11t | %-17s | ")
         (org-agenda-show-all-dates nil)
         (org-agenda-show-log 'clockcheck)
         (org-agenda-files (list (expand-file-name "agenda/timetracking.org" org-directory)))))))
     ("p" "All TODO Items"
      ((tags-todo
        "-TODO=\"HABIT\"-goals"
        ((org-agenda-prefix-format "%-4.4T: %l")
         (org-agenda-todo-keyword-format "%-1s")
         (org-agenda-sorting-strategy '(category-keep))))))
     ("o" "My Agenda"
      (
       (tags-todo
        "+annual_goal"
        ((org-agenda-overriding-header "Annual Goals:")
         (org-use-tag-inheritance nil)
         (org-agenda-todo-ignore-timestamp 'future)))
       (tags-todo
        "+monthly_goal"
        ((org-agenda-overriding-header "Monthly Goals:")
         (org-use-tag-inheritance nil)
         (org-agenda-todo-ignore-timestamp 'future)))
       (tags-todo
        "+weekly_goal"
        ((org-agenda-overriding-header "Weekly Goals:")
         (org-use-tag-inheritance nil)
         (org-agenda-todo-ignore-timestamp 'future)))
       (tags-todo
        "+daily_goal"
        ((org-agenda-overriding-header "Daily Goals:")
         (org-agenda-todo-ignore-timestamp 'future)
         (org-agenda-dim-blocked-tasks nil)))
       (agenda ;; habits
        ""
        (;; I do the header funny to avoid an extra newline
         (org-agenda-overriding-header (lambda () "Today's Habits: "))
         (org-agenda-format-date "")
         (org-agenda-prefix-format "%-4.4T: ")
         (org-agenda-span 'day)
         (org-habit-clock-completes-habit t)
         ;; (org-habit-show-done-always-green t) ;; TODO: doesn't take effect here
         ;; (org-habit-graph-column 23) ;; TODO: doesn't take effect here
         (org-agenda-entry-types '(:scheduled))
         (org-agenda-skip-function
          '(org-agenda-skip-entry-if 'nottodo '("HABIT")))))
       (agenda ;; schedule
        ""
        ((org-agenda-overriding-header "Schedule:")
         (org-agenda-prefix-format "    %-12t| %?-12:c %s")
         (org-agenda-span 60)
         (org-deadline-warning-days 0)
         (org-agenda-skip-function
          '(org-agenda-skip-entry-if 'todo '("HABIT")))))
       (tags-todo ;; todo
        "TODO=\"TODO\"-goals"
        ((org-agenda-overriding-header "Todo:")
         (org-agenda-prefix-format "%-4.4T: ")
         ;; Will show up in "Schedule" section
         (org-agenda-todo-ignore-deadlines 'all)
         (org-agenda-todo-ignore-scheduled 'all)
         (org-agenda-todo-ignore-timestamp 'all)))
       (todo ;; waiting for
        "WAITINGFOR"
        ((org-agenda-overriding-header "Waiting for:")))
       (todo
        "PROJECT"
        ((org-agenda-overriding-header "Projects:")))
       ;; Don't need a list of stuck projects because any stuck projects will
       ;; show up as unblocked in the project section
       ;; TODO: not true
       ;; (stuck "")
       (agenda
        ""
        ((org-agenda-overriding-header "Time Tracking:")
         (org-agenda-entry-types '())
         (org-agenda-prefix-format "%-11t | %-17s | ")
         (org-agenda-span 15)
         (org-agenda-show-all-dates nil)
         (org-agenda-show-log 'clockcheck)
         (org-agenda-files (list (expand-file-name "agenda/timetracking.org" org-directory))))))))))

(use-package org-habit
  :custom
  (org-habit-show-done-always-green t)
  (org-habit-graph-column 29)
  (org-habit-following-days 3)
  ;; TODO: report use-package not being able to reference previously set variables
  (org-habit-preceding-days (- 103 3 29)) ;; (- 103 org-habit-following-days org-habit-graph-column))
  ;; org-habit has too many colors.  Use fewer
  (face-remapping-alist '((org-habit-clear-face . org-habit-ready-face)
                          (org-habit-alert-future-face . org-habit-overdue-face)
                          (org-habit-clear-future-face . org-habit-ready-future-face))))

(setopt
 org-agenda-clock-consistency-checks '(:max-gap "0:00")
 org-clock-continuously t
 org-clock-display-default-range 'untilnow
 org-clock-in-resume t
 org-clock-mode-line-total 'current
 org-clock-out-switch-to-state #'org-clock-out-state
 org-clock-persist t
 org-clock-persist-query-resume nil
 org-clock-report-include-clocking-task t)

(setopt org-clock-ask-before-exiting nil)


(defun org-clock-out-state (state)
  "State we should be after we clock out of STATE."
  (if (string= state "HABIT")
      "DONE"
    state))
(org-clock-persistence-insinuate)

(use-package org-clock
  :custom
  (org-clock-resolve-expert t)
  :config
  ;; This sorting doesn't work if there are multiple un-compacted levels.  But
  ;; as far as I can tell there is no built-in way to sort in that scenario
  (plist-put org-clocktable-defaults :sort '(2 . ?T))
  (plist-put org-clocktable-defaults :compact t)
  ;; Same width as header so tables are always the same width regardless of
  ;; contents
  (plist-put org-clocktable-defaults :narrow '12!)
  (plist-put org-clocktable-defaults :formula '%)
  (plist-put org-clocktable-defaults :match "-ignore")
  (plist-put org-clocktable-defaults :maxlevel 1)
  ;; Remove annoying file headers
  (plist-put org-clocktable-defaults :hidefiles t)
  (plist-put org-clocktable-defaults :fileskip0 t))

;; org-capture-before-finalize-hook
(keymap-global-set "C-c c" #'org-capture)
(setopt org-capture-bookmark nil)
(setopt org-capture-templates
        '(
          ("i" "Inbox" entry (file "inbox.org") "* %?" :prepend t)
          ("w" "Weight"
           table-line (file+headline "wiki/morgan.org" "weight")
           "| %? | %U |" :jump-to-captured t :prepend t)
          ("h" "headache"
           table-line (file+headline "wiki/morgan.org" "headache")
           "| %^{Intensity
1: Slight pain
2: Pain
3: Trouble thinking
4: Light sensitive
5: Light and sound sensitive
||1|2|3|4|5} | %U |" :prepend t :immediate-finish t)
          ("m" "Mood"
           table-line (file+headline "wiki/morgan.org" "mood")
           "| %? | %U |" :prepend t)
          ("e" "Energy"
           table-line (file+headline "wiki/morgan.org" "energy")
           "| %^{Energy level
1: Eye's feel heavy
2: Yawning now and then
3: Average
4: Excited
5: Bursting with energy!
||1|2|3|4|5} | %U |" :prepend t :immediate-finish t)))

(setopt org-link-elisp-confirm-function nil)
(setopt org-link-descriptive nil)
(setopt org-link-abbrev-alist
        `(("guix" . "elisp:(guix-packages-by-name \"%s\")")
          ("possessions" . "file:~/documents/wiki/possessions.org::*")
          ("contact" . "file:~/documents/contacts.org::*")
          ("money" . ,(concat "file:" (getenv "LEDGER_FILE") "::[[possessions:%s]]"))))

(setopt org-imenu-depth 5)

;;;; Syncing calendar with my phone
(setopt org-icalendar-combined-agenda-file (expand-file-name "org.ics" org-directory))

;; TODO: fix root cause: icalendar--convert-float-to-ical
;; diary-float entries get scheduled today without this
(setopt icalendar-export-sexp-enumerate-all t)

(setopt org-icalendar-exclude-tags '("goals" "annual_goal" "monthly_goal" "weekly_goal" "daily_goal"))
(setopt org-icalendar-use-scheduled '(event-if-not-todo))
;; 2 hours
(defvar update-org-icalendar-timer-loop-seconds (* 2 60 60))
(defvar update-org-icalendar-timer)

(defun maybe-update-org-icalendar ()
  "Update org icalendar file if it hasn't been updated in recently."
  (when (and
         ;; I export to sync to other devices so I only need to do this when connected to the network
         (network-connectivityp)
         (<
          update-org-icalendar-timer-loop-seconds
          (time-to-seconds
           (time-since (file-attribute-modification-time
                        (file-attributes org-icalendar-combined-agenda-file))))))
    (let ((org-export-with-broken-links t)
          (org-agenda-files
           (remove (expand-file-name "agenda/timetracking.org" org-directory)
                   org-agenda-files))
          ;; TODO: Look into these warnings
          (warning-inhibit-types '((holidays))))
      (org-icalendar-combine-agenda-files))
    (kill-buffer "*icalendar-errors*")
    (message "Updating org icalendar file")))

(defun update-org-icalendar-timer-loop ()
  "Keep the org icalendar file up to date."
  (run-with-idle-timer 5 nil #'maybe-update-org-icalendar)
  (cancel-function-timers #'update-org-icalendar-timer-loop)
  (run-with-timer update-org-icalendar-timer-loop-seconds
                  nil
                  #'update-org-icalendar-timer-loop))

(update-org-icalendar-timer-loop)
;;;; Syncing calendar with my phone


;; Half my 1920x1080 screen
(setopt org-plot/gnuplot-term-extra "size 960,1080")
;; Add a nice grid
(setopt org-plot/gnuplot-script-preamble "set grid")

(defun replace-region-with-org-timestamp (beg end)
  "Replace region BEG to END with an org timestamp."
  (interactive "r")
  (kill-region beg end)
  (let* ((time-string (downcase (car kill-ring)))
         (correction (or (and (string-suffix-p "pm" time-string) (* 60 60 12))
                         0)))
   (org-insert-timestamp (time-add (date-to-time time-string) correction) t t)))

(defun search-org-directory (regexp)
  "Search through my org files in my `org-directory' for REGEXP."
  (interactive "sRegex: ")
  (let ((files (directory-files-recursively
                (expand-file-name org-directory)
                "\\.org\\'"
                nil
                (lambda (directory)
                  (not
                   (member (file-name-nondirectory directory)
                           '(".stversions")))))))
    (xref-show-xrefs (lambda () (xref-matches-in-files regexp files)) nil)))
;;; Org Section Ends


;;; Programming Section Begins
(use-package project
  ;; dumb autoload to avoid warnings
  :autoload project-root-dir
  :custom
  ;; Ignore translation files
  (project-vc-ignores (list "*.po"))
  :config
  (defun project-root-dir (root)
    "Return a function to set directories under ROOT to be a project."
    (setq root (file-name-as-directory root))
    (lambda (dir)
      (when (and (file-in-directory-p dir root)
                 (not (file-equal-p dir root)))
        (cons 'transient
              (file-name-as-directory
               (file-name-concat root
                                (car (file-name-split (file-relative-name dir root)))))))))

  (add-to-list 'project-find-functions (project-root-dir "/tmp") t)
  (add-to-list 'project-find-functions (project-root-dir "/gnu/store") t))

(use-package proof ;; Proof General
  :custom
  (proof-electric-terminator-enable t))

(use-package elisp-mode :delight emacs-lisp-mode)
(use-package eldoc
  :delight
  :hook ielm-mode)

(defmacro time-execution (&rest body)
  "Time how long it takes to run BODY."
  (let ((start-time (make-symbol "start-time")))
    `(let ((,start-time (current-time)))
       ,@body
       (time-to-seconds (time-since ,start-time)))))

(defmacro profile (&rest body)
  "Profile BODY."
  `(prog1
     (unwind-protect
         (progn
           (profiler-start 'cpu)
           (time-execution
            ,@body))
       (profiler-stop))
     (profiler-report)))

(add-hook 'prog-mode-hook #'elide-head-mode)

;; Handy keybinds are
;; M-.     xref-find-definitions
;; M-,     xref-pop-marker-stack
;; M-?     xref-find-references
;; C-M-.   xref-find-apropos
(use-package ggtags
  :if EXTERNAL-PACKAGES?
  :delight
  :hook c-mode-common
  :functions ggtags-build-imenu-index
  :custom
  (ggtags-use-sqlite3 t)
  (ggtags-enable-navigation-keys nil)
  :config
  (setenv "GTAGSLABEL" "ctags")
  (add-hook
   'c-mode-common-hook
   (lambda ()
     (setq-local imenu-create-index-function #'ggtags-build-imenu-index))))

(use-package cwarn
  :delight
  :hook c-mode-common)

(use-package flymake
  :hook prog-mode
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :custom
  (flymake-mode-line-format
   '("" flymake-mode-line-exception
     flymake-mode-line-counters))
  (flymake-show-diagnostics-at-end-of-line t)
  (flymake-cc-command 'my-flymake-cc-command)
  :config
  (defun my-flymake-cc-command ()
    "Use the makefile at project root."
    (unless (executable-find "make") (error "Make not found"))
    `("make"
      "-s" "-C" ,(project-root (project-current t))
      "check-syntax"
      ,(format "CHK_SOURCES=-x %s -c -"
               (cond ((derived-mode-p 'c++-mode) "c++")
                     (t "c"))))))

(use-package electric
  :hook
  (((prog-mode conf-mode) . electric-layout-local-mode)
   ;; TODO: make it respect eshell prompt field
   ;; (add-to-list 'electric-pair-pairs '(?\' . ?\'))
   ((prog-mode conf-mode comint-mode) . electric-pair-local-mode)
   ((prog-mode conf-mode) . electric-indent-local-mode)))

(which-function-mode)
(global-prettify-symbols-mode)
(setq-default c-auto-newline t)
(setq-default c-hungry-delete-key t)

(with-eval-after-load "cc-mode"
  (keymap-set c-mode-base-map "RET" 'c-context-line-break))

(setopt c-cleanup-list (list 'brace-else-brace
                             'brace-elseif-brace
                             'compact-empty-funcall
                             'comment-close-slash))

(with-eval-after-load "cc-vars"
  (setf (alist-get 'other c-default-style) "stroustrup"))


(use-package display-line-numbers
  :hook (prog-mode conf-mode))

(setopt gdb-many-windows t)

(use-package bug-reference
  :hook ((gnus-mode gnus-summary-mode gnus-article-mode log-view-mode debbugs-browse-mode)
         (prog-mode . bug-reference-prog-mode))
  :custom
  (bug-reference-url-format "https://debbugs.gnu.org/%s")
  ;; Take from [[info:guix#Viewing Bugs within Emacs]]
  (bug-reference-bug-regexp
   (rx (group (or (seq word-boundary
                       (or (seq (char "Bb") "ug"
                                (zero-or-one " ")
                                (zero-or-one "#"))
                           (seq (char "Pp") "atch"
                                (zero-or-one " ")
                                "#")
                           (seq (char "Ff") "ixes"
                                (zero-or-one ":")
                                (zero-or-one " ") "#")
                           (seq "RFE"
                                (zero-or-one " ") "#")
                           (seq "PR "
                                (one-or-more (char "a-z+-")) "/"))
                       (group (one-or-more (char "0-9"))
                              (zero-or-one
                               (seq "#" (one-or-more
                                         (char "0-9"))))))
                  (seq (? "<") "https://bugs.gnu.org/"
                       (group-n 2 (one-or-more (char "0-9")))
                       (? ">"))
                  (seq (? "<") "https://issues.guix.gnu.org/"
                       (? "issue/")
                       (group-n 2 (one-or-more (char "0-9")))
                       (? ">")))))))

(use-package debbugs-gnu
  :if EXTERNAL-PACKAGES?
  :commands debbugs-gnu
  :custom
  ;; seems to cause performance issues maybe?
  (debbugs-show-progress nil)
  ;; Show feature requests.
  (debbugs-gnu-default-severities
   '("serious" "important" "normal" "minor" "wishlist")))

(defun debbugs-gnu-guix ()
  "List Guix issues."
  (interactive)
  (debbugs-gnu '("serious" "important" "normal")
               '("guix" "guix-patches")
               nil
               t))

(defun debbugs-gnu-guile ()
  "List guile issues."
  (interactive)
  (debbugs-gnu '("serious" "important" "normal")
               '("guile")
               nil
               t))

(setopt scheme-program-name "guile")
(setopt scheme-mit-dialect nil)

(use-package geiser
  :if EXTERNAL-PACKAGES?
  :custom
  (geiser-repl-history-filename
   (expand-file-name "geiser_history" user-emacs-directory))
  (geiser-mode-smart-tab-p t)
  (geiser-debug-jump-to-debug nil)
  (geiser-debug-show-debug nil)
  (geiser-guile-warning-level 'high))

(use-package guix-devel
  :if EXTERNAL-PACKAGES?
  :hook scheme-mode
  :config
  (defalias 'pcomplete/guix #'ignore)) ;; Freezes up eshell

(use-package arei
  :if EXTERNAL-PACKAGES?)

(defun run-guile-server ()
  "Run a guile server for arei."
  (interactive)
  (make-process
   :name "guile-server" :buffer (generate-new-buffer "*guile-server*")
   :command (list
             "guix" "shell" "guile-next" "guile-ares-rs" "--"
             "guile" "-c" "((@ (ares server) run-nrepl-server))")))

(use-package yasnippet
  :if EXTERNAL-PACKAGES?
  :delight yas-minor-mode
  :hook ((prog-mode conf-mode text-mode) . yas-minor-mode))

(defvar-local elogind-inhibit-process nil)
(use-package compile
  :commands compilation-next-file compilation-previous-file
  ;; dumb autoload to avoid warnings
  :autoload elogind-inhibit-compilation elogind-inhibit-kill
  :custom
  (compilation-max-output-line-length nil)
  (compilation-ask-about-save nil)
  :hook (compilation-filter . ansi-color-compilation-filter)
  :bind (:map compilation-mode-map
              ("c" . compile))
  :config
  (defun elogind-inhibit-compilation (_process)
    "Run elogind-inhibit for the duration of the compilation."
    (when elogind-inhibit-process
      (elogind-inhibit-kill nil nil))
    (setq elogind-inhibit-process
          (make-process
           :name "elogind-inhibit for compilation"
           :command '("elogind-inhibit" "--who=emacs-compilation" "sleep" "infinity"))))

  (defun elogind-inhibit-kill (&optional buffer _status)
    "Kill elogind-inhibit when the compilation is finished."
    (with-current-buffer (or buffer (current-buffer))
      (when elogind-inhibit-process
        (kill-process elogind-inhibit-process))
      (setq elogind-inhibit-process nil)))

  (add-hook 'compilation-mode-hook
            (lambda ()
              (add-hook 'kill-buffer-hook #'elogind-inhibit-kill nil t)))
  (add-hook 'compilation-start-hook #'elogind-inhibit-compilation)
  (add-to-list 'compilation-finish-functions #'elogind-inhibit-kill))

(defun set-emacs-lisp-compile-command ()
  "Set elisp compile command to run checkdoc and `native-compile'."
  (setq-local compile-command
              (string-join
               (list "emacs -Q --batch"
                     (shell-quote-argument "--eval=(setq byte-compile-warnings 'all)")
                     (concat "--eval="
                             (shell-quote-argument
                              (concat "(checkdoc-file \"" buffer-file-name "\")")))
                     "-f batch-native-compile"
                     buffer-file-name)
               " ")))
(add-hook 'emacs-lisp-mode-hook #'set-emacs-lisp-compile-command)

(use-package grep
  :custom
  (grep-highlight-matches 'always)
  (grep-use-headings t)
  :config
  (keymap-set grep-mode-map "C-c C-n" #'compilation-next-file)
  (keymap-set grep-mode-map "C-c C-p" #'compilation-previous-file))

(use-package xref
  :autoload xref--search-property
  ;; dumb autoload to avoid warnings
  xref-next-group-no-show xref-prev-group-no-show
  :custom
  (xref-search-program 'ripgrep)
  :config
  ;; TODO: try to upstream these functions
  (defun xref-next-group-no-show ()
    "Move to the first item of the next xref group but don't display its source."
    (interactive)
    (xref--search-property 'xref-group)
    (xref--search-property 'xref-item))

  (defun xref-prev-group-no-show ()
    "Move to the first item of the previous xref group but don't display its source."
    (interactive)
    ;; Search for the xref group of the current item, provided that the
    ;; point is not already in an xref group.
    (unless (plist-member (text-properties-at (point)) 'xref-group)
      (xref--search-property 'xref-group t))
    ;; Search for the previous xref group.
    (xref--search-property 'xref-group t)
    (xref--search-property 'xref-item))

  ;; TODO: tell upstream to rename this to an external name
  (keymap-set xref--xref-buffer-mode-map "C-c C-n" #'xref-next-group-no-show)
  (keymap-set xref--xref-buffer-mode-map "C-c C-p" #'xref-prev-group-no-show))

;;; Programming Section Ends


;;; VC/Diffs Section Begins

(use-package diff-hl
  :if EXTERNAL-PACKAGES?
  :functions global-diff-hl-mode
  :config
  (global-diff-hl-mode))

(setopt vc-handled-backends '(Git))
(setopt auto-revert-check-vc-info t)
(setopt vc-log-short-style '(directory file))
(setopt vc-git-annotate-switches '("-w" "-C" "-C" "-C"))
(setopt vc-git-print-log-follow t)
(setopt vc-log-finish-functions nil)  ; no buffer resizing!
(setopt vc-diff-finish-functions nil) ; no buffer resizing!

(with-eval-after-load "vc-git"
  (add-hook 'log-edit-done-hook #'unlock-gpg))

(use-package ediff
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-diff-options "-w")
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package diff-mode
  :bind
  (:map diff-mode-map
        ("C-c C-n" . diff-file-next)
        ("C-c C-p" . diff-file-prev)))
;;; VC/Diffs Section Ends


;;; Parens Section Begins
(use-package paren
  :custom
  (show-paren-delay 0)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen t))

(use-package rainbow-delimiters
  :if EXTERNAL-PACKAGES?
  :hook prog-mode)
;;; Parens Section Ends


;;; Whitespace Section Begins
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setopt require-final-newline t)

(use-package ws-butler
  :if EXTERNAL-PACKAGES?
  :delight
  :hook (prog-mode conf-mode text-mode))

(setopt diff-whitespace-style '(face trailing tabs missing-newline-at-eof tab-mark))
(add-hook 'diff-mode-hook #'whitespace-mode)

(setopt adaptive-fill-mode nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;; Whitespace Section Ends


;;; Auto-complete/Hints Section Begins

(global-completion-preview-mode)
(setopt completion-preview-exact-match-only t)

;; otherwise keeps adding a space while doing path completion in eshell which is quite annoying
(setopt pcomplete-termination-string "")

(setopt completions-detailed t)

(use-package minibuffer
  :config
  (add-to-list 'completion-styles 'substring))

(setopt completions-format 'one-column)
(setopt completions-header-format nil)
(setopt completion-show-help nil)
(setopt completion-auto-help 'visible)
(setopt completion-fail-discreetly t)

(setopt completion-ignore-case t)
(setopt read-buffer-completion-ignore-case t)
(setopt read-file-name-completion-ignore-case t)

(setopt history-delete-duplicates t)

(setopt ido-enable-flex-matching t)
(setopt ido-everywhere t)
(setopt ido-auto-merge-work-directories-length -1)
(setopt ido-default-file-method 'selected-window)
(setopt ido-default-buffer-method 'selected-window)
(setopt ido-use-filename-at-point 'guess)
(ido-mode 1)

(use-package which-key
  :delight
  :custom (which-key-idle-secondary-delay 0.05)
  :config (which-key-mode))
;;; Auto-complete/Hints Section Ends


;;; EWW Section Begins
(setopt eww-auto-rename-buffer 'title)

(setopt browse-url-browser-function 'eww-browse-url)
(setopt shr-use-xwidgets-for-media t)
(setopt shr-use-colors nil)
(setopt shr-use-fonts nil)
(setopt shr-indentation 0)
(setopt shr-cookie-policy nil)
(setopt shr-max-width nil)
(setopt shr-width nil)
;; Send user agent as many sites require it
(setopt url-privacy-level '(email os emacs lastloc cookies))

(setopt eww-use-browse-url "\\`\\(?:gemini\\|gopher\\|mailto\\|magnet\\):\\|\\(youtube.com\\|youtu.be\\)\\|\\.\\(?:mp[34]\\|torrent\\)\\'")
(setopt browse-url-handlers
        '(("\\`\\(gemini\\|gopher\\)://" .
           (lambda (host-or-url &rest _) (elpher-go host-or-url)))
          ("\\`magnet:\\|\\.torrent\\'" .
           (lambda (host-or-url &rest _) (transmission-add host-or-url)))
          ("\\`https?://.*\\.\\(webm\\|m[kp][34v]\\)\\'" .
           (lambda (host-or-url &rest _)
             (message "Downloading file")
             (make-process
              :name "wget" :buffer (generate-new-buffer "*wget*")
              :command (list
                        "wget"
                        (concat "--directory-prefix=" (xdg-user-dir "DOWNLOAD"))
                        "--progress=dot:mega"
                        host-or-url)
              :sentinel (lambda (_ ret_str) (message "Download: %s" ret_str)))))
          ("\\(youtube.com\\|youtu.be\\)" .
           (lambda (host-or-url &rest _)
             (message "Downloading file")
             (make-process
              :name "yt-dlp" :buffer (generate-new-buffer "*yt-dlp*")
              :command (list "yt-dlp" host-or-url)
              :sentinel (lambda (_ ret_str) (message "Download: %s" ret_str)))))))

(use-package elpher
  :if EXTERNAL-PACKAGES?
  ;; make keybindings like eww
  :bind (:map elpher-mode-map
              ("p" . elpher-back)))
;;; EWW Section Ends


;;; auth Section Begins

(setopt epg-user-id user-mail-address)
(setopt epg-pinentry-mode 'loopback)
(setopt epg-gpg-home-directory (getenv "GNUPGHOME"))

(use-package pinentry
  :if (and EXTERNAL-PACKAGES? (not IS-INSIDE-EMACS))
  :custom (pinentry-popup-prompt-window nil)
  :functions pinentry-start
  :config (pinentry-start))

;;; auth Section Ends

(use-package erc
  :if nil
  :custom
  (erc-nick "morgan")
  (erc-user-full-name "Morgan")
  (erc-server "irc.libera.chat")
  (erc-port 6667)
  (erc-anonymous-login t)
  (erc-prompt-for-nickserv-password nil)
  (erc-log-channels-directory (create-directory "erc-logs" user-emacs-directory))
  (erc-save-buffer-on-part t)
  (erc-header-line-format nil)
  (erc-autojoin-timing 'ident)
  (erc-kill-buffer-on-part t)
  :functions erc-services-mode erc-update-modules
  :config
  (add-to-list 'erc-modules 'keep-place)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))

(use-package ispell
  :config
  (add-to-list 'ispell-skip-region-alist (list "ispell-skip-region-start"
                                               "ispell-skip-region-end")))

(use-package flyspell
  :if (executable-find "aspell")
  :bind ("M-$" . ispell-word)
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :custom
  (flyspell-mode-line-string "")
  (flyspell-use-meta-tab nil)
  (flyspell-check-changes t)
  (flyspell-delay-use-timer t))


;; Backups and auto-saves and deleting

(add-hook 'before-save-hook #'backup-buffer)

(setopt backup-directory-alist
        `((".*" . ,(create-directory "backups" user-emacs-directory))))
(setopt auto-save-file-name-transforms
        `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))
(setopt delete-auto-save-files nil)
(setopt trash-directory (create-directory "trash" user-emacs-directory))
(setopt make-backup-files t)
(setopt backup-by-copying t)
(setopt version-control t)
(setopt vc-make-backup-files t)
(setopt delete-old-versions -1)
(setopt delete-by-moving-to-trash t)

(setopt custom-file (expand-file-name "custom-garbage" trash-directory))

(use-package eshell
  :bind
  (("s-<return>" . eshell)
   ("s-RET"      . eshell))
  :hook
  ;; Save command history when commands are entered
  (eshell-pre-command . eshell-save-some-history)
  :custom
  (eshell-banner-message "")
  (eshell-history-size nil) ;; Pull history size from environment variables
  ;; XXX: doesn't error when set to nil and HISTFILE isn't set
  (eshell-history-file-name nil) ;; Pull history file from environment variables
  (eshell-history-append t)
  (eshell-hist-ignoredups t)
  (eshell-cp-overwrite-files nil)
  (eshell-mv-overwrite-files nil)
  (eshell-destroy-buffer-when-process-dies t))

(use-package eshell-syntax-highlighting
  :if EXTERNAL-PACKAGES?
  :hook eshell-mode)

(use-package em-term
  :config
  (add-to-list 'eshell-visual-commands "pulsemixer")
  (add-to-list 'eshell-visual-commands "alsamixer")
  (add-to-list 'eshell-visual-commands "weechat")
  (add-to-list 'eshell-visual-commands "pw-top"))

(setopt comint-pager "cat")

(use-package dired
  :commands dired-goto-file
  :bind
  (:map dired-mode-map
        ("C-c e a" . emms-add-dired)
        ("C-c e p" . emms-play-dired))
  :hook
  ((dired-mode . turn-on-gnus-dired-mode)
   (dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-listing-switches
   "-l --all --group-directories-first --si --sort=version")
  (dired-filename-display-length 'window)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-create-destination-dirs 'ask)
  (wdired-allow-to-change-permissions t))

(autoload 'image-dired-original-file-name "image-dired-util")
(autoload 'image-dired-associated-dired-buffer "image-dired-util")
(use-package image-dired
  :functions dired-do-rename image-dired-rename-file
  :defines ido-use-filename-at-point
  :config
  (defun image-dired-rename-file ()
    (interactive nil image-dired-thumbnail-mode image-dired-image-mode)
    (image-dired--do-mark-command nil nil
      (let ((dired-dwim-target nil)
            (ido-use-filename-at-point nil))
        (dired-do-rename))))
  (keymap-set image-dired-thumbnail-mode-map "R" #'image-dired-rename-file))

(setopt tramp-default-method "ssh")
(setopt remote-file-name-inhibit-cache nil)
(setopt tramp-use-ssh-controlmaster-options nil) ;; Use system settings
(setopt vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

(use-package tramp
  :defines tramp-remote-path
  :config
  ;; guix system bin
  (add-to-list 'tramp-remote-path "/run/current-system/profile/bin")
  (add-to-list 'tramp-remote-path "/run/privileged/bin"))

(setopt time-stamp-format "%Y-%02m-%02d %3a %02H:%02M")
(add-hook 'before-save-hook 'time-stamp)

;; doesn't let me jump to errors :/
;; (add-hook 'ledger-report-mode-hook 'compilation-minor-mode)
(use-package ledger-mode
  :if EXTERNAL-PACKAGES?
  :mode "\\.ledger\\'"
  :custom
  (ledger-default-date-format ledger-iso-date-format))

(use-package ledger-flymake
  :if EXTERNAL-PACKAGES?
  :hook (ledger-mode . ledger-flymake-enable)
  :custom (ledger-binary-path "ledger"))

(use-package autoinsert
  :custom
  (auto-insert-query nil)
  (auto-insert t)
  :config
  (mapc
   (lambda (x) (push x auto-insert-alist))
   '((("\\.sh\\'" . "Shell Script")
      nil
      "#!/bin/sh\n"
      comment-start "Time-stamp: <>\n"
      '(copyright) "\n")
     (("\\.org\\'" . "Org mode file")
      nil
      "#+title: " (file-name-base buffer-file-name) "\n"
      "Time-stamp: <>\n")
     (("\\.scm\\'" . "Guile Script")
      nil
      "#!/usr/bin/env sh\n"
      "# -*- mode: scheme; -*-\n"
      "exec guile -s \"$0\" \"$@\"\n"
      "!#")))
  (auto-insert-mode 1))

(setopt copyright-names-regexp "Morgan Smith")
(setenv "ORGANIZATION" "Morgan Smith")

(keymap-global-set "C-c o" #'dictionary-lookup-definition)
(setopt dictionary-server "localhost")
(setopt dictionary-use-single-buffer t)

;; Use C-x r j to jump to a register
(mapc
 (lambda (args)
   (set-register (car args) (cons 'file (cdr args))))
 (list
  (cons ?c (locate-user-emacs-file "init.el"))
  (cons ?d (xdg-user-dir "DOWNLOAD"))
  (cons ?i (expand-file-name "inbox.org" org-directory))
  (cons ?j (expand-file-name "wiki/journal.org" org-directory))
  (cons ?m (expand-file-name "money/money.ledger" org-directory))
  (cons ?o (expand-file-name "wiki/contributions.org" org-directory))
  (cons ?p (expand-file-name "wiki/possessions.org" org-directory))
  (cons ?r (expand-file-name "wiki/routines.org" org-directory))
  (cons ?t (expand-file-name "agenda/todo.org" org-directory))))

(setopt register-use-preview nil)

(use-package doc-view
  :bind
  (:map doc-view-mode-map
        ("r" . image-rotate)
        ("<remap> <fit-window-to-buffer>" . doc-view-fit-window-to-page))
  :config
  (defun doc-view-toggle-use-svg ()
    "Sometimes SVG doesn't work."
    (interactive nil doc-view-mode)
    (setq doc-view-mupdf-use-svg (not doc-view-mupdf-use-svg)))

  (defun doc-view-invert-face ()
    "Applying dark theme to SVGs often hides stuff and looks wrong."
    (interactive nil doc-view-mode)
    (let* ((default-foreground (face-attribute 'default :foreground))
           (default-background (face-attribute 'default :background))
           (invert? (eq default-foreground (face-attribute 'doc-view-svg-face :foreground)))
           (new-foreground (if invert? default-background default-foreground))
           (new-background (if invert? default-foreground default-background)))
      (set-face-foreground 'doc-view-svg-face new-foreground)
      (set-face-background 'doc-view-svg-face new-background))
    (revert-buffer-quick)))

(setopt image-use-external-converter t)

(setopt ibuffer-expert t)
(setopt ibuffer-saved-filter-groups
        `(("default"
           ("erc" (mode . erc-mode))
           ("emacs" (or
                     (name . "*scratch*")
                     (name . "*Warnings*")
                     (name . "*Flymake log*")
                     (mode . pinentry-prompt-mode)
                     (mode . debugger-mode)
                     (mode . messages-buffer-mode)
                     (mode . completion-list-mode)
                     (mode . native-comp-limple-mode)))
           ("gnus/debbugs" (or
                            (mode . message-mode)
                            (mode . bbdb-mode)
                            (mode . mail-mode)
                            (mode . gnus-group-mode)
                            (mode . gnus-summary-mode)
                            (mode . gnus-article-mode)
                            (name . "^\\.bbdb$")
                            (name . "^\\.newsrc-dribble")
                            (mode . debbugs-gnu-mode)))
           ("repl things" (or
                      (name . "*guile-server*")
                      (mode . arei-connection-mode)
                      (mode . geiser-repl-mode)
                      (mode . geiser-messages-mode)
                      (mode . geiser-debug-mode)
                      (mode . inferior-emacs-lisp-mode)))
           ("lisp" (or
                    (mode . emacs-lisp-mode)
                    (mode . scheme-mode)
                    (mode . lisp-mode)))
           ("internet" (or
                        (mode . eww-mode)
                        (mode . eww-history-mode)
                        (mode . eww-bookmark-mode)))
           ("agenda" (or
                      ,@(mapcar (lambda (file) `(filename . ,file)) org-agenda-files)
                      (mode . diary-mode)
                      (mode . org-agenda-mode)))
           ("dired" (mode . dired-mode))
           ("commands" (or
                        (name . ,shell-command-buffer-name-async)
                        (name . ,shell-command-buffer-name)
                        (mode . shell-mode)
                        (name . "*Async-native-compile-log*")))
           ("DocView" (mode . doc-view-mode))
           ("GDB" (or
                   (mode . gud-mode)
                   (mode . gdb-locals-mode)
                   (mode . gdb-inferior-io-mode)
                   (mode . gdb-threads-mode)
                   (mode . gdb-memory-mode)
                   (mode . gdb-disassembly-mode)
                   (mode . gdb-breakpoints-mode)
                   (mode . gdb-frames-mode)
                   (mode . gdb-registers-mode)))
           ("help" (or
                    (mode . help-mode)
                    (mode . Info-mode))))))

(use-package ibuffer
  :functions ibuffer-switch-to-saved-filter-groups
  :config
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))


(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-z")

(global-auto-composition-mode -1)

(setopt repeat-exit-timeout 5)
(with-eval-after-load "em-prompt"
  ;; I want to use C-n and C-p for next and previous line
  (setq eshell-prompt-repeat-map nil))

(repeat-mode 1)

;;; EMMS
(autoload 'emms-volume-pulse-change "emms-volume-pulse")
(autoload 'emms-player-simple-regexp "emms-player-simple")
(autoload 'emms-add-playlist "emms-source-playlist")
(defvar emms-info-functions)
(use-package emms
  :if EXTERNAL-PACKAGES?
  :commands emms-stop emms-playlist-current-clear emms-shuffle emms-start
  :autoload emms-track-type emms-track-set emms-track-name
  :bind
  (("C-c p"     . emms-playlist-mode-go)
   ("s-p"       . emms-pause)
   ("s-<right>" . emms-next)
   ("s-<left>"  . emms-previous)
   ("s-<up>"    . emms-volume-raise)
   ("s-<down>"  . emms-volume-lower)))

(use-package emms-setup
  :after emms
  :autoload emms-all
  ;; dumb autoload to avoid warnings
  emms-info-file-name-base
  :config
  (emms-all)

  (setopt emms-playing-time-display-mode nil)
  (setopt emms-player-list '(;; emms-player-mpd
                             emms-player-mpv))
  (setopt emms-player-mpd-music-directory (xdg-user-dir "MUSIC"))

  (defun emms-info-file-name-base (track)
    "The TRACK file base name."
    (when (eq 'file (emms-track-type track))
      (emms-track-set track 'info-title
                      (file-name-base (emms-track-name track)))))
  (setopt emms-info-functions nil)
  (push #'emms-info-file-name-base emms-info-functions)

  ;; (setopt emms-repeat-playlist t)

  ;; TODO: add normal mixer support to emms-volume-amixer-change.  Currently it
  ;; only supports simple mixers
  (setopt emms-volume-change-function #'emms-volume-pulse-change)

  (setopt emms-volume-change-amount 5)
  (setopt emms-mode-line-format " %s")

  ;; TODO: auto get decoders from server
  ;; (emms-player-mpd-send "decoders" nil (lambda (_ string) (print string)))
  (setopt emms-player-mpd-supported-regexp
          (emms-player-simple-regexp
           "m3u" "ogg" "flac" "mp3" "wav" "mod" "au" "aiff"
           "opus" "m4a" "webm")))

(defun music-setup ()
  "Setup my music."
  (interactive)
  (emms-stop)
  (emms-playlist-current-clear)
  (emms-add-playlist (expand-file-name "playlist.pls" (xdg-user-dir "MUSIC")))
  (emms-shuffle)
  (emms-start))

(use-package abbrev
  :delight)

(when (and (display-graphic-p) (featurep 'pgtk))
  ;; Wayland pgtk stuff
  (setopt pgtk-wait-for-event-timeout nil)
  (defun fix-input () "." (pgtk-use-im-context nil))
  (add-hook 'emacs-startup-hook 'fix-input))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(autoload 'url-cookie-delete-cookies "url-cookie")
(autoload 'url-gc-dead-buffers "url")
(autoload 'org-persist-gc "org-persist")
(autoload 'diff--cache-clean "diff-mode")
(autoload 'profiler-reset "profiler")
(defun cleanup (&rest _ignore)
  "Cleanup stuff."
  (interactive)
  (setq debug-on-error nil)
  (setq debug-on-quit nil)
  (cancel-debug-on-entry)
  (cancel-debug-on-variable-change)

  (save-some-buffers (not (called-interactively-p 'any)))
  (when (fboundp 'eglot-shutdown-all)
    (eglot-shutdown-all))
  (mapc #'kill-buffer (match-buffers "^\\*disk-usage"))
  (mapc #'kill-buffer (match-buffers "^ \\*org-src-fontification:"))
  (when dired-buffers
    (mapc #'kill-buffer (mapcar #'cdr dired-buffers)))
  (mapc
   (lambda (buffer)
     (ignore-errors
       (kill-buffer buffer)))
   '("*Async-native-compile-log*"
     "*Backtrace*"
     "*Completions*"
     "*Dictionary*"
     "*Flymake log*"
     "*Help*"
     "*info*"
     "*Messages*"
     "*Multiple Choice Help*"
     "*Native-compile-Log*"
     "*gcc-flymake*"
     "*Org Clock*"
     "*Bugs*" ;; debbugs
     ;; TODO: why does `log-edit-kill-buffer' hide this buffer instead of
     ;; killing it?
     "*log-edit-files*"
     "*vc*"))
  ;; De-duplicate HISTFILE
  (with-current-buffer (find-file-noselect (getenv "HISTFILE"))
    (delete-trailing-whitespace (point-min) (point-max))
    (delete-duplicate-lines (point-min) (point-max))
    (save-buffer)
    (kill-current-buffer))
  ;; TODO: tell upstream to expose function
  (diff--cache-clean)
  (profiler-reset)
  (native-compile-prune-cache)
  (url-cookie-delete-cookies)
  (url-gc-dead-buffers)
  (org-persist-gc)
  (garbage-collect))

(autoload 'dbus-register-signal "dbus")
(defun register-cleanup-dbus ()
  "Run cleanup function on lock, sleep, and shutdown."
 (when (featurep 'dbusbind)
  (dbus-register-signal :system
                        "org.freedesktop.login1"
                        ;; TODO: Hard coded c1 session
                        "/org/freedesktop/login1/session/c1"
                        "org.freedesktop.login1.Session"
                        "Lock"
                        #'cleanup)
  (dbus-register-signal :system
                        "org.freedesktop.login1"
                        "/org/freedesktop/login1"
                        "org.freedesktop.login1.Manager"
                        "PrepareForSleep"
                        #'cleanup)
  (dbus-register-signal :session
                        "org.freedesktop.login1"
                        "/org/freedesktop/login1"
                        "org.freedesktop.login1.Manager"
                        "PrepareForShutdown"
                        #'cleanup)))
(add-hook 'emacs-startup-hook 'register-cleanup-dbus)

(use-package viper
  :if nil
  :custom
  (viper-expert-level 5) ;; viper-max-expert-level == 5
  (viper-inhibit-startup-message t)
  (viper-want-ctl-h-help t)
  (viper-ex-style-editing nil)
  (viper-no-multiple-ESC nil)
  (viper-syntax-preference 'emacs)
  (viper-vi-style-in-minibuffer nil)
  :init
  (copy-face 'default 'viper-minibuffer-vi)
  (copy-face 'default 'viper-minibuffer-insert)
  (copy-face 'default 'viper-minibuffer-emacs))

(use-package osm
  :if EXTERNAL-PACKAGES?
  :custom
  (osm-copyright nil)
  :bind
  (:map osm-mode-map
        (("<remap> <next-line>"     . osm-down)
         ("<remap> <previous-line>" . osm-up)
         ("<remap> <forward-char>"  . osm-right)
         ("<remap> <backward-char>" . osm-left)
         ("=" . osm-zoom-in)))
  :config
  (add-to-list 'osm-server-list
               '(arcgisonline :name "arcgisonline" :description "satellite map" :url
                              "https://services.arcgisonline.com/ArcGis/rest/services/World_Imagery/MapServer/tile/%z/%y/%x.jpeg"
                              :group "Satallite" :copyright ("idk")
                              :min-zoom 0 :max-zoom 23  :subdomains ("nope"))))

;; https://github.com/minad/osm/discussions/39
(autoload 'org-element-map "org-element")
(autoload 'org-element-parse-buffer "org-element")
(autoload 'osm--bookmark-record "osm")
(autoload 'osm--bookmark-name "osm")
(defun osm-override-bookmarks ()
  "Override bookmarks with links in current org buffer."
  (when (eq (with-current-buffer (window-buffer) major-mode) 'org-mode)
    (make-local-variable 'bookmark-alist)
    (setq bookmark-alist '())
    (let ((bookmark-save-flag nil))
      (org-element-map (with-current-buffer (window-buffer) (org-element-parse-buffer))
          '(link)
        (lambda (elem)
          (let ((url (org-element-property :path elem)))
            (if (and (string-equal "geo" (org-element-property :type elem))
                     ;; string-match bit copied from osm.el
                     (string-match
                      "\\`\\([0-9.-]+\\),\\([0-9.-]+\\)\\(?:,[0-9.-]+\\)?\\(;.+\\'\\|\\'\\)" url))
                (let* ((lat (string-to-number (match-string 1 url)))
                       (lon (string-to-number (match-string 2 url)))
                       (args (url-parse-args (match-string 3 url) ""))
                       (osm--zoom (cdr (assoc "z" args)))
                       ;; (server (cdr (assoc "s" args)))
                       (name (car (org-element-contents elem)))
                       (bookmark-make-record-function
                        (lambda () (osm--bookmark-record name lat lon name))))
                  (bookmark-set
                   (osm--bookmark-name lat lon name))))))))))

(add-hook 'osm-mode-hook 'osm-override-bookmarks)

(provide 'init.el)
;;; init.el ends here
