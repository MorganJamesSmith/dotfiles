;;; init.el --- My personal init file -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Morgan Smith

;;; Commentary:

;; This is my personal Emacs init file.
;; It is crafted with love, care, and stupidity.
;; Use at your own risk.

;;; Code:

(setopt source-directory "~/src/emacs/emacs")

;; This is non-nil for instances of Emacs started from within an
;; instance of Emacs
(defconst IS-INSIDE-EMACS (getenv "INSIDE_EMACS"))

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

;; Don't be so in my face with issues
(setopt warning-minimum-level :emergency)
(setopt process-error-pause-time 0)

;;; Make buffers appear where I want them to
(setopt display-buffer-alist
        (list (list shell-command-buffer-name-async #'display-buffer-no-window)))
(setopt async-shell-command-buffer 'new-buffer)

;;; Optimization Section Begins

;; Display the bare minimum at startup
(setopt inhibit-startup-screen t)
(setopt inhibit-startup-echo-area-message user-login-name)
(setopt inhibit-default-init t)
(setopt initial-major-mode 'fundamental-mode)
(setopt initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

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

;; Use ibuffer
;; TODO: make this respect global-auto-revert-non-file-buffers
;; TODO: make this not jump my cursor around on refresh when window not active
;; (add-hook 'ibuffer-hook 'ibuffer-auto-mode) ;; auto-revert ibuffer
(keymap-global-set "C-x C-b" #'ibuffer)
;; Buffer-menu-group-by

(keymap-global-set "C-c c" #'compile)
(setopt compilation-scroll-output 'first-error)

(setopt grep-highlight-matches 'always)
(setopt grep-use-headings t)

(setopt read-mail-command 'gnus)
(setopt mail-user-agent 'gnus-user-agent)

(setopt sendmail-program "msmtp"
        send-mail-function #'sendmail-send-it
        message-sendmail-envelope-from 'header
        message-interactive nil) ;; prevents lockup from emacs-pinentry

;; Move gnus folders to the `user-emacs-directory'
(setopt gnus-init-file (expand-file-name "gnus" user-emacs-directory))
(setopt gnus-home-directory (create-directory "gnus-files" user-emacs-directory))
(setopt gnus-directory (create-directory "News" gnus-home-directory))
(setopt mail-source-directory (create-directory "Mail" gnus-home-directory))
(setopt gnus-save-newsrc-file nil)
(setopt gnus-read-newsrc-file nil)

(add-hook 'message-mode-hook 'footnote-mode)
(setopt mm-uu-hide-markers nil)
(setopt mml-attach-file-at-the-end t)

(setopt imenu-flatten 'prefix)
(setopt imenu-space-replacement nil)
(setopt imenu-auto-rescan t)
(setopt imenu-auto-rescan-maxout 50000000)

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

(setopt minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode)
(setopt extended-command-suggest-shorter nil)
(setopt kill-do-not-save-duplicates t)
(setopt kill-read-only-ok t)
(setopt save-interprogram-paste-before-kill t)
(setopt kill-whole-line t)
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
(setopt modus-themes-bold-constructs t)
(setopt modus-themes-prompts '(bold))
(setopt modus-themes-org-blocks 'tinted-background)
(setopt modus-themes-headings '((0 . (2.5))
                                (agenda-structure . (1.5))))
(load-theme 'modus-vivendi t)

(setopt proced-enable-color-flag t)

;; I dislike gui stuff
(setopt use-file-dialog nil)
(setopt use-dialog-box nil)
(setopt visible-bell t)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
;;; Pretty Visuals Section Ends


;;; Modeline/Tab Bar Section Begins
(setopt display-time-default-load-average nil)
(setopt display-time-24hr-format t)
(setopt display-time-day-and-date t)
(display-time-mode)

(display-battery-mode)
(size-indication-mode)
(column-number-mode)

(setopt mode-line-compact 'long)

(setopt tab-bar-format '(tab-bar-format-global)
        tab-bar-auto-width nil)
(tab-bar-mode)
;;; Modeline/Tab Bar Section Ends


;;; Org Section Begins
;; Useful for more then just org
(keymap-global-set "C-c ." #'org-timestamp)
(keymap-global-set "C-c !" #'org-timestamp-inactive)

(setopt org-fast-tag-selection-single-key t)
(setopt org-tag-alist '((:startgrouptag)
                        ("computer")
                        (:grouptags)
                        ("research")
                        (:endgrouptag)
                        (:startgrouptag)
                        ("event" . ?e)
                        (:grouptags)
                        ("programming_social")
                        ("shopping")
                        ("festival")
                        (:endgrouptag)
                        (:startgroup)
                        ("goals" . ?g)
                        (:grouptags)
                        ("annual_goal")
                        ("monthly_goal")
                        ("weekly_goal")
                        ("daily_goal" . ?d)
                        (:endgroup)
                        (:startgrouptag)
                        ("health")
                        (:grouptags)
                        ;; ("sleep")
                        ("exercise")
                        ("food")
                        ("mental_health")
                        (:endgrouptag)
                        ("tinkering" . ?t)
                        ("ignore")))

(setopt org-directory "~/documents/"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-preview-latex-image-directory "~/.cache/org-preview-latex/"
        org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
        org-duration-format 'h:mm
        org-log-done 'time
        org-edit-src-content-indentation 0
        org-use-speed-commands t ; org-speed-commands
        org-src-ask-before-returning-to-edit-buffer nil
        org-src-window-setup 'current-window
        org-read-date-popup-calendar nil
        org-special-ctrl-a/e t
        org-fold-catch-invisible-edits 'show-and-error
        org-enforce-todo-dependencies t
        org-todo-keywords
        '((sequence "TODO" "|" "DONE" "FAILED")
          (sequence "WAITINGFOR" "DONE")
          (sequence "HABIT" "DONE")
          (sequence "PROJECT" "DONE")))

(setopt org-html-preamble nil
        org-html-postamble nil
        org-html-head-include-default-style nil
        org-html-meta-tags nil)

(with-eval-after-load "org"
  (push 'org-habit org-modules)

  ;; I keep accidentally archiving stuff
  (keymap-unset org-mode-map "C-c C-x C-s")

  (keymap-set org-mode-map "M-p" #'org-metaup)
  (keymap-set org-mode-map "M-n" #'org-metadown))

(with-eval-after-load "org-indent"
  (org-indent-mode -1))


;; For when I use org-babel to create images
(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

(keymap-global-set "C-c a" (lambda () (interactive) (org-agenda nil "o")))

(setopt
 calendar-holidays
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
   (holiday-fixed 12 26 "Boxing Day (Federal and Ontario Holiday)"))

 ;; Optimization
 org-agenda-inhibit-startup t
 org-agenda-skip-comment-trees nil
 org-agenda-skip-archived-trees nil

 org-agenda-sticky t
 org-agenda-prefix-format " "
 org-agenda-format-date "%F %A"
 org-agenda-show-outline-path nil
 org-agenda-block-separator ""
 org-agenda-scheduled-leaders '("" "")
 org-agenda-deadline-leaders '("" "" "")
 org-agenda-start-on-weekday nil
 org-agenda-use-time-grid nil
 org-agenda-time-leading-zero t
 org-agenda-window-setup 'current-window
 org-agenda-todo-keyword-format ""
 org-agenda-remove-tags t
 org-agenda-tags-todo-honor-ignore-options t
 org-agenda-remove-timeranges-from-blocks t
 org-agenda-skip-timestamp-if-deadline-is-shown t
 org-agenda-skip-scheduled-if-deadline-is-shown t
 org-agenda-skip-scheduled-repeats-after-deadline t
 org-agenda-skip-deadline-if-done t
 org-agenda-sorting-strategy '(time-up priority-down tag-up category-keep)
 org-stuck-projects '("TODO=\"PROJECT\"" ("TODO") nil "")

 org-agenda-files
 (list
  (expand-file-name "contacts.org" org-directory)
  (expand-file-name "agenda/daily.org" org-directory)
  (expand-file-name "agenda/events.org" org-directory)
  (expand-file-name "agenda/timetracking.org" org-directory)
  (expand-file-name "agenda/todo.org" org-directory))

 org-agenda-custom-commands
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
       (org-agenda-prefix-format " %-4e |%l")
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
       (org-agenda-files (list (expand-file-name "agenda/timetracking.org" org-directory)))))))))

(setopt org-habit-show-done-always-green t)
(setopt org-habit-graph-column 29)
(setopt org-habit-following-days 3)
(setopt org-habit-preceding-days (- 103 org-habit-following-days org-habit-graph-column))
;; org-habit has too many colors.  Use fewer
(setopt face-remapping-alist '((org-habit-clear-face . org-habit-ready-face)
                               (org-habit-alert-future-face . org-habit-overdue-face)
                               (org-habit-clear-future-face . org-habit-ready-future-face)))

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

(with-eval-after-load "org-clock"
  ;; This sorting doesn't work if there are multiple un-compacted levels.  But
  ;; as far as I can tell there is no built-in way to sort in that scenario
  (plist-put org-clocktable-defaults :sort '(2 . ?T))
  (plist-put org-clocktable-defaults :compact t)
  ;; Same width as header so tables are always the same width regardless of
  ;; contents
  (plist-put org-clocktable-defaults :narrow '12!)
  (plist-put org-clocktable-defaults :formula '%)
  (plist-put org-clocktable-defaults :match "-ignore")
  (plist-put org-clocktable-defaults :maxlevel 1))

;; org-capture-before-finalize-hook
(keymap-global-set "C-c c" #'org-capture)
(setopt org-capture-bookmark nil)
(setopt org-capture-templates
        '(
          ("i" "Inbox" entry (file "inbox.org") "* %?" :prepend t)
          ("w" "Weight"
           table-line (file+headline "wiki/morgan.org" "weight")
           "| %? | %U |" :jump-to-captured t :prepend t)
          ("m" "Mood"
           table-line (file+headline "wiki/morgan.org" "mood")
           "| %? | %U |" :jump-to-captured t :prepend t)
          ("a" "Alertness"
           table-line (file+headline "wiki/morgan.org" "alertness")
           "| %? | %U |" :jump-to-captured t :prepend t)))

(setopt org-link-elisp-confirm-function nil)
(setopt org-link-descriptive nil)
(setopt org-link-abbrev-alist
        `(("guix" . "elisp:(guix-packages-by-name \"%s\")")
          ("possessions" . "file:~/documents/wiki/possessions.org::*")
          ("money" . ,(concat "file:" (getenv "LEDGER_FILE") "::[[possessions:%s]]"))))

(setopt org-imenu-depth 5)

;; Half my 1920x1080 screen
(setopt org-plot/gnuplot-term-extra "size 960,1080")
;; Add a nice grid
(setopt org-plot/gnuplot-script-preamble "set grid")

(defun replace-region-with-org-timestamp (beg end)
  "Replace region with an org timestamp."
  (interactive "r")
  (kill-region beg end)
  (let* ((time-string (downcase (car kill-ring)))
         (correction (or (and (string-suffix-p "pm" time-string) (* 60 60 12))
                         0)))
   (org-insert-timestamp (time-add (date-to-time time-string) correction) t t)))
;;; Org Section Ends


;;; Programming Section Begins

;; TODO: I love using buffer-env to get dependencies but I wish I could enable
;; it for specific directories
;; TODO: check out `hack-dir-local-get-variables-functions'
(defun buffer-env-setup ()
  "."
  (interactive)
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  (add-hook 'comint-mode-hook #'hack-dir-local-variables-non-file-buffer))

(defun buffer-env-go-away ()
  "."
  (interactive)
  (remove-hook 'hack-local-variables-hook #'buffer-env-update)
  (remove-hook 'comint-mode-hook #'hack-dir-local-variables-non-file-buffer))

;; Ignore translation files
(setopt project-vc-ignores (list "*.po"))

(delight 'emacs-lisp-mode nil 'elisp-mode)
(delight 'eldoc-mode nil 'eldoc)

(add-hook 'ielm-mode-hook 'eldoc-mode)

(add-hook 'prog-mode-hook #'elide-head-mode)

;; Handy keybinds are
;; M-.     xref-find-definitions
;; M-,     xref-pop-marker-stack
;; M-?     xref-find-references
;; C-M-.   xref-find-apropos
(add-hook
 'c-mode-common-hook
 (lambda ()
   (cwarn-mode 1)
   (ggtags-mode 1)
   (setq-local imenu-create-index-function #'ggtags-build-imenu-index)))
(setopt ggtags-enable-navigation-keys nil)

(delight 'cwarn-mode nil 'cwarn)
(delight 'ggtags-mode nil 'ggtags)

(add-hook 'prog-mode-hook #'flymake-mode)
(with-eval-after-load "flymake"
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

  (setopt flymake-mode-line-format
          '("" flymake-mode-line-exception
            flymake-mode-line-counters)))

(setopt flymake-show-diagnostics-at-end-of-line t)

(defun my-flymake-cc-command ()
  "Use the makefile at project root."
  (unless (executable-find "make") (error "Make not found"))
  `("make"
    "-s" "-C" ,(project-root (project-current t))
    "check-syntax"
    ,(format "CHK_SOURCES=-x %s -c -"
             (cond ((derived-mode-p 'c++-mode) "c++")
                   (t "c")))))

(setopt flymake-cc-command 'my-flymake-cc-command)

(which-function-mode)
(electric-layout-mode)
(electric-pair-mode)
;; (add-to-list 'electric-pair-pairs '(?\' . ?\'))
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

;; Save all buffers on compile automatically
(setopt compilation-ask-about-save nil)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

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

(setopt gdb-many-windows t)

(require 'debbugs-gnu)
(add-hook 'log-view-mode-hook #'bug-reference-mode)
(setopt bug-reference-url-format "https://debbugs.gnu.org/%s")
;; Show feature requests.
(setq debbugs-gnu-default-severities
      '("serious" "important" "normal" "minor" "wishlist"))

(setopt scheme-program-name "guile")
(setopt scheme-mit-dialect nil)
(setopt geiser-mode-auto-p nil)
(defalias 'pcomplete/guix #'ignore) ;; Freezes up eshell
(require 'arei)

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

(delight 'yas-minor-mode nil 'yasnippet)
(yas-global-mode 1)

(defun set-texinfo-compile-command ()
  "Set texinfo compile command to run proselint."
  (setq-local compile-command
              (string-join
               (list "proselint" buffer-file-name)
               " ")))
(add-hook 'texinfo-mode-hook #'set-texinfo-compile-command)
;;; Programming Section Ends


;;; VC/Diffs Section Begins

(global-diff-hl-mode)

(setopt vc-handled-backends '(Git))
(setopt auto-revert-check-vc-info t)
(setopt vc-log-short-style '(directory file))
(setopt vc-git-annotate-switches '("-w" "-C" "-C" "-C"))
(setopt vc-git-print-log-follow t)
(setopt vc-log-finish-functions nil)  ; no buffer resizing!
(setopt vc-diff-finish-functions nil) ; no buffer resizing!

(setopt ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-diff-options "-w"
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally)
;;; VC/Diffs Section Ends


;;; Parens Section Begins
(setopt show-paren-delay 0
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;; Parens Section Ends


;;; Whitespace Section Begins
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(electric-indent-mode 1)
(setopt require-final-newline t)


(setopt ws-butler-global-exempt-modes
        '(eshell-mode gnus-mode org-agenda-mode
          minibuffer-inactive-mode minibuffer-mode))

(ws-butler-global-mode)
(delight 'ws-butler-mode nil 'ws-butler)

(setopt diff-whitespace-style '(face trailing tabs missing-newline-at-eof tab-mark))
(add-hook 'diff-mode-hook #'whitespace-mode)

(setopt adaptive-fill-mode nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;; Whitespace Section Ends


;;; Auto-complete/Hints Section Begins

;; Ignore compiled guile files
(add-to-list 'completion-ignored-extensions ".go")

(global-completion-preview-mode)

(setopt completions-detailed t)

(setopt completion-styles '(basic partial-completion emacs22 substring))

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

(delight 'which-key-mode nil 'which-key)
(setopt which-key-idle-secondary-delay 0.05)
(which-key-mode)
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

;; Elpher
;; make keybindings like eww
(with-eval-after-load "elpher"
  (keymap-set elpher-mode-map "p" #'elpher-back))
;;; EWW Section Ends


;;; auth Section Begins

(setopt epg-user-id user-mail-address)
(setopt epg-pinentry-mode 'loopback)
(setopt epg-gpg-home-directory (getenv "GNUPGHOME"))

(setopt pinentry-popup-prompt-window nil)
(when (not IS-INSIDE-EMACS)
  (setenv "INSIDE_EMACS" emacs-version)
  (pinentry-start))
;;; auth Section Ends

(setopt erc-nick "morgan")
(setopt erc-user-full-name "Morgan")
(setopt erc-server "irc.libera.chat")
(setopt erc-port 6667)
(setopt erc-anonymous-login t)
(setopt erc-prompt-for-nickserv-password nil)
(setopt erc-log-channels-directory (create-directory "erc-logs" user-emacs-directory))
(setopt erc-save-buffer-on-part t)
(setopt erc-header-line-format nil)
(setopt erc-autojoin-timing 'ident)
(setopt erc-kill-buffer-on-part t)


(with-eval-after-load "erc"
  (add-to-list 'erc-modules 'keep-place)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'flyspell-mode)
(setopt flyspell-mode-line-string "")
(setopt flyspell-use-meta-tab nil)
(setopt flyspell-check-changes t)
(add-to-list 'ispell-skip-region-alist (list "ispell-skip-region-start"
                                             "ispell-skip-region-end"))
(keymap-global-set "M-$" #'ispell-word)


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


;; Save command history when commands are entered
(add-hook 'eshell-pre-command-hook #'eshell-save-some-history)

(setopt eshell-banner-message "")
(setopt eshell-history-size nil) ;; Pull history size from environment variables
;; XXX: doesn't error when set to nil and HISTFILE isn't set
(setopt eshell-history-file-name nil) ;; Pull history file from environment variables
(setopt eshell-hist-ignoredups 'erase)
(setopt eshell-cp-overwrite-files nil)
(setopt eshell-mv-overwrite-files nil)

(with-eval-after-load "eshell"
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(with-eval-after-load "em-term"
  (add-to-list 'eshell-visual-commands "pulsemixer")
  (add-to-list 'eshell-visual-commands "alsamixer")
  (add-to-list 'eshell-visual-commands "weechat")
  (add-to-list 'eshell-visual-commands "pw-top"))

(setopt comint-pager "cat")

(keymap-global-set "s-<return>" #'eshell)
(keymap-global-set "s-RET" #'eshell)

(add-hook 'eshell-mode-hook #'eshell-syntax-highlighting-mode)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load "dired"
  (keymap-set dired-mode-map "C-c e a" #'emms-add-dired)
  (keymap-set dired-mode-map "C-c e p" #'emms-play-dired))

(setopt dired-dwim-target t)
(setopt dired-recursive-copies 'always)
(setopt dired-recursive-deletes 'always)
(setopt dired-listing-switches
        "-l --all --group-directories-first --si --sort=version")
(setopt dired-filename-display-length 'window)

(setopt dired-create-destination-dirs 'ask)
(setopt wdired-allow-to-change-permissions t)
(add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

;; TODO: why don't this work!
;; (setopt image-dired-thumbnail-storage 'standard)
(setopt image-dired-rotate-original-ask-before-overwrite nil)

(setopt tramp-default-method "ssh")
(setopt remote-file-name-inhibit-cache nil)
(setopt tramp-use-ssh-controlmaster-options nil) ;; Use system settings
(setopt vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

(with-eval-after-load "tramp"
  ;; guix system bin
  (add-to-list 'tramp-remote-path "/run/current-system/profile/bin")
  (add-to-list 'tramp-remote-path "/run/setuid-programs"))


(load "tramp") ;; else sudo won't work

(autoload 'xdg-user-dir "xdg")

(setopt time-stamp-format "%Y-%02m-%02d %3a %02H:%02M")
(add-hook 'before-save-hook 'time-stamp)

(add-to-list 'auto-mode-alist '("\\.ledger\\'" . hledger-mode))
(setopt hledger-jfile (expand-file-name "money/money.ledger" org-directory))
(setopt hledger-currency-string "$")
(setopt hledger-comments-column 4)


(setopt auto-insert-query nil)
(setopt auto-insert t)
(with-eval-after-load "autoinsert"
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
      "!#"))))
(auto-insert-mode 1)

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

(with-eval-after-load "doc-view"
  (keymap-set doc-view-mode-map "r" #'image-rotate)
  (keymap-set doc-view-mode-map "<remap> <fit-window-to-buffer>" #'doc-view-fit-window-to-page)

  ;; Applying dark theme to SVGs often hides stuff and looks wrong so just don't
  (set-face-foreground 'doc-view-svg-face "black")
  (set-face-background 'doc-view-svg-face "white"))

(setopt doc-view-imenu-flatten t)


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
                      (mode . arei-connection-mode)
                      (mode . geiser-repl-mode)
                      (mode . geiser-messages-mode)
                      (mode . geiser-debug-mode)))
           ("lisp" (or
                    (mode . emacs-lisp-mode)
                    (mode . scheme-mode)
                    (mode . lisp-mode)))
           ("internet" (or
                        (mode . eww-mode)
                        (mode . eww-history-mode)
                        (mode . eww-bookmark-mode)))
           ("agenda" (or
                      (and
                       (filename . "/agenda/")
                       (mode . org-mode))
                      (filename . "contacts.org")
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

(with-eval-after-load "ibuffer"
  (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default"))))


(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-z")

(global-auto-composition-mode -1)

(setopt repeat-exit-timeout 5)
(with-eval-after-load "em-prompt"
  ;; I want to use C-n and C-p for next and previous line
  (setq eshell-prompt-repeat-map nil))

(repeat-mode 1)

;;; EMMS
(require 'emms)
(require 'emms-setup)
(emms-all)
(keymap-global-set "C-c p" #'emms-playlist-mode-go)
(keymap-global-set "s-p" #'emms-pause)
(keymap-global-set "s-<right>" #'emms-next)
(keymap-global-set "s-<left>" #'emms-previous)
(keymap-global-set "s-<up>" #'emms-volume-raise)
(keymap-global-set "s-<down>" #'emms-volume-lower)

(setopt emms-playing-time-display-mode nil)
(setopt emms-player-list '(;; emms-player-mpd
                           emms-player-mpv))
(setopt emms-player-mpd-music-directory (xdg-user-dir "MUSIC"))
(setopt emms-info-functions nil)
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
         "opus" "m4a" "webm"))

(defun music-setup ()
  "Setup my music."
  (interactive)
  (emms-stop)
  (emms-playlist-current-clear)
  (emms-add-playlist (expand-file-name "playlist.pls" (xdg-user-dir "MUSIC")))
  (emms-shuffle)
  (emms-start))

(delight 'abbrev-mode nil 'abbrev)

;; Wayland pgtk stuff
(defun fix-input () "." (pgtk-use-im-context nil))
(add-hook 'emacs-startup-hook 'fix-input)

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun cleanup ()
  "Cleanup stuff."
  (interactive)
  (buffer-env-go-away)
  (save-some-buffers)
  (mapc #'kill-buffer (match-buffers "^ \\*diff-syntax"))
  (mapc #'kill-buffer (match-buffers "^\\*disk-usage"))
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
     "*vc*"))
  (setq values '()) ;; TODO: is this a good idea?
  (native-compile-prune-cache)
  (url-cookie-delete-cookies)
  (url-gc-dead-buffers)
  (org-persist-gc)
  (garbage-collect))

;;; viper Section Begins
(setopt viper-expert-level 5) ;; viper-max-expert-level == 5
(setopt viper-inhibit-startup-message t)

(setopt viper-want-ctl-h-help t)
(setopt viper-ex-style-editing nil)
(setopt viper-no-multiple-ESC nil)
(setopt viper-syntax-preference 'emacs)
(setopt viper-vi-style-in-minibuffer nil)

(copy-face 'default 'viper-minibuffer-vi)
(copy-face 'default 'viper-minibuffer-insert)
(copy-face 'default 'viper-minibuffer-emacs)
;;; viper Section Ends

(with-eval-after-load "osm"
  (setopt osm-copyright nil)
  (keymap-set osm-mode-map "<remap> <next-line>" #'osm-down)
  (keymap-set osm-mode-map "<remap> <previous-line>" #'osm-up)
  (keymap-set osm-mode-map "<remap> <forward-char>" #'osm-right)
  (keymap-set osm-mode-map "<remap> <backward-char>" #'osm-left)
  (keymap-set osm-mode-map "=" #'osm-zoom-in)
  (add-to-list 'osm-server-list
               '(arcgisonline :name "arcgisonline" :description "satellite map" :url
                              "https://services.arcgisonline.com/ArcGis/rest/services/World_Imagery/MapServer/tile/%z/%y/%x.jpeg"
                              :group "Satallite" :copyright ("idk")
                              :min-zoom 0 :max-zoom 23  :subdomains ("nope"))))

;; https://github.com/minad/osm/discussions/39
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
