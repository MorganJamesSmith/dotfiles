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
  (unless default-dir
    (setq default-dir "~"))
  (let ((directory (file-name-as-directory (expand-file-name dir default-dir))))
    (unless (file-exists-p directory)
      (make-directory directory t))
    directory))

;; Don't be so in my face with issues
(setopt warning-minimum-level :emergency)

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

(setopt shell-kill-buffer-on-exit t)

(setopt view-read-only t)

(savehist-mode 1)
(recentf-mode)

;; Use ibuffer
(keymap-global-set "C-x C-b" #'ibuffer)

(keymap-global-set "C-c c" #'compile)
(setopt compilation-scroll-output 'first-error)

(setopt grep-highlight-matches 'always)
(setopt grep-use-headings t)

(setopt read-mail-command 'gnus)
(setopt mail-user-agent 'gnus-user-agent)

(setopt sendmail-program "msmtp"
        send-mail-function #'sendmail-send-it
        message-sendmail-envelope-from 'header)


;; Move gnus folders to the `user-emacs-directory'
(setopt gnus-init-file (expand-file-name "gnus" user-emacs-directory))
(setopt gnus-home-directory (create-directory "gnus-files" user-emacs-directory))
(setopt gnus-directory (create-directory "News" gnus-home-directory))
(setopt mail-source-directory (create-directory "Mail" gnus-home-directory))
(setopt gnus-save-newsrc-file nil)
(setopt gnus-read-newsrc-file nil)

;; Use only encrypted authinfo
(setopt auth-sources `((:source ,(expand-file-name "authinfo.gpg" user-emacs-directory)))
        auth-source-save-behavior t
        auth-source-gpg-encrypt-to (list user-mail-address))

(setopt browse-url-default-scheme "https")

(setopt text-quoting-style 'grave)

;; Date should always be big to small (year/month/day)
(setopt calendar-date-style 'iso)

(setopt fill-column 79)

(setopt completions-detailed t)

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
                                (agenda-structure . (2.5))))
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

(setopt tab-bar-format '(tab-bar-format-global))
(tab-bar-mode)
;;; Modeline/Tab Bar Section Ends


;;; Org Section Begins
;; Useful for more then just org
(keymap-global-set "C-c ." #'org-time-stamp)
(keymap-global-set "C-c !" #'org-time-stamp-inactive)
(keymap-global-set "C-c C-o" #'org-open-at-point)

(setopt org-directory "~/documents/"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-preview-latex-image-directory "~/.cache/org-preview-latex/"
        org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
        org-duration-format 'h:mm
        org-log-done 'time
        org-edit-src-content-indentation 0
        org-use-speed-commands t
        org-src-ask-before-returning-to-edit-buffer nil
        org-src-window-setup 'current-window
        org-special-ctrl-a/e t
        org-fold-catch-invisible-edits 'show-and-error
        org-todo-keywords
        '((sequence "TODO" "DONE")
          (sequence "HABIT" "DONE")
          (sequence "DAYOF" "DONE")))

(setopt org-html-preamble nil
        org-html-postamble nil
        org-html-head-include-default-style nil
        org-html-meta-tags nil)

(with-eval-after-load "org"
  (push 'org-habit org-modules)
  (push "SHOWFROMTIME" org-default-properties)
  (push "SHOWFROMDATE" org-default-properties)
  (push "EXCEPTIONS" org-default-properties)

  ;; I keep accidentally archiving stuff
  (keymap-unset org-mode-map "C-c C-x C-s"))

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
 org-agenda-dim-blocked-tasks nil
 org-agenda-inhibit-startup t
 org-agenda-use-tag-inheritance nil
 org-agenda-ignore-properties '(effort appt stats category)

 org-agenda-sticky t
 org-agenda-format-date "%F %A"
 org-agenda-show-outline-path nil
 org-agenda-block-separator nil
 org-agenda-scheduled-leaders '("" "")
 org-agenda-deadline-leaders '("" "" "")
 org-agenda-start-on-weekday nil
 org-agenda-use-time-grid nil
 org-agenda-time-leading-zero t
 org-agenda-window-setup 'current-window
 org-agenda-todo-keyword-format ""
 org-agenda-remove-tags t

 org-agenda-files
 (list
  (expand-file-name "agenda/daily.org" org-directory)
  (expand-file-name "agenda/events.org" org-directory)
  (expand-file-name "agenda/timetracking.org" org-directory)
  (expand-file-name "agenda/todo.org" org-directory))

 org-agenda-custom-commands
 '(("o" "My Agenda"
    ((todo
      "TODO|DAYOF"
      ((org-agenda-overriding-header "Todo:")
       (org-agenda-prefix-format "%s")
       (org-agenda-todo-ignore-deadlines 'future)
       (org-agenda-todo-ignore-scheduled 'future)
       (org-agenda-todo-ignore-timestamp 'future)
       (org-agenda-skip-function
        ;; Low priority is shown in "Eventually maybe" section
        '(org-agenda-skip-entry-if 'regexp "\\[#C\\]"))))
     (agenda
      ""
      (;; I do the header funny to avoid an extra newline
       (org-agenda-overriding-header (lambda () "Today's Habits: "))
       (org-agenda-format-date "")
       (org-agenda-prefix-format "")
       (org-agenda-span 'day)
       (org-habit-show-all-today t)
       (org-habit-clock-completes-habit t)
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'nottodo '("HABIT")))))
     (agenda
      ""
      ((org-agenda-overriding-header "Schedule:")
       (org-agenda-prefix-format "    %-12t| %?-12:c %s")
       (org-agenda-span 60)
       (org-deadline-warning-days 0)
       (org-agenda-include-diary t) ;; For holidays
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'todo '("DONE" "HABIT" "DAYOF")))))
     (todo
      "TODO"
      ((org-agenda-overriding-header "Eventually maybe:")
       (org-agenda-prefix-format "%?T%s")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'notregexp "\\[#C\\]"))))
     (agenda
      ""
      ((org-agenda-overriding-header "Time Tracking:")
       (org-agenda-prefix-format "%-11t | %-17s | ")
       (org-agenda-show-all-dates nil)
       (org-agenda-show-log 'clockcheck)))))))

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
  (if (or (string= state "HABIT")
          (string= state "DAYOF"))
      "DONE"
    state))
(org-clock-persistence-insinuate)

;; TODO: package org-passwords for guix
;; (keymap-global-set "C-c q" #'org-passwords)
;; (with-eval-after-load "org-passwords"
;;   (keymap-set org-passwords-mode-map "C-c u" #'org-passwords-copy-username)
;;   (keymap-set org-passwords-mode-map "C-c p" #'org-passwords-copy-password))

(setopt org-passwords-file (expand-file-name "codes.gpg" org-directory))
;;; Org Section Ends


;;; Programming Section Begins
(delight 'emacs-lisp-mode nil 'elisp-mode)
(delight 'eldoc-mode nil 'eldoc)

(add-hook 'prog-mode-hook #'elide-head-mode)

;; Handy keybinds are
;; M-.     xref-find-definitions
;; M-,     xref-pop-marker-stack
;; M-?     xref-find-references
;; C-M-.   xref-find-apropos
(add-hook
 'c-mode-common-hook
 (lambda ()
   (ggtags-mode 1)
   (setq-local imenu-create-index-function #'ggtags-build-imenu-index)))
(setopt ggtags-enable-navigation-keys nil)

(delight 'ggtags-mode nil 'ggtags)

(add-hook 'prog-mode-hook #'flymake-mode)
(with-eval-after-load "flymake"
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

  (setopt flymake-mode-line-format
          '("" flymake-mode-line-exception
            flymake-mode-line-counters)))

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

(global-cwarn-mode)
(delight 'cwarn-mode nil 'cwarn)

(which-function-mode)
(electric-layout-mode)
(electric-pair-mode)
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

(setopt gdb-many-windows t)

;; Guix development
(setopt geiser-repl-history-filename
        (expand-file-name "geiser_history" user-emacs-directory))

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/src/guix"))

(setopt geiser-mode-smart-tab-p t)

(setopt org-link-elisp-confirm-function nil)
(setopt org-link-descriptive nil)
(setopt org-link-abbrev-alist
        `(("guix" . "elisp:(guix-packages-by-name \"%s\")")
          ("possessions" . "file:~/documents/wiki/possessions.org::*")
          ("money" . ,(concat "file:" (getenv "LEDGER_FILE") "::[[possessions:%s]]"))))


(add-hook 'scheme-mode-hook #'guix-devel-mode)
(setopt scheme-program-name "guile")
(setopt scheme-mit-dialect nil)


(delight 'yas-minor-mode nil 'yasnippet)
(yas-global-mode 1)
;;; Programming Section Ends


;;; VC/Diffs Section Begins

(global-diff-hl-mode)

(setopt vc-handled-backends '(Git))
(setopt auto-revert-check-vc-info t)
(setopt vc-log-short-style '(directory file))
(setopt vc-git-annotate-switches "-w")

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


(setopt ws-butler-global-exempt-modes '(eshell-mode gnus-mode))
(ws-butler-global-mode)
(delight 'ws-butler-mode nil 'ws-butler)


(setopt adaptive-fill-mode nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'message-mode-hook 'turn-on-auto-fill)
;;; Whitespace Section Ends


;;; Auto-complete/Hints Section Begins

;; Ignore compiled guile files
(add-to-list 'completion-ignored-extensions ".go")

(setopt completion-styles '(basic partial-completion emacs22 substring))

(setopt completions-format 'one-column)
(setopt completions-header-format nil)
(setopt completion-show-help nil)
(setopt completion-auto-help 'visible)

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
(setopt url-privacy-level 'high)

(defun my-shr-url-transformer (url)
  "Transform URL."
  (let ((urlobj (url-generic-parse-url url)))
    (setf (url-host urlobj)
          (pcase (url-host urlobj)
            ((or "www.reddit.com" "reddit.com") "teddit.net")
            ((or "www.twitter.com" "twitter.com") "nitter.net")
            (_ (url-host urlobj))))
    ;; TODO: I'd like to use https when available but this fails when a site only offers http
    ;; (setf (url-type urlobj)
    ;;       (pcase (url-type urlobj)
    ;;         ("http" "https")
    ;;         (_ (url-type urlobj))))

    (url-recreate-url urlobj)))

(setopt shr-url-transformer #'my-shr-url-transformer)
(with-eval-after-load "eww"
  (add-to-list 'eww-url-transformers #'my-shr-url-transformer))

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
(add-hook 'message-mode-hook #'flyspell-mode)
(setopt flyspell-mode-line-string "")
(setopt flyspell-use-meta-tab nil)


;; Backups and auto-saves and deleting

(add-hook 'before-save-hook #'backup-buffer)

(setopt backup-directory-alist
        `(("." . ,(create-directory "backups" user-emacs-directory))))
(setopt auto-save-file-name-transforms
        `(("." ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))
(setopt trash-directory (create-directory "trash" user-emacs-directory))
(setopt make-backup-files t)
(setopt backup-by-copying t)
(setopt version-control t)
(setopt vc-make-backup-files t)
(setopt delete-old-versions -1)
(setopt auto-save-default t)
(setopt auto-save-timeout 20)
(setopt auto-save-interval 200)
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

(setenv "PAGER" (executable-find "cat"))

(eshell-syntax-highlighting-global-mode +1)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(setopt dired-dwim-target t)
(setopt dired-recursive-copies 'always)
(setopt dired-recursive-deletes 'always)
(setopt dired-listing-switches
        "-l --all --group-directories-first --si --sort=version")
(setopt dired-omit-line dired-re-dot)

(setopt dired-create-destination-dirs 'ask)

(add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(load "dired-x")

(setopt dired-guess-shell-alist-user
        `((,(regexp-opt '(".amv" ".avi" ".flv" ".mkv" ".mov" ".mp4" ".webm" ".m4v" ".wav" ".mp3" ".opus" ".ogv" ".flac")) "mpv")
          (,(regexp-opt '(".pdf")) "pdftotext")))
(setopt dired-omit-size-limit nil)

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
  (add-to-list 'tramp-remote-path "/run/current-system/profile/bin"))

(load "tramp") ;; else sudo won't work

(autoload 'xdg-user-dir "xdg")

;; Tell gpg what screen to use for pinentry
;; (shell-command "gpg-connect-agent \"UPDATESTARTUPTTY\" /bye")


(setopt time-stamp-format "%Y-%02m-%02d %3a %02H:%02M")
(add-hook 'before-save-hook 'time-stamp)

(with-eval-after-load "disk-usage"
  (defun disk-usage-filter-proc (path _attributes)
    (not (string-match "\\(^/proc\\|:/proc\\)" path)))
  (add-to-list 'disk-usage-available-filters 'disk-usage-filter-proc)
  (add-to-list 'disk-usage-default-filters 'disk-usage-filter-proc))

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
  (cons ?m (expand-file-name "money/money.ledger" org-directory))
  (cons ?i (expand-file-name "inbox.org" org-directory))
  (cons ?t (expand-file-name "agenda/todo.org" org-directory))
  (cons ?c (locate-user-emacs-file "init.el"))
  (cons ?d (xdg-user-dir "DOWNLOAD"))
  (cons ?p (expand-file-name "wiki/possessions.org" org-directory))))


(with-eval-after-load "doc-view"
  (keymap-set doc-view-mode-map "r" #'image-rotate))

(setopt doc-view-imenu-flatten t)

;; mupdf 1.21.1 doesn't produce valid svg's
(setopt doc-view-mupdf-use-svg nil)
(setopt doc-view-resolution 400)

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
           ("geiser" (or
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
                      (mode . diary-mode)
                      (mode . org-agenda-mode)))
           ("dired" (mode . dired-mode))
           ("commands" (or
                        (name . ,shell-command-buffer-name-async)
                        (name . ,shell-command-buffer-name)
                        (mode . shell-mode)
                        (name . "*Async-native-compile-log*")))
           ("PDF" (or
                   (mode . pdf-view-mode)
                   (mode . pdf-outline-buffer-mode)))
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

;; EMMS
(keymap-global-set "s-p" #'emms-pause)
(keymap-global-set "s-<right>" #'emms-next)
(keymap-global-set "s-<left>" #'emms-previous)
(keymap-global-set "s-<up>" #'emms-volume-raise)
(keymap-global-set "s-<down>" #'emms-volume-lower)

(emms-all)
(setopt emms-playing-time-display-mode nil)
(setopt emms-player-list '(emms-player-mpd
                           emms-player-mpv))
(setopt emms-player-mpd-music-directory (xdg-user-dir "MUSIC"))
(setopt emms-repeat-playlist t)
(setopt emms-info-functions nil)

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
  (emms-add-directory-tree emms-player-mpd-music-directory)
  (emms-shuffle)
  (emms-start))

(keymap-global-set "s-<return>" #'eshell)
(keymap-global-set "s-RET" #'eshell)

(delight 'abbrev-mode nil 'abbrev)

;; Wayland pgtk stuff
(defun fix-input () (pgtk-use-im-context nil))
(add-hook 'emacs-startup-hook 'fix-input)


(keymap-global-set "<remap> <narrow-to-region>" #'logos-narrow-dwim)
(keymap-global-set "<remap> <forward-page>" #'logos-forward-page-dwim)
(keymap-global-set "<remap> <backward-page>" #'logos-backward-page-dwim)
(setopt logos-outlines-are-pages t)

(with-eval-after-load "osm"
  (setopt osm-copyright nil)
  (keymap-set osm-mode-map "<remap> <next-line>" #'osm-down)
  (keymap-set osm-mode-map "<remap> <previous-line>" #'osm-up)
  (keymap-set osm-mode-map "<remap> <forward-char>" #'osm-right)
  (keymap-set osm-mode-map "<remap> <backward-char>" #'osm-left)
  (keymap-set osm-mode-map "=" #'osm-zoom-in))


(provide 'init.el)
;;; init.el ends here
