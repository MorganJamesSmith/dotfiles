;; gnus.el --- My gnus config -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my gnus config.
;; It's mostly for email.

;;; Code:

(require 'gnus)
(require 'gnus-art)
(require 'gnus-msg)
(require 'gnus-util)
(require 'time-stamp)

;; Exit gnus on Emacs exit
(defun exit-gnus-on-exit ()
  "Exits gnus non-interactively."
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))

(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

;; I want to silence the prompt in `gnus-offer-save-summaries' and in
;; `gnus-group-exit'.  So I need to set `gnus-interactive-exit' to nil and also
;; to 'quiet.  I don't like that I have to do this
(setopt gnus-interactive-exit nil)
(with-eval-after-load "gnus-sum"
  (add-function :around (symbol-function #'gnus-offer-save-summaries)
                (lambda (fun)
                  (let ((gnus-interactive-exit 'quiet))
                    (funcall fun)))))

(with-eval-after-load "gnus-art"
  ;; git send-email --in-reply-to=message-id
  (setopt gnus-visible-headers (concat gnus-visible-headers "\\|^Message-ID:")))

;; Always do wide replys
(with-eval-after-load "gnus-sum"
  (keymap-set gnus-summary-mode-map "r" #'gnus-summary-very-wide-reply)
  (keymap-set gnus-summary-mode-map "R" #'gnus-summary-very-wide-reply-with-original))

(with-eval-after-load "gnus-art"
  (keymap-set gnus-article-mode-map "r" #'gnus-summary-very-wide-reply)
  (keymap-set gnus-article-mode-map "R" #'gnus-summary-very-wide-reply-with-original))

(with-eval-after-load "gnus-sum"
  ;; Make it consistent with dired
  (keymap-set gnus-summary-mode-map "m" #'gnus-summary-mark-as-processable)
  (keymap-set gnus-summary-mode-map "u" #'gnus-summary-clear-mark-forward)

  (keymap-set gnus-summary-mode-map "t" #'gnus-summary-toggle-threads))

(with-eval-after-load "gnus-art"
  ;; Make it consistent with eww
  (keymap-set gnus-url-button-map "w" #'gnus-article-copy-string)

  (keymap-set gnus-article-mode-map "n" #'next-line)
  (keymap-set gnus-article-mode-map "p" #'previous-line))

(setopt gnus-auto-select-next nil)
(setopt gnus-summary-goto-unread 'never)
(setopt gnus-summary-stop-at-end-of-message t)

;; Messes up the display of diffs
;; (setopt gnus-treat-unsplit-urls t)

(setopt gnus-treat-display-smileys nil)

(setopt gnus-treat-strip-trailing-blank-lines t
        gnus-treat-strip-leading-blank-lines t
        gnus-treat-strip-multiple-blank-lines t
        gnus-treat-strip-cr t)

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-file-name-rewrite-functions
               #'mm-file-name-replace-whitespace)
  (add-to-list 'mm-file-name-rewrite-functions
               #'mm-file-name-trim-whitespace))

;; Encrypted emails are sent with the subject "...".  Don't assume they're all
;; part of the same thread
(setopt gnus-summary-gather-exclude-subject "^ *$\\|^...$\\|^(none)$")

(with-eval-after-load "gnus-sum"
  (setopt gnus-simplify-subject-functions (list #'gnus-simplify-subject-re
                                                #'gnus-simplify-all-whitespace)))

(defun gnus-maybe-group-get-new-news (&optional _process _change)
  "Run `gnus-group-get-new-news' if gnus is running."
  (when gnus-newsrc-alist
      (gnus-group-get-new-news)))

(defun mbsync-process-sentinel (process _change)
  "Run after I get mail from running mbsync (PROCESS)."
  (unless (process-live-p process)
    (gnus-maybe-group-get-new-news)
    (let ((exit-status (process-exit-status process)))
      (if (not (eq 0 exit-status))
          (message "mbsync: an error has occured: '%s'" exit-status)
        (with-current-buffer "*mbsync*"
          (goto-char (point-min))
          (search-forward "Channels")
          (message (string-trim
                    (buffer-substring-no-properties
                     (match-beginning 0) (point-max))))
          (kill-buffer (current-buffer)))))))

(defun mbsync ()
  "Run mbsync."
  (interactive)
  (assert-network-connectivity)
  (unlock-gpg)
  (when-let* ((buffer (get-buffer "*mbsync*")))
    (kill-buffer buffer))
  (make-process
   :name "mbsync"
   :buffer "*mbsync*"
   :sentinel #'mbsync-process-sentinel
   :command '("mbsync" "-a")))

(defun rss2email ()
  "Run rss2email."
  (interactive)
  (assert-network-connectivity)
  (when-let* ((buffer (get-buffer "*rss2email*")))
    (kill-buffer buffer))
  (set-process-sentinel
   (start-process "rss2email" "*rss2email*" "r2e" "run")
   #'gnus-maybe-group-get-new-news))

(defun get-mail ()
  "Get mail."
  (interactive)
  (assert-network-connectivity)
  (rss2email)
  (mbsync))

(define-key gnus-group-mode-map (kbd "f") #'get-mail)

;; Don't ask how many messages I want to see. I want them all
(setopt gnus-large-newsgroup nil)

;; Don't check for new newsgroups
(setopt gnus-save-killed-list nil)
(setopt gnus-check-new-newsgroups nil)


(setopt gnus-message-archive-group nil)

;; We don't need to cache since we use a local dovecot server
(setopt gnus-agent nil)
(setopt gnus-use-cache nil)

(setopt mml-secure-openpgp-encrypt-to-self t)
(setopt mml-secure-smime-encrypt-to-self t)

;; Always decrypt messages without asking me
(setopt mm-decrypt-option 'always)

(setopt gnus-completing-read-function #'gnus-ido-completing-read)
;; Always show all my groups
(setopt gnus-permanently-visible-groups ".")

(setopt gnus-parameters '(("." (display . all) (gcc-self . t))
                          ("local" (gnus-show-threads nil) (display . default))))

;; TODO: figure out how to specifiy my nnvirtual groups from here
;; G V (gnus-group-make-empty-virtual)
;; G E (gnus-group-edit-group)
;; (nnvirtual "INBOX\\|Inbox\\|Junk\\|Spam")
;; (nnvirtual "[^l].\\(INBOX\\|Inbox\\|Junk\\|Spam\\)")

;; Receiving email stuff
(setopt gnus-select-method '(nnnil ""))
(setopt
 gnus-secondary-select-methods
 (mapcar
  (lambda (x)
    `(nnimap ,x
             (nnimap-user ,x)
             (nnimap-address "localhost")
             (nnimap-stream network)
             (nnimap-server-port 143)))
  '("cmail" "grommin" "hotbutterypancake" "morganjsmith" "work" "local")))

(setopt gnus-posting-styles
        '(("hotbutterypancake" (address "hotbutterypancake@gmail.com"))
          ("grommin" (address "grommin@hotmail.com"))
          ("morganjsmith" (address "morgan.j.smith@outlook.com"))))

(setopt mail-dont-reply-to-names
        (regexp-opt (mapcar #'cadadr gnus-posting-styles)))

;; Make stuff pretty section

(setopt gnus-summary-line-format
                        (concat
                         "%0{%U%R%z%}"
                         "%3{|%}" "%1{%&user-date;%}" "%3{|%}" ;; date
                         "  "
                         "%4{%-20,20n%}"             ;; name
                         "  "
                         "%3{|%}"
                         " "
                         "%1{%B%}"
                         "%s\n"))

(setopt gnus-article-date-headers '(user-defined))
(setopt gnus-article-time-format time-stamp-format)
(setopt gnus-user-date-format-alist `((t . ,time-stamp-format)))

;; TODO: Emacs master might do unread count correctly since 5c129791c5b
;; It would be cool to add that here
(setopt gnus-group-line-format "%5y:%B%(%c%)\n")
(setopt gnus-summary-display-arrow t)

(defvar gnus-mode-line-string "")
(defun gnus-update-unread-total-modeline ()
  "Put total unread emails in modeline."
  (or
   (and gnus-newsrc-alist ; gnus running?
        (let ((unread (gnus-group-unread "nnvirtual:inbox")))
          (when (and unread (numberp unread) (> unread 0))
            (setq gnus-mode-line-string
                  (concat "[Unread email: " (number-to-string unread) "] "))
            (or (memq 'gnus-mode-line-string global-mode-string)
                (setq global-mode-string
                      (append global-mode-string '(gnus-mode-line-string)))))))
   (setq global-mode-string
         (delq 'gnus-mode-line-string global-mode-string))))

(add-hook 'gnus-group-update-hook #'gnus-update-unread-total-modeline)
(add-hook 'gnus-summary-update-hook #'gnus-update-unread-total-modeline)
(add-hook 'gnus-group-update-group-hook #'gnus-update-unread-total-modeline)
(add-hook 'gnus-after-getting-new-news-hook #'gnus-update-unread-total-modeline)
(add-hook 'gnus-after-exiting-gnus-hook #'gnus-update-unread-total-modeline)

(provide 'my-gnus.el)
;;; gnus.el ends here
