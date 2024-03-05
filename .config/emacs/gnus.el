;; gnus.el --- My gnus config -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my gnus config.
;; It's mostly for email.

;;; Code:

(require 'gnus)
(require 'gnus-util)

;; Exit gnus on Emacs exit
(defun exit-gnus-on-exit ()
  "Exits gnus non-interactively."
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))

(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

(setopt gnus-dbus-close-on-sleep t)

(setopt gnus-interactive-exit nil)

;; Always do wide replys
(keymap-set gnus-article-mode-map "r" #'gnus-summary-very-wide-reply)
(keymap-set gnus-summary-mode-map "r" #'gnus-summary-very-wide-reply)
(keymap-set gnus-article-mode-map "R" #'gnus-summary-very-wide-reply-with-original)
(keymap-set gnus-summary-mode-map "R" #'gnus-summary-very-wide-reply-with-original)

;; Make it consistent with dired
(keymap-set gnus-summary-mode-map "m" #'gnus-summary-mark-as-processable)
(keymap-set gnus-summary-mode-map "u" #'gnus-summary-clear-mark-forward)

;; Make it consistent with eww
(with-eval-after-load "gnus-art"
  (keymap-set gnus-url-button-map "w" #'gnus-article-copy-string))

(setopt message-generate-hashcash t)

(setopt gnus-summary-stop-at-end-of-message t)

(setopt gnus-treat-unsplit-urls t)

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

(defun get-mail ()
  "Get mail."
  (interactive)
  (let ((processes (list
                    (start-process "mbsync" "*mbsync*" "mbsync" "-a")
                    (start-process "rss2email" "*rss2email*" "r2e" "run"))))
    (dolist (process processes)
      (set-process-sentinel process #'gnus-group-get-new-news))))

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
(setopt gnus-group-line-format "%5y:%B%(%c%)\n")
(setopt gnus-summary-display-arrow t)

(provide 'my-gnus.el)
;;; gnus.el ends here
