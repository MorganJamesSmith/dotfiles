;; gnus.el --- My gnus config -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my gnus config.
;; It's mostly for email.

;;; Code:

(require 'nnmaildir)
(require 'smtpmail)
(require 'gnus-sum)
(require 'gnus-agent)

;; Exit gnus on Emacs exit
(defun exit-gnus-on-exit ()
  "Exits gnus non-interactively."
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))
(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

;; Perfer plain text email
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(customize-set-variable 'send-mail-function         'smtpmail-send-it) ; not for gnus
(customize-set-variable 'message-send-mail-function 'smtpmail-send-it)

;; I don't want to use a .newsrc file
(customize-set-variable 'gnus-save-newsrc-file nil)
(customize-set-variable 'gnus-read-newsrc-file nil)

;; Read the entire active file because I would like everything
(customize-set-variable 'gnus-read-active-file t)

;; Update marks on the server as well
(customize-set-variable 'gnus-use-backend-marks t)

;; Don't ask how many messages I want to see. I want them all
(customize-set-variable 'gnus-large-newsgroup nil)

;; Sanitize the mail a little
(add-hook 'nnmail-prepare-incoming-header-hook #'nnmail-remove-leading-whitespace)
(add-hook 'nnmail-prepare-incoming-header-hook #'nnmail-remove-tabs)

;; Select first article instead of first unread
(customize-set-variable 'gnus-auto-select-subject 'first)
(customize-set-variable 'gnus-summary-goto-unread 'never)

;; All groups are new each time gnus is run
(customize-set-variable 'gnus-save-killed-list nil)

;; If you get a new newsgroup, manually run M-x
;; gnus-find-new-newsgroups. New groups will be subscribed to
;; alphabetically
(customize-set-variable 'gnus-check-new-newsgroups nil)
(customize-set-variable 'gnus-subscribe-newsgroup-method #'gnus-subscribe-alphabetically)

(customize-set-variable 'gnus-ignored-newsgroups
                        (regexp-opt
                         '("Notes" "Outbox" "Scheduled" "Calendar" "Contacts"
                           "Conversation" "Clutter" "Journal" "Tasks"
                           "[/Trash" "[/Important" "[/Starred")))

(customize-set-variable 'gnus-message-archive-group nil)

;; Add articles I look at to the cache
(add-hook 'gnus-select-article-hook #'gnus-agent-fetch-selected-article)

;; Make sure we don't expire anything
(customize-set-variable 'gnus-agent-enable-expiration 'DISABLE)
(customize-set-variable 'gnus-agent-expire-days most-positive-fixnum)

(customize-set-variable 'gnus-agent-consider-all-articles t)
(customize-set-variable 'gnus-agent-synchronize-flags t)

(customize-set-variable 'gnus-agent-predicate 'gnus-agent-true)

;; Encrypt email by default and also encrypt to self
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)
(customize-set-variable 'mml-secure-openpgp-encrypt-to-self t)
(customize-set-variable 'mml-secure-smime-encrypt-to-self t)

;; Always decrypt messages without asking me
(customize-set-variable 'mm-decrypt-option 'always)

(customize-set-variable 'gnus-completing-read-function #'gnus-ido-completing-read)


;; Always show all my groups
(customize-set-variable 'gnus-permanently-visible-groups ".*")

(customize-set-variable 'gnus-parameters '((".*" (display . all))))

(customize-set-variable 'gnus-visible-headers (concat gnus-visible-headers "\\|^Message-ID:"))

;; Receiving email stuff

(customize-set-variable 'gnus-select-method '(nnnil ""))
(customize-set-variable 'gnus-secondary-select-methods
                        `((nnimap "morganjsmith"
                                  (nnimap-user "morgan.j.smith@outlook.com")
                                  (nnimap-address "outlook.office365.com")
                                  (nnimap-authenticator login))
                          (nnimap "cmail"
                                  (nnimap-user "morgansmith@cmail.carleton.ca")
                                  (nnimap-address "outlook.office365.com")
                                  (nnimap-authenticator login))
                          (nnimap "grommin"
                                  (nnimap-user "grommin@hotmail.com")
                                  (nnimap-address "outlook.office365.com")
                                  (nnimap-authenticator login))
                          (nnimap "hotbutterpancake"
                                  (nnimap-user "hotbutterypancake@gmail.com")
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-authenticator login))
                          (nnimap "work"
                                  (nnimap-user ,(auth-source-pass-get "address" "email/work"))
                                  (nnimap-address ,(auth-source-pass-get "imap-server" "email/work"))
                                  (nnimap-server-port ,(string-to-number (auth-source-pass-get "imap-port" "email/work")))
                                  (nnimap-authenticator xoauth2))))

;; Sending email stuff

(defun compose-mail-choose-account ()
  "Call `compose-mail' after setting up the environment for a specific account."
  (interactive)

  (let ((account (ido-completing-read
                  "Choose account: "
                  '("morganjsmith" "cmail" "grommin" "hotbutterypancake" "work"))))
    (cond ((equal account "morganjsmith")
           (setq
            user-mail-address "Morgan.J.Smith@outlook.com"
            smtpmail-smtp-server "smtp.office365.com"
            smtpmail-smtp-service 587
            smtpmail-auth-supported '(login)))
          ((equal account "cmail")
           (setq
            user-mail-address "morgansmith@cmail.carleton.ca"
            smtpmail-smtp-server "smtp.office365.com"
            smtpmail-smtp-service 587
            smtpmail-auth-supported '(login)))
          ((equal account "grommin")
           (setq
            user-mail-address "grommin@hotmail.com"
            smtpmail-smtp-server "smtp.office365.com"
            smtpmail-smtp-service 587
            smtpmail-auth-supported '(login)))
          ((equal account "hotbutterypancake")
           (setq
            user-mail-address "hotbutterypancake@gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587
            smtpmail-auth-supported '(login)))
          ((equal account "work")
           (setq
            user-mail-address (auth-source-pass-get "address" "email/work")
            smtpmail-smtp-server (auth-source-pass-get "smtp-server" "email/work")
            smtpmail-smtp-service  (string-to-number (auth-source-pass-get "smtp-port" "email/work")))
            smtpmail-auth-supported '(xoauth2)))

    (setq smtpmail-smtp-user account)
    (compose-mail)))
(global-set-key (kbd "C-x m") #'compose-mail-choose-account)

;; Make stuff pretty section

 ;; Split windows horizontally instead of vertically when reading articles

(customize-set-variable 'gnus-summary-line-format
                        (concat
                         "%0{%U%R%z%}"
                         "%3{|%}" "%1{%&user-date;%}" "%3{|%}" ;; date
                         "  "
                         "%4{%-20,20f%}"             ;; name
                         "  "
                         "%3{|%}"
                         " "
                         "%1{%B%}"
                         "%s\n"))

(customize-set-variable 'gnus-user-date-format-alist
                        '((t . "%d-%b %Y, %H:%M")))

(customize-set-variable 'gnus-group-line-format "%M%S%p%P%5y:%B%(%c%)\n")

(customize-set-variable 'gnus-summary-display-arrow t)

(provide 'my-gnus.el)
;;; gnus.el ends here
