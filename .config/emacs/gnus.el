;; gnus.el --- My gnus config -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my gnus config.
;; It's mostly for email.

;;; Code:

(require 'nnmaildir)
(require 'smtpmail)
(require 'gnus-sum)

;; Exit gnus on Emacs exit
(defun exit-gnus-on-exit ()
  "Exits gnus non-interactively."
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))
(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

(customize-set-variable 'send-mail-function         'smtpmail-send-it) ; not for gnus
(customize-set-variable 'message-send-mail-function 'smtpmail-send-it)

(customize-set-variable 'gnus-cache-enter-articles '(ticked dormant unread read))
(customize-set-variable 'gnus-cache-remove-articles nil)
(customize-set-variable 'gnus-cacheable-groups "^nnimap")
(customize-set-variable 'gnus-use-cache t)

(customize-set-variable 'gnus-asynchronous t)
(customize-set-variable 'gnus-async-prefetch-article-p (lambda (article) t))
(customize-set-variable 'gnus-prefetched-article-deletion-strategy nil)
(customize-set-variable 'gnus-use-article-prefetch t)
(customize-set-variable 'gnus-use-header-prefetch t)

(customize-set-variable 'mail-source-directory (file-name-as-directory (expand-file-name "Mail" gnus-home-directory)))
(customize-set-variable 'nnml-directory "~/.config/emacs/gnus-files/News/cache/")
(unless (file-exists-p mail-source-directory)
  (make-directory mail-source-directory t))

(customize-set-variable 'mail-source-delete-incoming nil)

;; I don't want to use a .newsrc file
(customize-set-variable 'gnus-save-newsrc-file nil)
(customize-set-variable 'gnus-read-newsrc-file nil)


(customize-set-variable 'gnus-inhibit-user-auto-expire t)


;; Select first article instead of first unread
(customize-set-variable 'gnus-auto-select-subject 'first)
(customize-set-variable 'gnus-summary-goto-unread 'never)

;; All groups are new each time gnus is run
(customize-set-variable 'gnus-save-killed-list nil)

(customize-set-variable 'gnus-ignored-newsgroups nil)

;; Subscribe to new groups alphabetically
(customize-set-variable 'gnus-subscribe-newsgroup-method #'gnus-subscribe-alphabetically)

(customize-set-variable 'gnus-message-archive-group nil)

;; Encrypt email by default and also encrypt to self
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)
(customize-set-variable 'mml-secure-openpgp-encrypt-to-self t)


;; Always show all my groups
(customize-set-variable 'gnus-permanently-visible-groups ".*")


(defvar user-mail-address)
(defvar smtpmail-smtp-user)
(defvar smtpmail-smtp-server)
(defvar smtpmail-smtp-service)
(setq gnus-newsgroup-variables '(user-mail-address smtpmail-smtp-user smtpmail-smtp-server smtpmail-smtp-service))
(customize-set-variable 'gnus-parameters
                        `((".*" (display . all))
                          (".*work.*"
                           (user-mail-address ,(auth-source-pass-get 'secret "email/work/address"))
                           (smtpmail-smtp-user ,(auth-source-pass-get 'secret "email/work/address"))
                           (smtpmail-smtp-server ,(auth-source-pass-get 'secret "email/work/smtp-server"))
                           (smtpmail-smtp-service  ,(string-to-number (auth-source-pass-get 'secret "email/work/smtp-service"))))
                          (".*morganjsmith.*"
                           (user-mail-address "Morgan.J.Smith@outlook.com")
                           (smtpmail-mail-address "Morgan.J.Smith@outlook.com")
                           (smtpmail-smtp-server "smtp-mail.outlook.com")
                           (smtpmail-smtp-service 587))
                          (".*morganjsmith:emacs.*"
                           (to-list . "emacs-devel@gnu.org"))
                          (".*cmail.*"
                           (user-mail-address "MorganSmith@cmail.carleton.ca")
                           (smtpmail-mail-address "MorganSmith@cmail.carleton.ca")
                           (smtpmail-smtp-server "smtp-mail.outlook.com")
                           (smtpmail-smtp-service 587))
                          (".*grommin.*"
                           (user-mail-address "grommin@hotmail.com")
                           (smtpmail-mail-address "grommin@hotmail.com")
                           (smtpmail-smtp-server "smtp-mail.outlook.com")
                           (smtpmail-smtp-service 587))
                          (".*hotbutterypancake.*"
                           (user-mail-address "hotbutterypancake@gmail.com")
                           (smtpmail-mail-address "hotbutterypancake@gmail.com")
                           (smtpmail-smtp-server "smtp.gmail.com")
                           (smtpmail-smtp-service 587))))


(customize-set-variable 'gnus-select-method '(nnnil ""))
(customize-set-variable 'gnus-secondary-select-methods
                        `((nnmaildir "morganjsmith"
                                     (directory "~/.local/share/mail/morganjsmith/"))
                          (nnmaildir "cmail"
                                     (directory "~/.local/share/mail/cmail/"))
                          (nnmaildir "grommin"
                                     (directory "~/.local/share/mail/grommin/"))
                          (nnmaildir "hotbutterpancake"
                                     (directory "~/.local/share/mail/hotbutterypancake/"))
                          (nnimap "work"
                                  (nnimap-user ,(auth-source-pass-get 'secret "email/work/address"))
                                  (nnimap-address ,(auth-source-pass-get 'secret "email/work/imap-server"))
                                  (nnimap-server-port ,(string-to-number (auth-source-pass-get 'secret "email/work/imap-port")))
                                  (nnimap-authenticator xoauth2))))



;; Make stuff pretty section

 ;; Split windows horizontally instead of vertically when reading articles

(when (display-graphic-p)
    (customize-set-variable 'gnus-sum-thread-tree-indent "  ")
    (customize-set-variable 'gnus-sum-thread-tree-root "● ")
    (customize-set-variable 'gnus-sum-thread-tree-false-root "◯ ")
    (customize-set-variable 'gnus-sum-thread-tree-single-indent "◎ ")
    (customize-set-variable 'gnus-sum-thread-tree-vertical        "│")
    (customize-set-variable 'gnus-sum-thread-tree-leaf-with-other "├─► ")
    (customize-set-variable 'gnus-sum-thread-tree-single-leaf     "╰─► "))

(customize-set-variable 'gnus-summary-line-format
                        (concat
                         "%0{%U%R%z%}"
                         "%3{│%}" "%1{%&user-date;%}" "%3{│%}" ;; date
                         "  "
                         "%4{%-20,20f%}"             ;; name
                         "  "
                         "%3{│%}"
                         " "
                         "%1{%B%}"
                         "%s\n"))

(customize-set-variable 'gnus-user-date-format-alist
                        '((t . "%d-%b %Y, %H:%M")))

(customize-set-variable 'gnus-group-line-format "%M%S%p%P%5y:%B%(%c%)\n")

(customize-set-variable 'gnus-summary-display-arrow t)

(provide 'my-gnus.el)
;;; gnus.el ends here
