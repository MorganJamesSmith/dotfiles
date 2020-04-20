;; .gnus.el --- My gnus config -*- lexical-binding: t; -*-

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

(setq user-mail-address "Morgan.J.Smith@outlook.com"
      user-full-name    "Morgan Smith")

(setq send-mail-function         'smtpmail-send-it ; not for gnus
      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type       nil
      smtpmail-smtp-server       "smtp-mail.outlook.com"
      smtpmail-smtp-service      587)

(setq smtpmail-debug-info t
      smtpmail-debug-verb t)

(setq mail-source-delete-incoming nil)

;; I don't want to use a .newsrc file
(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)


(setq gnus-inhibit-user-auto-expire t)

(setq gnus-summary-goto-unread 'never)

;; Select first article instead of first unread
(setq gnus-auto-select-subject 'first)


;; All groups are new each time gnus is run
(setq gnus-save-killed-list nil)

;; Subscribe to new groups alphabetically
(setq gnus-subscribe-newsgroup-method #'gnus-subscribe-alphabetically)

;(add-to-list 'gnus-newsgroup-variables 'message-from-style)
(setq gnus-parameters '((".*" (display . all))
                        (".*morganjsmith.*"
                         (expiry-target . "nnmaildir+morganjsmith:Deleted")
                         (user-mail-address . "Morgan.J.Smith@outlook.com")
                         (smtpmail-mail-address . "Morgan.J.Smith@outlook.com")
                         (smtpmail-smtp-server . "smtp-mail.outlook.com")
                         (smtpmail-smtp-service . 587))
                        (".*morganjsmith:emacs.*"
                         (to-list . "emacs-devel@gnu.org"))
                        (".*cmail.*"
                         (expiry-target . "nnmaildir+cmail:Trash")
                         (user-mail-address . "MorganSmith@cmail.carleton.ca")
                         (smtpmail-smtp-server . "smtp-mail.outlook.com")
                         (smtpmail-smtp-service . 587))
                        (".*grommin.*"
                         (expiry-target . "nnmaildir+grommin:Deleted")
                         (user-mail-address . "grommin@hotmail.com")
                         (smtpmail-smtp-server . "smtp-mail.outlook.com")
                         (smtpmail-smtp-service . 587))
                        (".*hotbutterypancake.*"
                         (expiry-target . "nnmaildir+hotbutterypancake:[Gmail].Trash")
                         (user-mail-address . "hotbutterypancake@gmail.com")
                         (smtpmail-smtp-server . "smtp.gmail.com")
                         (smtpmail-smtp-service . 587))))

(setq gnus-permanently-visible-groups ".*")

(setq gnus-use-cache t)


(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '((nnmaildir "morganjsmith"
                   (directory "~/.local/share/mail/morganjsmith/"))
        (nnmaildir "cmail"
                   (directory "~/.local/share/mail/cmail/"))
        (nnmaildir "grommin"
                   (directory "~/.local/share/mail/grommin/"))
        (nnmaildir "hotbutterpancake"
                   (directory "~/.local/share/mail/hotbutterypancake/"))))


;; Make stuff pretty section

 ;; Split windows horizontally instead of vertically when reading articles

(when window-system
    (setq gnus-sum-thread-tree-indent "  ")
    (setq gnus-sum-thread-tree-root "● ")
    (setq gnus-sum-thread-tree-false-root "◯ ")
    (setq gnus-sum-thread-tree-single-indent "◎ ")
    (setq gnus-sum-thread-tree-vertical        "│")
    (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
    (setq gnus-sum-thread-tree-single-leaf     "╰─► "))

(setq gnus-summary-line-format
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

(setq gnus-user-date-format-alist
      '((t . "%d-%b %Y, %H:%M")))

(setq gnus-group-line-format "%M%S%p%P%5y%5R:%B%(%c%)\n")

(setq gnus-summary-display-arrow t)

(provide '.gnus.el)
;;; .gnus.el ends here
