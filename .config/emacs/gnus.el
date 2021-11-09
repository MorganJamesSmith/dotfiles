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

(customize-set-variable 'gnus-interactive-exit nil)

(bind-key "m" #'gnus-summary-mark-as-processable gnus-summary-mode-map)
(bind-key "u" #'gnus-summary-clear-mark-forward gnus-summary-mode-map)

;; f runs the command mbsync
(defun get-mail ()
  "Get mail."
  (interactive)
  (let ((processes (list
                    (start-process "mbsync" "*mbsync*" "mbsync" "-a")
                    (start-process "rss2email" "*rss2email*" "r2e" "run"))))
    (dolist (process processes)
      (set-process-sentinel process #'gnus-group-get-new-news))))

(define-key gnus-group-mode-map (kbd "f") #'get-mail)

;; Perfer plain text email
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;; Don't ask how many messages I want to see. I want them all
(customize-set-variable 'gnus-large-newsgroup nil)

;; Leave the cursor where it is damn it!
(customize-set-variable 'gnus-auto-select-subject 'first)
(customize-set-variable 'gnus-summary-goto-unread 'never)
(customize-set-variable 'gnus-group-goto-unread nil)

;; All groups are new each time gnus is run
(customize-set-variable 'gnus-save-killed-list nil)

(customize-set-variable 'gnus-ignored-newsgroups
                        (regexp-opt
                         '("Notes" "Outbox" "Scheduled" "Calendar" "Contacts"
                           "Conversation" "Clutter" "Journal" "Tasks"
                           "[Gmail]/Trash" "[Gmail]/Important" "Starred" "Unwanted" "Drafts"
                           "Deleted" "All Mail")))

(customize-set-variable 'gnus-message-archive-group nil)

;; We don't need to cache since we use a local dovecot server
(customize-set-variable 'gnus-agent nil)

;; Encrypt email by default and also encrypt to self
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)
(customize-set-variable 'mml-secure-openpgp-encrypt-to-self t)
(customize-set-variable 'mml-secure-smime-encrypt-to-self t)

;; Always decrypt messages without asking me
(customize-set-variable 'mm-decrypt-option 'always)

(customize-set-variable 'gnus-completing-read-function #'gnus-ido-completing-read)
;; Always show all my groups
(customize-set-variable 'gnus-permanently-visible-groups ".")

(customize-set-variable 'gnus-visible-headers (concat gnus-visible-headers "\\|^Message-ID:"))

(customize-set-variable 'gnus-parameters `(("." (display . all))))


;; Receiving email stuff
(customize-set-variable 'gnus-select-method '(nnnil ""))
(customize-set-variable
 'gnus-secondary-select-methods
 (mapcar
  (lambda (x)
    `(nnimap ,x (nnimap-stream shell)))
  '("cmail" "grommin" "hotbutterypancake" "morganjsmith" "work" "local")))


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

(customize-set-variable 'gnus-user-date-format-alist `((t . ,time-stamp-format)))
(customize-set-variable 'gnus-group-line-format "%5y:%B%(%c%)\n")
(customize-set-variable 'gnus-summary-display-arrow t)

(provide 'my-gnus.el)
;;; gnus.el ends here
