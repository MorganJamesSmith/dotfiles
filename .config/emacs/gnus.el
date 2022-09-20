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

(keymap-set gnus-summary-mode-map "m" #'gnus-summary-mark-as-processable)
(keymap-set gnus-summary-mode-map "u" #'gnus-summary-clear-mark-forward)

;; Encrypted emails are sent with the subject "...".  Don't assume they're all
;; part of the same thread
(setopt gnus-summary-gather-exclude-subject "^ *$\\|^...$\\|^(none)$")

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

;; Don't ask how many messages I want to see. I want them all
(customize-set-variable 'gnus-large-newsgroup nil)

;; Don't check for new newsgroups
(customize-set-variable 'gnus-save-killed-list nil)
(customize-set-variable 'gnus-check-new-newsgroups nil)


(customize-set-variable 'gnus-message-archive-group nil)

;; We don't need to cache since we use a local dovecot server
(customize-set-variable 'gnus-agent nil)
(customize-set-variable 'gnus-use-cache nil)

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

;; Speeds up display of large groups quite a bit
(setopt gnus-show-threads nil)


;; TODO: figure out how to specifiy my nnvirtual groups from here
;; (nnvirtual "INBOX\\|Inbox\\|Junk\\|Spam")
;; (nnvirtual "[^l].\\(INBOX\\|Inbox\\|Junk\\|Spam\\)")

;; Receiving email stuff
(customize-set-variable 'gnus-select-method '(nnnil ""))
(customize-set-variable
 'gnus-secondary-select-methods
 (mapcar
  (lambda (x)
    `(nnimap ,x
             (nnimap-user ,x)
             (nnimap-address "localhost")
             (nnimap-stream network)
             (nnimap-server-port 143)))
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
