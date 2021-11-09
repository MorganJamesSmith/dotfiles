(use-modules
 ((ice-9 popen) #:select (open-pipe* close-pipe))
 ((ice-9 rdelim) #:select (read-line))
 ((guix transformations) #:select (options->transformation))
 ((guix build utils) #:select (with-directory-excursion)))

(use-modules
 (flat packages emacs))

(define (emacs-git-commit)
  (let* ((pipe (with-directory-excursion "/home/pancake/src/emacs/emacs"
                 (open-pipe* OPEN_READ "git" "rev-parse" "HEAD")))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

(define transformations
  (options->transformation
   `((with-commit  . ,(string-append "emacs-native-comp=" (emacs-git-commit)))

     (with-git-url . "emacs-native-comp=/home/pancake/src/emacs/emacs")
     (with-input   . "emacs=emacs-native-comp")
     (with-input   . "emacs-minimal=emacs-native-comp")
     (with-input   . "emacs-no-x=emacs-native-comp")
     (with-input   . "emacs-no-x-toolkit=emacs-native-comp")

     ;; (with-git-url . "emacs-dash=/home/pancake/src/emacs/dash.el")
     ;; (with-branch  . "emacs-dash=master")

     (without-tests . "emacs-yasnippet") ;; Problem with 0.14.0.  Fixed in elpa
     (with-branch  . "emacs-ledger-mode=master") ;; Problem with 4.0.0

     ;; version: 20200515-1.0ef8b13
     ;; (wrong-type-argument utf-8-string-p "\177ELF...
     (without-tests  . "emacs-libgit")

     ;; version: 1.24
     ;; "Cannot find suitable directory for output in ‘native-comp-eln-load-path’"
     ;; (without-tests  . "emacs-buttercup")

     ;; version: 2.4.1
     ;; "Cannot find suitable directory for output in ‘native-comp-eln-load-path’"
     ;; (without-tests  . "emacs-use-package")

     ;; current value of native-comp-eln-load-path:
     ;; ("/homeless-shelter/.emacs.d/eln-cache/" "/gnu/store/vix7md2m5lm5j8k8hi7hw8fcjaq518hx-emacs-native-comp-git.9a5a35c/lib/emacs/28.0.50/native-lisp/")

     ;; (without-tests  . "emacs-esup")
     ;; (without-tests  . "emacs-lispy")
     )))

(define (specifications->manifest-with-transformations packages)
  (packages->manifest
   (map
    (compose
     (lambda (package output)
       (list (transformations package) output))
     specification->package+output)
    packages)))

(define emacs-packages
  (append!
   '("emacs-native-comp"
     "graphicsmagick" ; image-dired
     "libjpeg"        ; image-dired
     "pinentry-emacs"
     "ghostscript" ; allows Emacs to preview PostScript
     "unoconv")    ; allows Emacs to preview docx files
   (map
    (lambda (x) (string-append "emacs-" x))
    '("debbugs"
      "delight"
      "desktop-environment"
      "diff-hl"
      "disk-usage"
      "elpher"
      "eshell-syntax-highlighting"
      "exwm"
      "flymake-shellcheck"
      "geiser"
      "ggtags"
      "guix"
      "highlight-numbers"
      "irfc"
      "ledger-mode"
      "literate-calc-mode"
      "magit"
      "nov-el"
      "org"
      "org-contacts"
      "org-contrib" ;; for org-passwords
      "pdf-tools"
      "pinentry"
      "plantuml-mode"
      "rainbow-delimiters"
      "scad-mode"
      "sr-speedbar"
      "transmission"
      "typit"
      "undo-tree"
      "use-package"
      "vterm"
      "which-key"
      "ws-butler"
      "yasnippet"))))

(define stuff-only-needed-for-their-environment-variables
  '("man-db"     ;; MANPATH
    "texinfo"))  ;; INFOPATH

(specifications->manifest-with-transformations
 (append!
  emacs-packages
  stuff-only-needed-for-their-environment-variables))
