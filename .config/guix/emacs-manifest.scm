(use-modules
 ((ice-9 popen) #:select (open-pipe* close-pipe))
 ((ice-9 rdelim) #:select (read-line))
 ((guix transformations) #:select (options->transformation))
 ((guix build utils) #:select (with-directory-excursion)))

(use-modules
 (flat packages emacs))

(define (git-commit path)
  (let* ((pipe (with-directory-excursion path
                 (open-pipe* OPEN_READ "git" "rev-parse" "HEAD")))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

(define emacs-git-commit (git-commit "/home/pancake/src/emacs/emacs"))
(define org-git-commit (git-commit "/home/pancake/src/emacs/org-mode"))


(define transformations
  (options->transformation
   `(
     (with-commit  . ,(string-append "emacs-pgtk-native-comp=" emacs-git-commit))
     (with-git-url . "emacs-pgtk-native-comp=/home/pancake/src/emacs/emacs")

     (with-commit  . ,(string-append "emacs-org=" org-git-commit))
     (with-git-url . "emacs-org=/home/pancake/src/emacs/org-mode")

     (with-input   . "emacs=emacs-pgtk-native-comp")
     (with-input   . "emacs-minimal=emacs-pgtk-native-comp")
     (with-input   . "emacs-no-x=emacs-pgtk-native-comp")
     (with-input   . "emacs-no-x-toolkit=emacs-pgtk-native-comp")

     (without-tests . "emacs-kv")
     (without-tests . "emacs-org")
     (without-tests . "emacs-yasnippet") ;; Problem with 0.14.0.  Fixed in elpa
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
   '("emacs-pgtk-native-comp"
     "pinentry-emacs"
     "ghostscript" ; allows Emacs to preview PostScript
     "unoconv")    ; allows Emacs to preview docx files
   (map
    (lambda (x) (string-append "emacs-" x))
    '("crdt"
      "debbugs"
      "delight"
      "desktop-environment"
      "diff-hl"
      "disk-usage"
      "elpher"
      "eshell-syntax-highlighting"
      "flymake-shellcheck"
      "geiser"
      "geiser-guile"
      "ggtags"
      "guix"
      "ledger-mode"
      "literate-calc-mode"
      "logos"
      "magit"
      "nov-el"
      "org-contrib" ;; for org-passwords
      "osm"
      "pdf-tools"
      "pinentry"
      "plantuml-mode"
      "rainbow-delimiters"
      "rec-mode"
      "scad-mode"
      "sr-speedbar"
      "transmission"
      "tup-mode"
      "vterm"
      "vundo"
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
