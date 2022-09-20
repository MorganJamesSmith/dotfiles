(use-modules
 ((ice-9 popen) #:select (open-pipe* close-pipe))
 ((ice-9 rdelim) #:select (read-line))
 ((guix transformations) #:select (options->transformation))
 ((guix build utils) #:select (with-directory-excursion))
 (gnu packages fontutils))

(define (git-commit path)
  (let* ((pipe (with-directory-excursion path
                 (open-pipe* OPEN_READ "git" "rev-parse" "HEAD")))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

(define emacs-git-commit (git-commit "/home/pancake/src/emacs/emacs"))
(define org-git-commit (git-commit "/home/pancake/src/emacs/org-mode"))
(define emms-git-commit (git-commit "/home/pancake/src/emacs/emms"))

(define transformations
  (options->transformation
   `(
     (with-commit  . ,(string-append "emacs-next-pgtk=" emacs-git-commit))
     (with-git-url . "emacs-next-pgtk=/home/pancake/src/emacs/emacs")

     (with-commit  . ,(string-append "emacs-org=" org-git-commit))
     (with-git-url . "emacs-org=/home/pancake/src/emacs/org-mode")

     (with-commit  . ,(string-append "emacs-emms=" emms-git-commit))
     (with-git-url . "emacs-emms=/home/pancake/src/emacs/emms")

     (with-input   . "emacs=emacs-next-pgtk")
     (with-input   . "emacs-minimal=emacs-next-pgtk")
     (with-input   . "emacs-no-x=emacs-next-pgtk")
     (with-input   . "emacs-no-x-toolkit=emacs-next-pgtk")

     (without-tests . "emacs-rainbow-delimiters")

     (without-tests . "emacs-kv")
     ;; (without-tests . "emacs-buttercup")
     ;; (without-tests . "emacs-s")
     ;; (without-tests . "emacs-clojure-mode")
     ;; (without-tests . "emacs-org")
     ;; (without-tests . "emacs-yasnippet") ;; Problem with 0.14.0.  Fixed in elpa
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
   '("emacs-next-pgtk"
     "pinentry-emacs"
     "graphicsmagick" ; image-dired thumbnail generation
     "libjpeg"        ; image-dired rotate images
     "mupdf"       ; allows Emacs to preview EPUB
     "ghostscript" ; allows Emacs to preview PostScript
     "unoconv")    ; allows Emacs to preview docx files
   (map
    (lambda (x) (string-append "emacs-" x))
    '(
      "buffer-env"
      "crdt"
      "csv-mode"
      "debbugs"
      "delight"
      "desktop-environment"
      "diff-hl"
      "disk-usage"
      "elpher"
      "emms"
      "eshell-syntax-highlighting"
      "flymake-shellcheck"
      "geiser"
      "geiser-guile"
      "ggtags"
      "guix"
      "hledger-mode"
      "literate-calc-mode"
      "logos"
      "magit"
      "nov-el"
      ;; "org-passwords" ;; not packaged yet
      ;; "org" ;; TODO: won't build
      "osm"
      ;; "pdf-tools" ;; TODO: won't build
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
    "texinfo"    ;; INFOPATH
    ;; fontconfig ;; XDG_DATA_DIRS
    ))

(concatenate-manifests
 (list
  (packages->manifest
   (list fontconfig))
  (specifications->manifest-with-transformations
   (append!
    emacs-packages
    stuff-only-needed-for-their-environment-variables))))
