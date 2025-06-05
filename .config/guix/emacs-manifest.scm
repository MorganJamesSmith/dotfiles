(use-modules
 ((ice-9 popen) #:select (open-pipe* close-pipe))
 ((ice-9 rdelim) #:select (read-line))
 (guix cpu)
 ((guix transformations) #:select (options->transformation))
 ((guix build utils) #:select (with-directory-excursion)))

(define* (git-commit path #:optional (commit "HEAD"))
  (let* ((pipe (with-directory-excursion path
                 (open-pipe* OPEN_READ "git" "rev-parse" commit)))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

(define* (use-local-source-transformations name path #:optional (commit "HEAD"))
  (let ((commit (git-commit path commit)))
    `((with-commit  . ,(string-append name "=" commit))
      (with-git-url . ,(string-append name "=" path)))))

;; We do these separately as they don't combine with our source transformations
;; unless they are done as a separate step.
;;
;; Our source transformations also clear the patches that would normally be
;; applied so I add them back in
(define patch-transformations
  (options->transformation
   '()
   ;; Guix can't build current master branch
   #;`(
     (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-disable-jit-compilation.patch")
     (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-next-exec-path.patch")
     (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-fix-scheme-indent-function.patch")
     (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-next-native-comp-driver-options.patch")
     (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-next-native-comp-fix-filenames.patch")
     (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-native-comp-pin-packages.patch")
     (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-pgtk-super-key-fix.patch"))))


(define other-transformations
  (options->transformation
   `(
     (tune . ,(cpu->gcc-architecture (current-cpu)))
     ;; ;; Guix can't build current master branch
     ;; ,@(use-local-source-transformations "emacs-next-pgtk" "/home/pancake/src/emacs/emacs")

     ,@(use-local-source-transformations "emacs-org" "/home/pancake/src/emacs/org-mode")

     ,@(use-local-source-transformations "emacs-arei" "/home/pancake/src/emacs/emacs-arei")

     (with-input   . "emacs=emacs-next-pgtk")
     (with-input   . "emacs-minimal=emacs-next-pgtk")
     (with-input   . "emacs-no-x=emacs-next-pgtk")
     (with-input   . "emacs-no-x-toolkit=emacs-next-pgtk")

     (without-tests . "emacs-yasnippet"))))

(define (transformations package)
  (patch-transformations (other-transformations package)))

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
     "gnuplot"
     "shellcheck"  ; flymake shell files
     "graphicsmagick" ; image-dired thumbnail generation
     "libjpeg"        ; image-dired rotate images
     "mupdf"       ; allows Emacs to preview EPUB
     "ghostscript" ; allows Emacs to preview PostScript
     "djvulibre"   ; allows Emacs to preview djvu files
     "libreoffice" ; allows Emacs to preview docx files

     "proof-general" ; elisp package not pre-fixed with "emacs-"
     "coq")
   (map
    (lambda (x) (string-append "emacs-" x))
    '(
      "arei"
      "bluetooth"
      "crdt"
      "csv-mode"
      "debbugs"
      "delight"
      "diff-hl"
      "disk-usage"
      "elpher"
      "emms"
      "eshell-syntax-highlighting"
      "geiser"
      "geiser-guile"
      "ggtags"
      "gnuplot"
      "guix"
      "flycheck-ledger"
      "ledger-mode"
      "literate-calc-mode"
      "nov-el"
      "org"
      "osm"
      "pinentry"
      "rainbow-delimiters"
      "scad-mode"
      "smartparens"
      "transmission"
      "vterm"
      "ws-butler"
      "yasnippet"))))

(specifications->manifest-with-transformations
 emacs-packages)

;; Local Variables:
;; compile-command: "guix build --max-jobs=4 --keep-going -m /home/pancake/.config/guix/emacs-manifest.scm"
;; End:
