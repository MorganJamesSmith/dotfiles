(define-module (emacs-manifest))

(use-modules (gnu packages)
             (guix profiles))

(define-public emacs-manifest-packages
  (specifications->packages
   (cons*
    "emacs-next-pgtk"
    "emacs-next-pgtk:src"
    "gnuplot"
    "shellcheck"  ; flymake shell files
    "graphicsmagick" ; image-dired thumbnail generation
    "libjpeg"        ; image-dired rotate images
    "mupdf"       ; allows Emacs to preview EPUB and PDF
    "ghostscript" ; allows Emacs to preview PostScript
    "djvulibre"   ; allows Emacs to preview djvu files
    "unoserver" ; allows Emacs to preview docx files
    "public-inbox" ; sync org-mode mailing list
    ;; org-mode PDF export
    "texinfo"
    "texlive-bin" "texlive-texinfo"
    "emacs-org-texlive-collection"
    
    "proof-general" ; elisp package not pre-fixed with "emacs-"
    (map
     (lambda (x) (string-append "emacs-" x))
     '(
       "arei"
       "bison-mode"
       "bluetooth"
       "clojure-mode"
       "crdt"
       "csv-mode"
       "debbugs"
       "delight"
       "diff-hl"
       "disk-usage"
       "el-search" ;; emacs-lisp refactoring tool
       "elpher"
       "emms"
       "eshell-syntax-highlighting"
       "geiser"
       "geiser-guile"
       "ggtags"
       "gnuplot"
       "guix"
       "flycheck-ledger"
       "forgejo" ;; guix issues
       "ledger-mode"
       "literate-calc-mode"
       "nov"
       "org"
       "org-transclusion"
       "osm"
       "pinentry"
       "rainbow-delimiters"
       "scad-mode"
       "smartparens"
       "transmission"
       "tup-mode"
       "vterm"
       "yasnippet"))))))

(define-public emacs-manifest
  (packages->manifest emacs-manifest-packages))

emacs-manifest
