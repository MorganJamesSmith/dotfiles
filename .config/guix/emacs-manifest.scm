(define-module (emacs-manifest))

(use-modules
 (guix profiles))

(when (current-filename)
  (add-to-load-path (dirname (current-filename))))
(use-modules (transformations))

(define-public emacs-manifest-packages
  (specifications->packages-with-transformations
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
       "org-transclusion"
       "osm"
       "pinentry"
       "rainbow-delimiters"
       "scad-mode"
       "smartparens"
       "transmission"
       "tup-mode"
       "vterm"
       "ws-butler"
       "yasnippet")))))

(define-public emacs-manifest
  (packages->manifest emacs-manifest-packages))

emacs-manifest

;; Local Variables:
;; compile-command: "guix build --max-jobs=4 --keep-going -m /home/pancake/.config/guix/emacs-manifest.scm"
;; End:
