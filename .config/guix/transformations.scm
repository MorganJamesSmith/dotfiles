(define-module (transformations)
  #:use-module ((gnu packages) #:select (specification->package+output specification->package))
  #:use-module ((guix build utils) #:select (with-directory-excursion))
  #:use-module ((guix build-system) #:select (build-system-name))
  #:use-module ((guix cpu) #:select (cpu->gcc-architecture current-cpu))
  #:use-module (guix git)
  #:use-module (guix packages)
  #:use-module ((guix transformations) #:select (tuned-package))
  #:use-module ((guix utils) #:select (substitute-keyword-arguments))
  #:use-module ((ice-9 popen) #:select (open-pipe* close-pipe))
  #:use-module ((ice-9 rdelim) #:select (read-line)))

(load "machine-specific.scm")

(define micro-architecture (cpu->gcc-architecture (current-cpu)))

(define* (git-commit path #:optional (commit "HEAD"))
  (let* ((pipe (with-directory-excursion path
                 (open-pipe* OPEN_READ "git" "rev-parse" commit)))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

(define* (package-rewrite-use-local-source name path #:optional (commit "HEAD")
                                           #:key without-tests?)
  (if (file-exists? path)
      (let ((commit (git-commit path commit)))
        (cons
         name
         (lambda (pkg)
           (package
             (inherit pkg)
             (version (string-append (package-version pkg) "-" (string-take commit 7)))
             (source (git-checkout (url path) (commit commit)))
             (arguments
              (substitute-keyword-arguments arguments
                ((#:tests? _ #f) (not without-tests?))))))))
      (begin
        (display (string-append "Transformation aborted! No such path " path "\n"))
        #f)))

(define* (package-rewrite-without-tests name)
  (cons name
        (lambda (pkg)
          (package
            (inherit pkg)
            (arguments
             (substitute-keyword-arguments arguments
               ((#:tests? _ #f) #f)))))))

(define dummy-package
  (eval
   `(begin
      (use-modules
       (guix packages)
       (guix build-system trivial)
       (guix licenses))
      (package
        (name "dummy")
        (version "0")
        (source #f)
        (build-system trivial-build-system)
        (arguments
         `(#:modules ((guix build utils))
           #:target #f
           #:builder (begin
                       (use-modules (guix build utils))
                       (let* ((out (assoc-ref %outputs "out"))
                              (dummy (string-append out "/dummy")))
                         (mkdir-p out)
                         (call-with-output-file dummy
                           (const #t))))))
        (home-page #f)
        (synopsis #f)
        (description #f)
        (license (fsdg-compatible "dummy"))))
   (make-fresh-user-module)))

(define* (package-rewrite-eliminate-package name)
  (cons name (const dummy-package)))

(define-public emacs-custom
  (let* ((path "/home/pancake/src/emacs/emacs")
         (commit (git-commit path)))
    (eval
     `(begin
        (use-modules
         (gnu packages emacs)
         (guix gexp)
         (guix git)
         (guix git-download)
         (guix packages)
         (guix utils))
        (let ((path ,path)
              (commit ,commit)
              (emacs emacs-next-pgtk))
          (package
            (inherit emacs)
            (version (git-version "32.0.50" "0" commit))
            (source
             (origin
               (inherit (package-source emacs))
               (method (@@ (guix packages) computed-origin-method))
               (file-name (git-file-name "emacs-custom" version))
               (sha256 #f)
               (uri
                (delay
                  (with-imported-modules '((guix build utils))
                    #~(begin
                        (use-modules (guix build utils))
                        (copy-recursively #+(git-checkout (url path) (commit commit)) #$output)))))
               ;; FIXME: apply patches from upstream
               (patches '())))
            (arguments
             ;; Optimization suggestions from
             ;; https://www.jamescherti.com/compiling-emacs/
             (substitute-keyword-arguments arguments
               ((#:configure-flags flags #~'())
                #~(append
                   #$flags
                   (list
                    ;; Makes build non-reproducible but is helpful
                    "--enable-build-details"

                    "--enable-link-time-optimization"
                    "--disable-gc-mark-trace"
                    "--without-compress-install"
                    "--without-x"
                    "--without-xft"
                    "--without-xim"
                    "--without-libotf"
                    "--without-gpm"
                    "--disable-acl"
                    "--disable-xattr"

                    "CFLAGS=-O2 -pipe -fno-omit-frame-pointer -fno-plt"
                    #$(string-append
                       "LDFLAGS=-Wl,-O2 -Wl,-z,now"
                       " -Wl,-z,relro -Wl,--sort-common"
                       " -Wl,--as-needed"
                       " -Wl,-z,pack-relative-relocs"
                       " -O2"))))
               ((#:phases phases)
                #~(modify-phases #$phases
                    (add-after 'patch-compilation-driver 'my-native-comp-settings
                      (lambda _
                        (substitute* "lisp/emacs-lisp/comp.el"
                          (("\\(defcustom native-comp-driver-options '\\(" all)
                           (string-append
                            all
                            (format
                             #f "~@{~s~^ ~}"
                             "-Wl,-z,pack-relative-relocs"
                             "-Wl,-O2"
                             "-Wl,--as-needed")))
                          (("\\(defcustom native-comp-compiler-options nil")
                           (format
                            #f "(defcustom native-comp-compiler-options '(~@{~s~^ ~})"
                            "-O2" "-g0"
                            "-fno-omit-frame-pointer"
                            "-fno-finite-math-only")))))))
               ;; Not supported by 'glib-or-gtk-build-system'
               ;; FIXME: tell upstream
               ;; ((#:substitutable? _ #f) #f)
               )))))
     (make-fresh-user-module))))

;; There are some bugs in "xdg-open" that have been fixed
(define-public xdg-utils-next
  (let* ((path "/home/pancake/src/xdg-utils")
         (commit (git-commit path)))
    (eval
     `(begin
        (use-modules
         (gnu packages freedesktop)
         (guix git)
         (guix git-download)
         (guix packages)
         (guix utils))
        (let ((path ,path)
              (commit ,commit))
          (package
            (inherit xdg-utils)
            ;; Version has to be same length as current one for grafting to work
            (version "1.2.9")
            ;; (version (git-version "1.2.1" "0" commit))
            (source (git-checkout (url path) (commit commit)))
            (arguments
             (substitute-keyword-arguments arguments
               ((#:substitutable? _ #f) #f))))))
     (make-fresh-user-module))))

(define-public xdg-utils-grafted
  (eval
   `(begin
      (use-modules
       (gnu packages freedesktop)
       (guix packages))
      (package
        (inherit xdg-utils)
        (replacement ,xdg-utils-next)))
   (make-fresh-user-module)))

;; FIXME: upstream should really provide a function like this
(define (package-input-rewriting/spec/recursive replacements
                                                extra-func)
  (define replacement-property
    (gensym " package-replacement"))
  
  (define (rewrite p)
    (if (assq-ref (package-properties p) replacement-property)
        p
        (let ((proc (compose extra-func
                             (or (assoc-ref replacements (package-name p))
                                 identity))))
          (let ((new (proc p)))
            ;; Mark NEW as already processed.
            (package/inherit new
              (properties `((,replacement-property . #t)
                            ,@(package-properties new))))))))
  (package-mapping rewrite #:deep? #t))

(define transformations
  (package-input-rewriting/spec/recursive
   `(("xdg-utils" . ,(const xdg-utils-grafted))
     ,@(map (lambda (pkg)
              (cons pkg (const emacs-custom)))
            (list
             "emacs-next-pgtk"
             "emacs"
             "emacs-minimal"
             "emacs-no-x"
             "emacs-no-x-toolkit"))

     ,(package-rewrite-eliminate-package "geoclue")

     ;; mad about missing geoclue
     ,(package-rewrite-without-tests "xdg-desktop-portal")

     ,(package-rewrite-use-local-source "tup" "/home/pancake/src/tup")

     ,(package-rewrite-use-local-source "emacs-org" "/home/pancake/src/emacs/org-mode" "installed"
                                        #:without-tests? #t)

     ,(package-rewrite-use-local-source "proof-general" "/home/pancake/src/emacs/proof-general")

     ,(package-rewrite-use-local-source "emacs-org-transclusion" "/home/pancake/src/emacs/org-transclusion"
                                        #:without-tests? #t)

     ,(package-rewrite-without-tests "emacs-ledger-mode"))
   (compose
    (lambda (p)
      ;; For some Emacs packages, I want to use the guix package.  For some I
      ;; want to use the version vendored with my Emacs

      ;; If I want to use the guix package, then I have to add it as a
      ;; dependency to every emacs package to ensure nothing is compiled suing
      ;; the vendored version

      ;; See  https://codeberg.org/guix/guix/issues/1055

      (let ((use-vendored-package
             (map (lambda (p)
                    (string-append "emacs-" p))
                  (list
                   "compat"
                   "csharp-mode"
                   "eglot"
                   "eldoc"
                   "erc"
                   "external-completion"
                   "flymake"
                   "jsonrpc"
                   "modus-themes"
                   "ntlm"
                   "peg"
                   "project"
                   "so-long"
                   "soap-client"
                   "track-changes"
                   "tramp"
                   "transient"
                   "use-package"
                   "verilog-mode"
                   "which-key"
                   "window-tool-bar"
                   "xref"

                   ;; This package is outdated and should be removed from guix
                   ;; entirely
                   "cl-print")))
            (use-guix-package '("emacs-org"))
            (name (package-name p)))
        (cond
         ((member name use-vendored-package)
          dummy-package)
         ;; Add everything from `use-guix-package' to the inputs of every emacs package
         ((and (or (eq? (build-system-name (package-build-system p)) 'emacs)
                   ;; Easier to hard-code exceptions then to find them a
                   ;; better way
                   (member name '("emacs-guix" "proof-general"))))
          (let ((additional-inputs (delete name use-guix-package)))
            (if (> 1 (length additional-inputs))
                p
                (package/inherit p
                  (propagated-inputs
                   (modify-inputs propagated-inputs
                     (prepend (apply specification->package additional-inputs))))))))
         (else
          (when (and (string-prefix? "emacs-" name)
                     (not (member name '("emacs-next-pgtk" "emacs-emms-print-metadata"))))
              ;; So I'll notice if anything slips through
              (format (current-error-port) "WARNING: not performing Emacs unvendoring on ~a~%"
                      (package-full-name p)))
            p))))
    (lambda (p)
      ;; Tune package for CPU
      (if (assq 'tunable? (package-properties p))
          (begin
            (format (current-error-port) "tuning ~a for CPU ~a~%"
                    (package-full-name p) micro-architecture)
            (package/inherit p
              (replacement (tuned-package p micro-architecture))))
          p)))))

(define-public specifications->packages-with-transformations
  (lambda* (specifications #:optional (packages '()))
    (map
     (lambda* (package #:optional (output "out"))
       (list (transformations package) output))
     (append!
      (map
       specification->package+output
       specifications)
      packages))))
