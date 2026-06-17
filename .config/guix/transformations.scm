(define-module (transformations))

(use-modules
 (srfi srfi-1)
 (ice-9 string-fun)
 (gnu packages)
 (guix cpu)
 ((guix packages) #:select (package-input-rewriting/spec))
 ((guix transformations) #:select (options->transformation))
 ((guix build utils) #:select (with-directory-excursion))
 ((ice-9 popen) #:select (open-pipe* close-pipe))
 ((ice-9 rdelim) #:select (read-line)))

(load "machine-specific.scm")

(define* (git-commit path #:optional (commit "HEAD"))
  (let* ((pipe (with-directory-excursion path
                 (open-pipe* OPEN_READ "git" "rev-parse" commit)))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

(define* (use-local-source-transformations name path #:optional (commit "HEAD")
                                           #:key without-tests?)
  (if (file-exists? path)
      (let ((commit (git-commit path commit)))
        `((with-commit  . ,(string-append name "=" commit))
          (with-git-url . ,(string-append name "=" path))
          ,@(if without-tests?
                (list (cons 'without-tests name))
                '())))
      (begin
        (display (string-append "Transformation aborted! No such path " path "\n"))
        '())))

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
               ;; TODO: apply patches from upstream
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
               ;; TODO: tell upstream
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

(define transformations
  (compose
   (options->transformation
    `(
      (tune . ,(cpu->gcc-architecture (current-cpu)))

      ,@(use-local-source-transformations "emacs-org" "/home/pancake/src/emacs/org-mode" "installed"
                                          #:without-tests? #t)

      ,@(use-local-source-transformations "proof-general" "/home/pancake/src/emacs/proof-general")

      ,@(use-local-source-transformations "emacs-org-transclusion" "/home/pancake/src/emacs/org-transclusion"
                                          #:without-tests? #t)

      ;; doesn't build.  TODO: investigate
      (with-input   . "emacs-ert-runner=emacs")

      (without-tests . "emacs-flycheck")
      (without-tests . "emacs-ledger-mode")))
   (package-input-rewriting/spec
    `(("xdg-utils" . ,(const xdg-utils-grafted))
      ,@(map (lambda (pkg)
               (cons pkg (const emacs-custom)))
             (list
              "emacs-next-pgtk"
              "emacs"
              "emacs-minimal"
              "emacs-no-x"
              "emacs-no-x-toolkit"))))))

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
