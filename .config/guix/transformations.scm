(define-module (transformations))

(use-modules
 (gnu packages)
 (guix cpu)
 ((guix transformations) #:select (options->transformation))
 ((guix build utils) #:select (with-directory-excursion))
 ((ice-9 popen) #:select (open-pipe* close-pipe))
 ((ice-9 rdelim) #:select (read-line)))

(define* (git-commit path #:optional (commit "HEAD"))
  (let* ((pipe (with-directory-excursion path
                 (open-pipe* OPEN_READ "git" "rev-parse" commit)))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

(define* (use-local-source-transformations name path #:optional (commit "HEAD")
                                           #:key without-tests?)
  (let ((commit (git-commit path commit)))
    `((with-commit  . ,(string-append name "=" commit))
      (with-git-url . ,(string-append name "=" path))
      ,@(if without-tests?
            (list (cons 'without-tests name))
            '()))))

(define transformations
  (options->transformation
   `(
     (tune . ,(cpu->gcc-architecture (current-cpu)))

     ,@(use-local-source-transformations "emacs-next-pgtk" "/home/pancake/src/emacs/emacs"
                                         #:without-tests? #t)

     ,@(use-local-source-transformations "emacs-org" "/home/pancake/src/emacs/org-mode" "installed"
                                         #:without-tests? #t)

     ,@(use-local-source-transformations "proof-general" "/home/pancake/src/emacs/proof-general")

     ,@(use-local-source-transformations "emacs-org-transclusion" "/home/pancake/src/emacs/org-transclusion"
                                         #:without-tests? #t)

     (with-input   . "emacs=emacs-next-pgtk")
     (with-input   . "emacs-minimal=emacs-next-pgtk")
     (with-input   . "emacs-no-x=emacs-next-pgtk")
     (with-input   . "emacs-no-x-toolkit=emacs-next-pgtk")

     ;; doesn't build.  TODO: investigate
     (with-input   . "emacs-ert-runner=emacs-next-pgtk")

     (without-tests . "emacs-ledger-mode")
     (without-tests . "emacs-dash")
     )))

;; We do these separately as they don't combine with our source transformations
;; unless they are done as a separate step.
;;
;; Our source transformations also clear the patches that would normally be
;; applied so I add them back in
(define patch-transformations
  (options->transformation
   '(
   (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-next-disable-jit-compilation.patch")
   (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-next-exec-path.patch")
   (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-fix-scheme-indent-function.patch")
   (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-native-comp-driver-options.patch")
   (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-next-native-comp-fix-filenames.patch")
   (with-patch . "emacs-next-pgtk=/home/pancake/src/guix/gnu/packages/patches/emacs-native-comp-pin-packages.patch"))))

(define-public specifications->packages-with-transformations
  (lambda* (specifications #:optional (packages '()))
    (map
     (lambda* (package #:optional (output "out"))
       (list (patch-transformations (transformations package)) output))
     (append!
      (map
       specification->package+output
       specifications)
      packages))))
