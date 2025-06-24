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

(define* (use-local-source-transformations name path #:optional (commit "HEAD"))
  (let ((commit (git-commit path commit)))
    `((with-commit  . ,(string-append name "=" commit))
      (with-git-url . ,(string-append name "=" path)))))

(define transformations
  (options->transformation
   `(
     (tune . ,(cpu->gcc-architecture (current-cpu)))

     ;; Current release version fails to parse my ledger file
     (with-branch . "ledger=master")

     ,@(use-local-source-transformations "emacs-next-pgtk" "/home/pancake/src/emacs/emacs")
     (without-tests . "emacs-next-pgtk")

     ,@(use-local-source-transformations "emacs-org" "/home/pancake/src/emacs/org-mode" "installed")
     (without-tests . "emacs-org")

     ,@(use-local-source-transformations "proof-general" "/home/pancake/src/emacs/proof-general" "master")

     ,@(use-local-source-transformations "emacs-arei" "/home/pancake/src/emacs/emacs-arei")

     (with-input   . "emacs=emacs-next-pgtk")
     (with-input   . "emacs-minimal=emacs-next-pgtk")
     (with-input   . "emacs-no-x=emacs-next-pgtk")
     (with-input   . "emacs-no-x-toolkit=emacs-next-pgtk")

     (without-tests . "emacs-ledger-mode")
     (without-tests . "emacs-yasnippet"))))

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
