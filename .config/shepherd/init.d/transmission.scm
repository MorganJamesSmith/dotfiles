(define transmission
  (make <service>
    #:provides '(transmission)
    #:docstring "Run `transmission-daemon'"
    #:start (make-forkexec-constructor
             '("transmission-daemon" "-f")
               #:log-file (string-append (getenv "XDG_DATA_HOME")
                                         "/log/transmission.log"))
    #:stop (make-kill-destructor)
    #:respawn? #t))
(register-services transmission)

(start transmission)
