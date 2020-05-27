(define mpd
  (make <service>
    #:provides '(mpd)
    #:docstring "Run `mpd'"
    #:start (make-forkexec-constructor
             '("mpd" "--no-daemon" "--verbose")
               #:log-file (string-append (getenv "XDG_DATA_HOME")
                                         "/log/mpd.log"))
    #:stop (make-kill-destructor)
    #:respawn? #t))
(register-services mpd)

(start mpd)
