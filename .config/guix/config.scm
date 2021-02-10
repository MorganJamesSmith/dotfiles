(use-modules
 ((srfi srfi-1) #:select (remove))
 ((nongnu packages linux) #:select (linux linux-firmware))
 ((nongnu system linux-initrd) #:select (microcode-initrd))
 ((gnu services linux) #:select (kernel-module-loader-service-type))
 (gnu)
 ((gnu packages admin) #:select (opendoas))
 ((gnu packages base) #:select (glibc-utf8-locales))
 ((gnu packages certs) #:select (nss-certs))
 ((gnu packages curl) #:select (curl))
 ((gnu packages gl) #:select (mesa))
 ((gnu packages security-token) #:select (libu2f-host))
 ((gnu packages linux) #:select (v4l2loopback-linux-module))
 ((gnu packages rsync) #:select (rsync))
 ((gnu packages shells) #:select (dash))
 ((gnu packages suckless) #:select (slock))
 ((gnu packages version-control) #:select (git))
 ((gnu packages wget) #:select (wget))
 ((gnu packages xorg) #:select (xkbcomp xorg-server))
 ((gnu services dbus) #:select (dbus-service))
 ((gnu services sysctl) #:select (sysctl-service-type sysctl-configuration))
 ((gnu services desktop) #:select (%desktop-services bluetooth-service))
 ((gnu services security-token) #:select (pcscd-service-type))
 ((gnu services audio) #:select (mpd-service-type mpd-configuration mpd-output))
 ((gnu services xorg)
  #:select (gdm-service-type xorg-configuration xorg-configuration-modules xorg-configuration-server-arguments))
 (guix gexp))

(define username "CHANGE ME")
(define host-name "CHANGE ME")
(define my-keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))


;; Things not exported by (gnu services xorg)
(define %default-xorg-server-arguments (@@ (gnu services xorg) %default-xorg-server-arguments))
(define xorg-configuration-directory (@@ (gnu services xorg) xorg-configuration-directory))
(define xorg-configuration->file (@@ (gnu services xorg) xorg-configuration->file))

;; Things not exported by (gnu system)
(define %default-modprobe-blacklist (@@ (gnu system) %default-modprobe-blacklist))


(define my-xorg-conf
  (xorg-configuration
   (keyboard-layout my-keyboard-layout)
   (server-arguments
    `("-keeptty" ,@%default-xorg-server-arguments))))

(define startx
  (program-file
   "startx"
   #~(begin
       (setenv
        "XORG_DRI_DRIVER_PATH" (string-append #$mesa "/lib/dri"))
       (setenv
        "XKB_BINDIR" (string-append #$xkbcomp "/bin"))

       ;; X doesn't accept absolute paths when run with suid
       (apply
        execl
        (string-append #$xorg-server "/bin/X")
        (string-append #$xorg-server "/bin/X")
        "-config" #$(xorg-configuration->file my-xorg-conf)
        "-configdir" #$(xorg-configuration-directory
                        (xorg-configuration-modules my-xorg-conf))
        "-logverbose" "-verbose" "-terminate"
        (append '#$(xorg-configuration-server-arguments my-xorg-conf)
                (cdr (command-line)))))))

(define chown-program-service-type
  (service-type
   (name 'chown-program-service-type)
   (extensions
    (list
     (service-extension setuid-program-service-type (const '()))
     (service-extension
      activation-service-type
      (lambda (params)
        #~(begin
            (define (chownership prog user group perm)
              (let ((uid (passwd:uid (getpw user)))
                    (gid (group:gid (getgr group))))
                (chown prog uid gid)
                (chmod prog perm)))
            (for-each (lambda (x) (apply chownership x)) #$params))))))
   (description "Modify permissions and ownership of programs.")))

(operating-system
  (host-name host-name)
  (timezone "America/New_York")
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))

  ;; US keyboard but replace caps with ctrl
  (keyboard-layout my-keyboard-layout)

  (kernel-loadable-modules (list v4l2loopback-linux-module))

  (kernel-arguments
   (list
    "mitigations=off"
    "numa=off"
    (string-append
     "modprobe.blacklist="
     (string-join (cons*
                   "pcspkr" "snd_pcsp" ; Stop the beeping
                   "snd_hda_intel"     ; Only use my usb audio
                   "sb_edac"           ; Stop EDAC warnings
                   %default-modprobe-blacklist)
                  ","))))

  (issue "Morgan's GNU Guix Machine\n\n")

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")
                (keyboard-layout my-keyboard-layout)))

  (swap-devices (list "/swapfile"))

  ;; Specify a mapped device for the encrypted root partition.
  ;; The UUID is that returned by 'cryptsetup luksUUID'.
  (mapped-devices
   (list (mapped-device
          (source (uuid "CHANGE ME"))
          (target "guix-root")
          (type luks-device-mapping))))

  (file-systems (append
                 (list (file-system
                         (device (file-system-label "guix-root"))
                         (mount-point "/")
                         (type "ext4")
                         (dependencies mapped-devices))
                       (file-system
                         (device (uuid "CHANGE ME" 'fat))
                         (mount-point "/boot/efi")
                         (type "vfat")))
                 %base-file-systems))

  (groups (cons (user-group (name "plugdev")
                            (system? #t))
                %base-groups))

  (users (cons (user-account
                (name username)
                (comment username)
                (group "users")
                (supplementary-groups '("wheel"   ; sudo
                                        "lp"      ; bluetooth
                                        "video"
                                        "dialout" ; tty stuff
                                        "plugdev" ; security keys
                                        "kvm")))  ; qemu
               %base-user-accounts))

  (setuid-programs (cons*
                    (file-append opendoas "/bin/doas")
                    ;; Stuff for xorg without display manager.
                    ;; startx and X need to be in setuid-programs.
                    ;; They also need extra tweaks in the chown-file service below.
                    (file-append xorg-server "/bin/X")
                    startx
                    %setuid-programs))

  ;; This is where we specify system-wide packages.
  (packages (cons*
             glibc-utf8-locales
             nss-certs
             %base-packages))

  (services
   (cons*
    ;; Helps with IO related freezing
    (service sysctl-service-type
             (sysctl-configuration
              (settings '(("vm.dirty_background_ratio" . "5")
                          ("vm.dirty_ratio" . "25")
                          ("kernel.hung_task_timeout_secs" . "30")))))
    (bluetooth-service)
    (simple-service
     'opendoas-config etc-service-type
     `(("doas.conf"
        ,(plain-file
          "doas.conf"
          (string-append "permit persist " username (string #\newline))))))
    (service mpd-service-type
             (mpd-configuration
              (user username)
              (music-dir "~/music")
              (playlist-dir "~/.config/mpd/playlists")
              (db-file "~/.config/mpd/database")
              (state-file "~/.config/mpd/state")
              (sticker-file "~/.config/mpd/sticker.sql")))
    (service guix-publish-service-type
             (guix-publish-configuration
              (host "0.0.0.0")
              (port 3000)
              (advertise? #t)))
    (service
     chown-program-service-type
     #~(list
        (list
         (string-append "/run/setuid-programs/" (basename #$startx))
         #$username "input" #o2755)
        `("/run/setuid-programs/X" ,#$username "input" #o2755)))
    (modify-services
        (remove
         (lambda (service)
           (eq? (service-kind service) gdm-service-type))
         %desktop-services)

      (guix-service-type config =>
                         (guix-configuration
                          (inherit config)
                          (discover? #t)
                          (authorized-keys
                           (append (list (local-file "./desktop.pub"))
                                   %default-authorized-guix-keys))))

      ;; Use dash for /bin/sh instead of bash
      (special-files-service-type
       c =>
       `(("/bin/sh" ,(file-append dash "/bin/dash"))
         ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))

      (udev-service-type
       c =>
       (udev-configuration
        (inherit c)
        (rules
         `(
           ;; Security key
           ,libu2f-host
           ;; For xorg sans display manager (gentoo wiki)
           ,(udev-rule
             "99-dev-input-group.rules"
             "SUBSYSTEM==\"input\", ACTION==\"add\", GROUP=\"input\"")
           ;; Black magic probe
           ,(udev-rule
             "99-blackmagic.rules"
             (string-append
              "SUBSYSTEM==\"tty\", ACTION==\"add\", ATTRS{interface}==\"Black Magic GDB Server\", SYMLINK+=\"ttyBmpGdb\""
              (string #\newline)
              "SUBSYSTEM==\"tty\", ACTION==\"add\", ATTRS{interface}==\"Black Magic UART Port\", SYMLINK+=\"ttyBmpTarg\""
              (string #\newline)
              "SUBSYSTEM==\"usb\", ENV{DEVTYPE}==\"usb_device\", ATTR{idVendor}==\"1d50\", ATTR{idProduct}==\"6017\", MODE=\"0666\""
              (string #\newline)
              "SUBSYSTEM==\"usb\", ENV{DEVTYPE}==\"usb_device\", ATTR{idVendor}==\"1d50\", ATTR{idProduct}==\"6018\", MODE=\"0666\""
              (string #\newline)))
           ,@(udev-configuration-rules c))))))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
