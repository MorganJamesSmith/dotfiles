(use-modules
 ((srfi srfi-1) #:select (remove))
 ((nongnu packages linux) #:select (linux linux-firmware))
 ((nongnu system linux-initrd) #:select (microcode-initrd))
 ((gnu services linux) #:select (kernel-module-loader-service-type))
 (gnu)
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
 ((gnu packages xorg) #:select (xinit xkbcomp xorg-server))
 ((gnu services audio) #:select (mpd-service-type mpd-configuration mpd-output))
 ((gnu services dbus) #:select (dbus-service))
 ((gnu services desktop) #:select (%desktop-services bluetooth-service))
 ((gnu services file-sharing) #:select (transmission-daemon-service-type))
 ((gnu services security-token) #:select (pcscd-service-type))
 ((gnu services syncthing) #:select (syncthing-service-type syncthing-configuration))
 ((gnu services sysctl) #:select (sysctl-service-type sysctl-configuration))
 ((gnu services xorg) #:select (gdm-service-type xorg-configuration xorg-start-command))
 (guix gexp))

(define username "CHANGE ME")
(define host-name "CHANGE ME")
(define my-keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))


;; Things not exported by (gnu services xorg)
(define %default-xorg-server-arguments (@@ (gnu services xorg) %default-xorg-server-arguments))

;; Things not exported by (gnu system)
(define %default-modprobe-blacklist (@@ (gnu system) %default-modprobe-blacklist))


(define my-xorg-conf
  (xorg-configuration
   (keyboard-layout my-keyboard-layout)
   (server-arguments
    `("-keeptty" ,@%default-xorg-server-arguments))))

(define my-startx
  #~(let ((xinit (string-append #$xinit "/bin/xinit")))
      (apply execl xinit xinit ;; Second xinit is for argv[0].
             "--" #$(xorg-start-command my-xorg-conf) (cdr (command-line)))))

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
                                        "transmission"
                                        "dialout" ; tty stuff
                                        "plugdev" ; security keys
                                        "kvm")))  ; qemu
               %base-user-accounts))

  ;; This is where we specify system-wide packages.
  (packages (cons*
             glibc-utf8-locales
             nss-certs
             %base-packages))

  (services
   (cons*
    (extra-special-file "/bin/startx"
                        (program-file "startx" my-startx))
    (service transmission-daemon-service-type)
    (service syncthing-service-type
             (syncthing-configuration (user username)))
    ;; Helps with IO related freezing
    (service sysctl-service-type
             (sysctl-configuration
              (settings '(("vm.dirty_background_ratio" . "5")
                          ("vm.dirty_ratio" . "25")
                          ("kernel.hung_task_timeout_secs" . "30")))))
    (bluetooth-service)
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
    (service mingetty-service-type
             (mingetty-configuration
              (tty "tty7")
              (auto-login username)))
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
