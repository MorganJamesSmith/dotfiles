(use-modules
 ((srfi srfi-1) #:select (remove))
 ((nongnu packages linux) #:select (linux linux-firmware))
 ((nongnu system linux-initrd) #:select (microcode-initrd))
 ((gnu services linux) #:select (kernel-module-loader-service-type))
 (gnu)
 ((gnu packages admin) #:select (opendoas sudo))
 ((gnu packages base) #:select (glibc-utf8-locales))
 ((gnu packages certs) #:select (nss-certs))
 ((gnu packages curl) #:select (curl))
 ((gnu packages gl) #:select (mesa))
 ((gnu packages security-token) #:select (libu2f-host))
 ((gnu packages zile) #:select (zile))
 ((gnu packages nano) #:select (nano))
 ((gnu packages linux) #:select (v4l2loopback-linux-module))
 ((gnu packages rsync) #:select (rsync))
 ((gnu packages shells) #:select (dash))
 ((gnu packages suckless) #:select (slock))
 ((gnu packages version-control) #:select (git))
 ((gnu packages wget) #:select (wget))
 ((gnu services audio) #:select (mpd-service-type mpd-configuration mpd-output))
 ((gnu services dbus) #:select (dbus-service))
 ((gnu services desktop) #:select (%desktop-services bluetooth-service))
 ((gnu services file-sharing) #:select (transmission-daemon-service-type transmission-daemon-configuration))
 ((gnu services security-token) #:select (pcscd-service-type))
 ((gnu services syncthing) #:select (syncthing-service-type syncthing-configuration))
 ((gnu services sysctl) #:select (sysctl-service-type sysctl-configuration))
 ((gnu services xorg) #:select (gdm-service-type xorg-server-service-type))
 (guix gexp))

(define username "CHANGE ME")
(define host-name "CHANGE ME")
(define my-keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))

;; Things not exported by (gnu system)
(define %default-modprobe-blacklist (@@ (gnu system) %default-modprobe-blacklist))

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
                (supplementary-groups '("wheel"   ; polkit group
                                        "lp"      ; bluetooth
                                        "video"
                                        "audio"   ; amixer commands
                                        "transmission"
                                        "dialout" ; tty stuff
                                        "plugdev" ; security keys
                                        "kvm")))  ; qemu
               %base-user-accounts))

  (sudoers-file #f)

  (setuid-programs
   (cons*
    (file-append opendoas "/bin/doas")
    (remove
     (lambda (file)
       (member file
               (list (file-append sudo "/bin/sudo")
                     (file-append sudo "/bin/sudoedit"))))
     %setuid-programs)))

  (pam-services
   (cons
    (unix-pam-service "doas")
    (remove
     (lambda (service)
       (equal? (pam-service-name service) "sudo"))
     (base-pam-services))))

  ;; This is where we specify system-wide packages.
  (packages
   (cons*
    glibc-utf8-locales
    nss-certs
    opendoas ; We already have the binary as it is a setuid-program. This is
             ; for the documentation
    (remove
     (lambda (package)
       (memq package (list
                      sudo
                      zile
                      nano)))
     %base-packages)))

  (services
   (cons*
    (service xorg-server-service-type)
    (simple-service
     'opendoas-config etc-service-type
     `(("doas.conf"
        ,(plain-file
          "doas.conf"
          (string-join
           (list
            "permit persist"
            "setenv { PATH=/bin:/usr/bin:/var/guix/profiles/system/profile/bin }"
            username (string #\newline)))))))
    (service transmission-daemon-service-type
             (transmission-daemon-configuration
              (download-dir "/torrents")))
    (service syncthing-service-type (syncthing-configuration (user username)))
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
         ("/usr/bin/env" ,(file-append coreutils "/bin/env")))))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
