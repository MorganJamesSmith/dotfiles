(use-modules
 ((srfi srfi-1) #:select (remove))
 (gnu)
 ((gnu packages admin) #:select (opendoas sudo))
 ((gnu packages base) #:select (glibc-utf8-locales))
 ((gnu packages certs) #:select (nss-certs))
 ((gnu packages ed) #:select (ed))
 ((gnu packages fonts) #:select (font-dejavu font-gnu-freefont font-wqy-zenhei))
 ((gnu packages fontutils) #:select (fontconfig))
 ((gnu packages ghostscript) #:select (gs-fonts))
 ((gnu packages linux) #:select (v4l2loopback-linux-module brightnessctl))
 ((gnu packages nano) #:select (nano))
 ((gnu packages nvi) #:select (nvi))
 ((gnu packages security-token) #:select (libu2f-host))
 ((gnu packages embedded) #:select (openocd))
 ((gnu packages shells) #:select (dash))
 ((gnu packages suckless) #:select (slock))
 ((gnu packages text-editors) #:select (mg))
 ((gnu services audio) #:select (mpd-service-type mpd-configuration))
 ((gnu services pm) #:select (thermald-service-type tlp-service-type tlp-configuration))
 ((gnu services desktop) #:select (%desktop-services))
 ((gnu services dict) #:select (dicod-service))
 ((gnu services file-sharing) #:select (transmission-daemon-service-type transmission-daemon-configuration))
 ((gnu services mail)); #:select (dovecot-service dovecot-configuration protocol-configuration service-configuration))
 ((gnu services security-token) #:select (pcscd-service-type))
 ((gnu services syncthing) #:select (syncthing-service-type syncthing-configuration))
 ((gnu services sysctl) #:select (sysctl-service-type sysctl-configuration %default-sysctl-settings))
 ((gnu services xorg) #:select (gdm-service-type xorg-server-service-type xorg-configuration))
 ((gnu system setuid) #:select (file-like->setuid-program)))

(define username "CHANGE ME")
(define host-name "CHANGE ME")
(define my-keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))

;; Things not exported by (gnu system)
(define %default-modprobe-blacklist (@@ (gnu system) %default-modprobe-blacklist))

(operating-system
  (host-name host-name)
  (timezone "America/New_York")

  ;; US keyboard but replace caps with ctrl
  (keyboard-layout my-keyboard-layout)

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

  (groups (cons
           (user-group (name "plugdev") (system? #t))
           %base-groups))

  (users (cons
          (user-account
           (name username)
           (comment username)
           (group "users")
           (supplementary-groups '("wheel"   ; polkit group
                                   "video"
                                   "audio"   ; amixer commands
                                   "transmission"
                                   "dialout" ; serial TTYs
                                   "plugdev" ; security key
                                   "kvm")))  ; qemu
          %base-user-accounts))

  (sudoers-file #f)

  (setuid-programs
   (cons*
    (file-like->setuid-program (file-append opendoas "/bin/doas"))
    (remove
     (lambda (program)
       (member program
               (map file-like->setuid-program
                    (list (file-append sudo "/bin/sudo")
                          (file-append sudo "/bin/sudoedit")))))
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
    glibc-locales
    nss-certs
    opendoas ; We already have the binary as it is a setuid-program. This is
             ; for the documentation
    ed ;; the standard editor

    ;; fonts
    fontconfig
    gs-fonts
    font-dejavu
    font-gnu-freefont
    font-wqy-zenhei

    (remove
     (lambda (package)
       (memq package (list sudo nano nvi mg)))
     %base-packages)))

  (services
   (cons*
    (dovecot-service
     #:config
     (dovecot-configuration
      (mail-location "maildir:~/.local/share/mail/local")
      (listen '("127.0.0.1"))
      ;; I do not need ssl support in a locally running dovecot.
      (ssl? "no")
      (protocols
       (list (protocol-configuration
              (name "lmtp")
              (mail-max-userip-connections 1))))
      (services (list
                 (service-configuration
                  (kind "lmtp")
                  (client-limit 1)
                  (process-limit 0)
                  (listeners
                   (list (unix-listener-configuration
                          (path "lmtp") (mode "0666")))))))))

    (service xorg-server-service-type
             (xorg-configuration
              (keyboard-layout my-keyboard-layout)))
    (dicod-service) ;; Dictionary server

    ;; Security Keys
    (service pcscd-service-type)
    (udev-rules-service 'security-key libu2f-host)
    (udev-rules-service 'brightnessctl brightnessctl)

    (service tlp-service-type
             (tlp-configuration
              (cpu-scaling-governor-on-ac (list "performance"))
              (cpu-scaling-governor-on-bat (list "powersave"))))

    (service thermald-service-type)

    (udev-rules-service 'openocd openocd)

    (udev-rules-service
     'planck-dfu
     (udev-rule
      "99-planck.rules"
      "ACTION==\"add\", SUBSYSTEM==\"usb\", ATTR{idVendor}==\"0483\", ATTR{idProduct}==\"df11\", GROUP=\"dialout\", MODE=\"0660\"\n"))

    (udev-rules-service
     'jlink
     (udev-rule
      "99-jlink.rules"
      "ACTION==\"add\", SUBSYSTEM==\"usb\", ATTR{product}==\"J-Link\", ATTR{manufacturer}==\"SEGGER\", GROUP=\"dialout\", MODE=\"0660\"\n"))

    (udev-rules-service
     'blackmagic
     (udev-rule
      "99-blackmagic.rules"
      (string-append
       "SUBSYSTEM==\"tty\", ACTION==\"add\", ATTRS{interface}==\"Black Magic GDB Server\", SYMLINK+=\"ttyBmpGdb\""
       (string #\newline)
       "SUBSYSTEM==\"tty\", ACTION==\"add\", ATTRS{interface}==\"Black Magic UART Port\", SYMLINK+=\"ttyBmpTarg\""
       (string #\newline)
       "SUBSYSTEM==\"usb\", ENV{DEVTYPE}==\"usb_device\", ATTR{idVendor}==\"1d50\", ATTR{idProduct}==\"6017\", MODE=\"0666\""
       (string #\newline)
       "SUBSYSTEM==\"usb\", ENV{DEVTYPE}==\"usb_device\", ATTR{idVendor}==\"1d50\", ATTR{idProduct}==\"6018\", MODE=\"0666\""
       (string #\newline))))

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
        %desktop-services

      (guix-service-type config =>
                         (guix-configuration
                          (inherit config)
                          (discover? #t)
                          (authorized-keys
                           (append (list (local-file "./desktop.pub"))
                                   %default-authorized-guix-keys))))
      (delete gdm-service-type)

      ;; Helps with IO related freezing
      (sysctl-service-type
       config =>
       (sysctl-configuration
        (settings (append
                   '(("vm.dirty_background_ratio" . "5")
                     ("vm.dirty_ratio" . "25")
                     ("kernel.hung_task_timeout_secs" . "30"))
                   %default-sysctl-settings)))))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
