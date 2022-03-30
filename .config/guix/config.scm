(use-modules
 ((srfi srfi-1) #:select (remove))
 (gnu)
 ((gnu packages certs) #:select (nss-certs))
 ((gnu packages fonts) #:select (font-dejavu font-gnu-freefont font-wqy-zenhei))
 ((gnu packages fontutils) #:select (fontconfig))
 ((gnu packages ghostscript) #:select (gs-fonts))
 ((gnu packages linux) #:select (brightnessctl))
 ((gnu packages wm) #:select (swaylock))
 ((gnu packages security-token) #:select (libu2f-host))
 ((gnu packages embedded) #:select (openocd))
 ((gnu packages gnome) #:select (adwaita-icon-theme))
 ((gnu services audio) #:select (mpd-service-type mpd-configuration))
 ((gnu services desktop) #:select (%desktop-services))
 ((gnu services dict) #:select (dicod-service))
 ((gnu services file-sharing) #:select (transmission-daemon-service-type transmission-daemon-configuration))
 ((gnu services mail) #:select (dovecot-service dovecot-configuration protocol-configuration service-configuration unix-listener-configuration userdb-configuration inet-listener-configuration passdb-configuration))
 ((gnu services security-token) #:select (pcscd-service-type))
 ((gnu services syncthing) #:select (syncthing-service-type syncthing-configuration))
 ((gnu services sysctl) #:select (sysctl-service-type sysctl-configuration %default-sysctl-settings))
 ((gnu services xorg) #:select (gdm-service-type))
 ((gnu system setuid) #:select (file-like->setuid-program)))

(define username "CHANGE ME")
(define host-name "CHANGE ME")
(define my-keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))

;; Things not exported by (gnu system)
(define %default-modprobe-blacklist (@@ (gnu system) %default-modprobe-blacklist))

(operating-system
  (host-name host-name)
  (timezone "America/New_York")

  ;; https://someonewhocares.org/hosts/zero/hosts
  (hosts-file (local-file "./hosts"))

  (issue "Morgan's GNU Guix Machine\n\n")

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets (list "/boot/efi"))))

  (swap-devices (list (swap-space (target "/swapfile"))))

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

  (setuid-programs
   (cons
    (file-like->setuid-program (file-append swaylock "/bin/swaylock"))
    %setuid-programs))

  ;; This is where we specify system-wide packages.
  (packages
   (cons*
    glibc-locales
    nss-certs
    adwaita-icon-theme
    
    ;; fonts
    fontconfig
    gs-fonts
    font-dejavu
    font-gnu-freefont
    font-wqy-zenhei

    %base-packages))

  (services
   (cons*
    (dovecot-service
     #:config
     (dovecot-configuration
      (mail-location "maildir:~")
      (listen '("127.0.0.1"))
      (ssl? "no")
      (protocols
       (list (protocol-configuration (name "lmtp"))
             (protocol-configuration (name "imap"))))
      (services (list
                 (service-configuration
                  (kind "lmtp")
                  (client-limit 1)
                  (process-limit 0)
                  (listeners
                   (list (unix-listener-configuration
                          (path "lmtp") (mode "0666")))))
                 (service-configuration
                  (kind "imap")
                  (client-limit 1))))
      (passdbs (list
                (passdb-configuration
                 (driver "static")
                 (args '("nopassword=y")))))
      (userdbs (list
                (userdb-configuration
                 (driver "static")
                 (args '("uid=1000" "gid=997" "allow_all_users=yes"
                         "username_format=%n"
                         "home=/home/pancake/.local/share/mail/%n")))))))

    (dicod-service) ;; Dictionary server

    ;; Security Keys
    (service pcscd-service-type)
    (udev-rules-service 'security-key libu2f-host)

    (udev-rules-service 'brightnessctl brightnessctl)

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

      (delete gdm-service-type))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
