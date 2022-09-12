(use-modules
 (gnu)
 ((gnu packages certs) #:select (nss-certs))
 ((gnu packages cups) #:select (cups))
 ((gnu packages linux) #:select (brightnessctl))
 ((gnu packages wm) #:select (swaylock))
 ((gnu packages gnome) #:select (adwaita-icon-theme hicolor-icon-theme))
 ((gnu services audio) #:select (mpd-service-type mpd-configuration))
 ((gnu services cups) #:select (cups-service-type cups-configuration))
 ((gnu services desktop) #:select (%desktop-services))
 ((gnu services dict) #:select (dicod-service))
 ((gnu services file-sharing) #:select (transmission-daemon-service-type transmission-daemon-configuration))
 ((gnu services mail) #:select (dovecot-service dovecot-configuration protocol-configuration service-configuration unix-listener-configuration userdb-configuration passdb-configuration))
 ((gnu services syncthing) #:select (syncthing-service-type syncthing-configuration))
 ((gnu services xorg) #:select (gdm-service-type screen-locker-service screen-locker-service-type)))

(define username "CHANGE ME")
(define host-name "CHANGE ME")

;; Things not exported by (gnu system)
(define %default-modprobe-blacklist (@@ (gnu system) %default-modprobe-blacklist))

(define my-glibc-locales
       (make-glibc-utf8-locales
        glibc
        #:locales (list "en_US")
        #:name "glibc-us-utf8-locales"))

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
           (supplementary-groups '("wheel"
                                   "video"
                                   "audio"   ; amixer commands
                                   "transmission"
                                   "dialout" ; serial TTYs
                                   "plugdev" ; security key
                                   "kvm")))  ; qemu
          %base-user-accounts))

  ;; This is where we specify system-wide packages.
  (packages
   (cons*
    my-glibc-locales
    nss-certs
    adwaita-icon-theme
    hicolor-icon-theme
    cups
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
                 (args `("uid=1000" "gid=997" "allow_all_users=yes"
                         "username_format=%n"
                         ,(string-append "home=/home/" username "/.local/share/mail/%n"))))))))

    (service cups-service-type
             (cups-configuration
                (web-interface? #t)))
    
    (dicod-service) ;; Dictionary server

    (screen-locker-service swaylock)

    (udev-rules-service 'brightnessctl brightnessctl)

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
    (service mingetty-service-type
             (mingetty-configuration
              (tty "tty7")
              (auto-login username)))
    (modify-services
        %desktop-services

      (delete gdm-service-type)
      (delete screen-locker-service-type))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
