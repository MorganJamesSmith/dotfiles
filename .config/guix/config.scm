(use-modules
 (gnu)
 (gnu packages audio)
 (gnu packages games)
 (gnu packages certs)
 (gnu packages linux)
 (gnu packages wm)
 (gnu packages security-token)
 (gnu packages gnome)
 (gnu services admin)
 (gnu services audio)
 (gnu services desktop)
 (gnu services avahi)
 (gnu services dbus)
 (gnu services desktop)
 (gnu services file-sharing)
 (gnu services mail)
 (gnu services monitoring)
 (gnu services networking)
 (gnu services pm)
 (gnu services security-token)
 (gnu services sound)
 (gnu services sysctl)
 (gnu services xorg))

(define username "CHANGE ME")
(define host-name "CHANGE ME")

(define user
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
                           "kvm")))) ; qemu

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

  (kernel-arguments
   (list
    "quiet"
    "numa=off" ; idk
    "mitigations=off" ; more performance
    "nowatchdog" ; more performance
    (string-append
     "modprobe.blacklist="
     (string-join (cons*
                   "sp5100_tco" ; disable watchdog timer
                   "pcspkr" "snd_pcsp" ; Stop the beeping
                   "uhci_hcd" ; USB 1.1
                   %default-modprobe-blacklist)
                  ","))))

  ;; https://someonewhocares.org/hosts/zero/hosts
  (hosts-file (local-file "./hosts"))

  (issue "Morgan's GNU Guix Machine\n\n")

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot.
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets (list "/boot"))))

  ;; Specify a mapped device for the encrypted root partition.
  ;; The UUID is that returned by 'cryptsetup luksUUID'.
  (mapped-devices
   (list (mapped-device
          (source (uuid "CHANGE ME"))
          (target "guix-root")
          (type luks-device-mapping))))

  (swap-devices (list (swap-space (target "/swapfile")
                                  (dependencies mapped-devices))))

  (file-systems (append
                 (list (file-system
                         (device (file-system-label "guix-root"))
                         (mount-point "/")
                         (type "btrfs")
                         (flags '(lazy-time))
                         (options
                          (alist->file-system-options
                           '(("compress" . "lzo"))))
                         (dependencies mapped-devices))
                       (file-system
                         (device (uuid "CHANGE ME" 'fat))
                         (mount-point "/boot")
                         (type "vfat")))
                 %base-file-systems))

  (groups (cons
           (user-group (name "plugdev") (system? #t))
           %base-groups))

  (users (cons
          user
          %base-user-accounts))

  ;; This is where we specify system-wide packages.
  (packages
   (cons*
    my-glibc-locales
    nss-certs
    adwaita-icon-theme
    hicolor-icon-theme
    bluez-alsa ;; bluetooth audio
    %base-packages))

  (services
   (cons*

    (service vnstat-service-type)

    (service package-database-service-type)
    (service file-database-service-type)

    (service tlp-service-type
             (tlp-configuration
              ;; Disable runtime-pm as this messes with my USBs
              (runtime-pm-on-bat "on")))

    (service dovecot-service-type
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

    (service bluetooth-service-type)

    (service screen-locker-service-type
         (screen-locker-configuration
           (name "swaylock")
           (program (file-append swaylock "/bin/swaylock"))
           (using-pam? #t)
           (using-setuid? #f)))

    ;; Security Keys
    (service pcscd-service-type)
    (udev-rules-service 'security-key libu2f-host)
    (udev-rules-service 'nitro-devices libnitrokey)


    (udev-rules-service 'brightnessctl brightnessctl)
    (udev-rules-service 'steam-devices steam-devices-udev-rules)

    (service transmission-daemon-service-type
             (transmission-daemon-configuration
              (download-dir "/torrents")))
    (service mpd-service-type
             (mpd-configuration
              (user user)
              (music-directory "~/music")
              (playlist-directory "~/.config/mpd/playlists")
              (db-file "~/.config/mpd/database")
              (state-file "~/.config/mpd/state")
              (sticker-file "~/.config/mpd/sticker.sql")))
    (service mingetty-service-type
             (mingetty-configuration
              (tty "tty7")
              (auto-login username)))

      polkit-wheel-service
      fontconfig-file-system-service

      (service network-manager-service-type
               (network-manager-configuration
                (dns "dnsmasq")))
      ;; (service dhcp-client-service-type)
      (service wpa-supplicant-service-type)    ;needed by NetworkManager
      ;; (service modem-manager-service-type)
      (service usb-modeswitch-service-type)

      ;; The D-Bus clique.
      (service avahi-service-type)
      ;; (udisks-service)
      (service upower-service-type)
      ;; (accountsservice-service)
      ;; (service cups-pk-helper-service-type)
      ;; (service colord-service-type)
      ;; (geoclue-service)
      (service polkit-service-type)
      (service elogind-service-type
               (elogind-configuration
                (handle-power-key 'suspend)))
      (service dbus-root-service-type)

      (service openntpd-service-type)

      (service x11-socket-directory-service-type)

      (service pulseaudio-service-type)
      (service alsa-service-type)

    (modify-services
        %base-services

      (guix-service-type
       config =>
       (guix-configuration
        (inherit config)
        (extra-options '("--cache-failures"))))

      ;; Transmission daemon wants this
      (sysctl-service-type
       config =>
       (sysctl-configuration
        (settings (append
                   '(
                     ("net.core.rmem_max" . "4194304")
                     ("net.core.wmem_max" . "1048576"))
                   %default-sysctl-settings)))))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
