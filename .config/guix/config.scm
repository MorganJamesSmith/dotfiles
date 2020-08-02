(use-modules (srfi srfi-1)) ; remove function

(use-modules (gnu))
(use-service-modules pm) ; power management
(use-service-modules networking)
(use-service-modules desktop xorg)
(use-system-modules nss locale)
(use-package-modules certs admin)

(operating-system
  (host-name "guix")
  (timezone "America/New_York")

  (kernel-arguments (append
                     '("modprobe.blacklist=pcspkr")
                     %default-kernel-arguments))

  (issue "Morgan's GNU Guix Machine\n\n")

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")
                (timeout 0)))

  (swap-devices (list "/swapfile"))

  ;; Specify a mapped device for the encrypted root partition.
  ;; The UUID is that returned by 'cryptsetup luksUUID'.
  (mapped-devices
   (list (mapped-device
          (source (uuid "2c789b36-ceea-4f46-98b4-9ffe226df489"))
          (target "my-root")
          (type luks-device-mapping))))

  (file-systems (append
                 (list (file-system
                         (device (file-system-label "my-root"))
                         (mount-point "/")
                         (type "ext4")
                         (dependencies mapped-devices))
                       (file-system
                         (device (uuid "9AF9-367B" 'fat))
                         (mount-point "/boot/efi")
                         (type "vfat")))
                 %base-file-systems))

  (users (cons (user-account
                (name "pancake")
                (comment "pancake")
                (group "users")
                (supplementary-groups '("wheel"
                                        "audio"
                                        "video"
                                        "lp")))
               %base-user-accounts))

  (setuid-programs (cons (file-append opendoas "/bin/doas")
                         %setuid-programs))

  ;; This is where we specify system-wide packages.
  (packages (cons nss-certs
                  (remove
                   (lambda (package)
                     (memq package
                           (map specification->package
                                '("nano"
                                  "zile"
                                  "bash-completion"))))
                   %base-packages)))

  (services
   (cons* (service tlp-service-type)  ; power management
          (service slim-service-type)
          (bluetooth-service)
          (remove (lambda (service)
                    (let ((type (service-kind service)))
                      (or (memq type
                                (list gdm-service-type))
                          (eq? (service-type-name type)
                               'network-manager-applet))))
                    %desktop-services))))
