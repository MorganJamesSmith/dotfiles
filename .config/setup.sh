#!/bin/sh

### OPTIONS AND VARIABLES ###
name="$SUDO_USER"
progsfile=/home/"$name"/.config/progs.csv


### FUNCTIONS ###

installpkg() {
    pacman -q --noconfirm --needed -S "$1"
}

refreshkeys() {
	echo "Refreshing Arch Keyring"
    pacman -q --noconfirm -Sy archlinux-keyring
}

newperms() { # Set special sudoers settings for install (or after).
    sed -i "/#MARBS/d" /etc/sudoers
    echo "$* #MARBS" >> /etc/sudoers
}

maininstall() { # Installs all needed programs from main repo.
	echo "Installing \`$1\` ($n of $total). $1 $2"
    installpkg "$1"
}

installationloop() {
	[ -f "$progsfile" ] && sed '/^#/d' "$progsfile" > /tmp/progs.csv
    total=$(wc -l < /tmp/progs.csv)
    while IFS=, read -r program comment; do
        n=$((n+1))
        comment=$(echo "$comment" | sed "s/^\"\(.*\)\"$/\1/") &&
        maininstall "$program" "$comment"
    done < /tmp/progs.csv
}

systemdon() {
    systemctl start "$1"
    systemctl enable "$1"
}

### THE ACTUAL SCRIPT ###

### This is how everything happens in an intuitive format and order.

# Refresh Arch keyrings.
refreshkeys || exit


# Make pacman colorful and adds eye candy on the progress bar because why not.
grep "^Color" /etc/pacman.conf >/dev/null || sed -i "s/^#Color/Color/" /etc/pacman.conf
grep "ILoveCandy" /etc/pacman.conf >/dev/null || sed -i "/#VerbosePkgLists/a ILoveCandy" /etc/pacman.conf

# Use all cores for compilation.
sed -i "s/-j2/-j$(nproc)/;s/^#MAKEFLAGS/MAKEFLAGS/" /etc/makepkg.conf

# Read the progs.csv file and install each program
installationloop

# Systemd system setup
systemdon "systemd-timesyncd"

# This line, overwriting the `newperms` command above will allow the user to run
# audio setup
usermod -a -G audio "$name"
cat >> /etc/security/limits.conf  <<EOF
@audio          -       rtprio          99
@audio          -       memlock   unlimited
@audio          -       nice           -10
EOF

# doas setup for the wheel group
usermod -a -G wheel "$name"
if [ -f /etc/doas.conf ]; then
    sed -i -e "\$ a permit :wheel persist" -e "/^permit :wheel.*/d" /etc/doas.conf
else
    echo "permit $name persist" > /etc/doas.conf
fi


echo "getting rid of error beep sound..."
rmmod pcspkr
echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf


# serveral important commands, `shutdown`, `reboot`, updating, etc. without a password.
newperms "%wheel ALL=(ALL) ALL #MARBS
%wheel ALL=(ALL) NOPASSWD: /usr/bin/shutdown,/usr/bin/reboot,/usr/bin/systemctl suspend,/usr/bin/wifi-menu,/usr/bin/mount,/usr/bin/umount,/usr/bin/pacman -Syu,/usr/bin/pacman -Syyu,/usr/bin/packer -Syu,/usr/bin/packer -Syyu,/usr/bin/systemctl restart NetworkManager,/usr/bin/rc-service NetworkManager restart,/usr/bin/pacman -Syyu --noconfirm,/usr/bin/loadkeys,/usr/bin/yay,/usr/bin/pacman -Syyuw --noconfirm"

echo "Script Complete!"
