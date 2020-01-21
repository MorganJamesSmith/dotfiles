#!/bin/sh
# Morgans's Auto Rice Boostrapping Script (MARBS)
# by Morgan Smith <morgan.j.smith@outlook.com>
# Based on LARBS by Luke Smith <luke@lukesmith.xyz>
# License: GNU GPLv3

### OPTIONS AND VARIABLES ###

dotfilesrepo="https://github.com/morganjamessmith/dotfiles.git"
progsfile="https://raw.githubusercontent.com/morganjamessmith/dotfiles/master/.config/progs.csv"
aurhelper="pacaur"
repobranch="master"
name=$(whoami)

### FUNCTIONS ###

installpkg() { pacman --noconfirm --needed -S "$1"; }

grepseq="\"^[PGA]*,\""

refreshkeys() { \
	pacman --noconfirm -Sy archlinux-keyring
	}

newperms() { # Set special sudoers settings for install (or after).
	sed -i "/#MARBS/d" /etc/sudoers
	echo "$* #MARBS" >> /etc/sudoers ;}

manualinstall() { # Installs $1 manually if not installed. Used only for AUR helper here.
	[ -f "/usr/bin/$1" ] || (
	echo "Installing \"$1\", an AUR helper..."
	cd /tmp || exit
	rm -rf "/tmp/$1*"
	curl -sO "https://aur.archlinux.org/cgit/aur.git/snapshot/$1.tar.gz" &&
	sudo -u "$name" tar -xf "$1.tar.gz" &&
	cd "$1" &&
	sudo -u "$name" makepkg --noconfirm -si
	cd /tmp || return) ;}

maininstall() { # Installs all needed programs from main repo.
	installpkg "$1"
	}

gitmakeinstall() {
	dir=$(mktemp -d)
	git clone --depth 1 "$1" "$dir"
	cd "$dir" || exit
	make
	make install
	cd /tmp || return ;}

aurinstall() { \
	echo "Installing \`$1\` ($n of $total) from the AUR. $1 $2"
	sudo -u "$name" $aurhelper -S --noconfirm "$1"
	}

pipinstall() { \
	echo "Installing the Python package \`$1\` ($n of $total). $1 $2"
	command -v pip || installpkg python-pip
	yes | pip install "$1"
	}

installationloop() { \
	([ -f "$progsfile" ] && cp "$progsfile" /tmp/progs.csv) || curl -Ls "$progsfile" | sed '/^#/d' | eval grep "$grepseq" > /tmp/progs.csv
	total=$(wc -l < /tmp/progs.csv)
	aurinstalled=$(pacman -Qqm)
	while IFS=, read -r tag program comment; do
		n=$((n+1))
		echo "$comment" | grep "^\".*\"$" >/dev/null 2>&1 && comment="$(echo "$comment" | sed "s/\(^\"\|\"$\)//g")"
		case "$tag" in
			"A") aurinstall "$program" "$comment" ;;
			"G") gitmakeinstall "$program" "$comment" ;;
			"P") pipinstall "$program" "$comment" ;;
			*) maininstall "$program" "$comment" ;;
		esac
	done < /tmp/progs.csv ;}

putgitrepo() { # Downlods a gitrepo $1 and places the files in $2 only overwriting conflicts
	echo "Downloading and installing config files..."
	[ -z "$3" ] && branch="master" || branch="$repobranch"
	dir=$(mktemp -d)
	[ ! -d "$2" ] && mkdir -p "$2" && chown -R "$name:wheel" "$2"
	chown -R "$name:wheel" "$dir"
	sudo -u "$name" git clone -b "$branch" --depth 1 "$1" "$dir/gitrepo" &&
	sudo -u "$name" cp -rfT "$dir/gitrepo" "$2"
	}

systembeepoff() {
	echo "Getting rid of error beep sound..."
	rmmod pcspkr
	echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf ;}

systemdon() {
    systemctl start $1
    systemctl enable $1
}

### THE ACTUAL SCRIPT ###

### This is how everything happens in an intuitive format and order.

# Refresh Arch keyrings.
refreshkeys || exit

installpkg base-devel
installpkg git

# Allow user to run sudo without password. Since AUR programs must be installed
# in a fakeroot environment, this is required for all builds with AUR.
newperms "%wheel ALL=(ALL) NOPASSWD: ALL"

# Make pacman colorful and adds eye candy on the progress bar because why not.
grep "^Color" /etc/pacman.conf >/dev/null || sed -i "s/^#Color/Color/" /etc/pacman.conf
grep "ILoveCandy" /etc/pacman.conf >/dev/null || sed -i "/#VerbosePkgLists/a ILoveCandy" /etc/pacman.conf

# Use all cores for compilation.
sed -i "s/-j2/-j$(nproc)/;s/^#MAKEFLAGS/MAKEFLAGS/" /etc/makepkg.conf

manualinstall $aurhelper || exit

# The command that does all the installing. Reads the progs.csv file and
# installs each needed program the way required. Be sure to run this only after
# the user has been created and has priviledges to run sudo without a password
# and all build dependencies are installed.
installationloop

# Install the dotfiles in the user's home directory
putgitrepo "$dotfilesrepo" "/home/$name" "$repobranch"

# Systemd system setup
systemdon "systemd-timesyncd"
systemdon "syncthing"

# This line, overwriting the `newperms` command above will allow the user to run
# serveral important commands, `shutdown`, `reboot`, updating, etc. without a password.
newperms "%wheel ALL=(ALL) ALL #MARBS
%wheel ALL=(ALL) NOPASSWD: /usr/bin/shutdown,/usr/bin/reboot,/usr/bin/systemctl suspend,/usr/bin/wifi-menu,/usr/bin/mount,/usr/bin/umount,/usr/bin/pacman -Syu,/usr/bin/pacman -Syyu,/usr/bin/packer -Syu,/usr/bin/packer -Syyu,/usr/bin/systemctl restart NetworkManager,/usr/bin/rc-service NetworkManager restart,/usr/bin/pacman -Syyu --noconfirm,/usr/bin/loadkeys,/usr/bin/yay,/usr/bin/pacman -Syyuw --noconfirm"

# Make zsh the default shell for the user
sed -i "s/^$name:\(.*\):\/bin\/.*/$name:\1:\/bin\/zsh/" /etc/passwd

# Install complete!
