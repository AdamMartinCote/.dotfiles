#!/bin/bash


install_dir="$HOME"
dropbox_dir="$HOME/Dropbox/dotfiles"

function link_folder {
		item=$1
		backup_if_exist "$install_d10ir/$1"
		ln -s "$source_directory/$item" "$install_dir/$item"
}

function backup_if_exist {
		counter=1
		while [ -d "$1.auto_backup$counter" ]; do
				counter++
		done
		if [ -d "$1" ]; then
				mv "$1" "$1.auto_backup$counter"
 				echo "\"$1\" exists, backup created"
		fi
}

function get_os {
		# Determine OS platform
		UNAME=$(uname | tr "[:upper:]" "[:lower:]")
		# If Linux, try to determine specific distribution
		if [ "$UNAME" == "linux" ]; then
				# If available, use LSB to identify distribution
				if [ -f /etc/lsb-release -o -d /etc/lsb-release.d ]; then
						export DISTRO=$(lsb_release -i | cut -d: -f2 | sed s/'^\t'//)
						# Otherwise, use release info file
				else
						export DISTRO=$(ls -d /etc/[A-Za-z]*[_-][rv]e[lr]* | grep -v "lsb" | cut -d'/' -f3 | cut -d'-' -f1 | cut -d'_' -f1)
				fi
		fi
		# For everything else (or if above failed), just use generic identifier
		[ "$DISTRO" == "" ] && export DISTRO=$UNAME
		unset UNAME
		echo $DISTRO
}
# config files to set up
config_files=( '.oh-my-zsh' '.zshrc' '.aliases' '.emacs.d' '.tmux.conf')

for i in "${config_files[@]}"
do
		echo "$i"
done


package_list=( 'zsh' 'pm-sensors' 'htop' 'emacs' 'tree' )
emacs_deps=( 'autoconf' 'textinfo' 'gtk2.0')

# checkinstall : add compiled program to ubuntu package manager

ubuntu_packages=( 'apt-file' )


echo $(get_os)
