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

# config files to set up
config_files=( '.oh-my-zsh' '.zshrc' '.aliases' '.emacs.d' '.tmux.conf')

for i in "${config_files[@]}"
do
		echo "$i"
done

echo 'testing link folder'
echo '...'
link_folder "dummy_fold"



package_list=( 'zsh' 'pm-sensors' 'htop' 'emacs' 'tree' )
emacs_deps=( 'autoconf' 'textinfo' 'gtk2.0')

# checkinstall : add compiled program to ubuntu package manager

ubuntu_packages=( 'apt-file' )
