#!/bin/bash


install_directory="$HOME"
source_directory="$HOME/code/dotfiles/"

function link_folder {
		item=$1
		backup_if_exist "$install_directory/$1"
		ln -s "$source_directory/$item" "$install_directory/$item"
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

link_folder dummydir
