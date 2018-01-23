#!/bin/bash

install_directory=$HOME

usage() {
    echo "usage:    ./install.sh [ -d <home directory> ] <config file name> "
    echo "config file name :    .emacs"
    echo "                      .vimrc"
}

link_emacs() {
    if [[ `ls -a $install_directory|grep -w "^\.emacs$"|wc -l` -eq 0 ]]; then
        touch $install_directory/link_to_emacs
        if [[ $? -eq 0 ]]; then
            echo "succes"
        else
            echo "error"
        fi
    else
        echo "this would overwrite the current .emacs in $HOME; aborting"
    fi
}

link_vimrc() {
    if [[ `ls -a $install_directory|grep -w "^\.vimrc$"|wc -l` -eq 0 ]]; then
        touch $install_directory/link_to_vim
    else
        echo "this would overwrite the current .vimrc in $HOME; aborting"
    fi
}

if [[ $# -lt 1 ]]; then
    usage
fi

while [[ $# -gt 0 ]]
do
    key="$1"
    case $key in
        -d)
            install_directory=$2
            shift
            shift
            ;;
        .emacs)
            link_emacs
            shift
            ;;
        .vimrc)
            link_vimrc
            shift
            ;;

        *)
            usage
            exit
    esac
done
