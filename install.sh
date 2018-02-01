#!/bin/bash
# Automatically create symlinks in home folder (Or specified folder)
# for specified config files

install_directory=$HOME

usage() {
    echo "usage:    ./install.sh [ -d <home directory> ] <config file name> "
    echo "config file name :    .emacs"
    echo "                      .vimrc"
}

link_emacs() {
    if [[ `ls -a $install_directory|grep -w "^\.emacs$"|wc -l` -eq 0 ]]; then
        ln -s .emacs $install_directory/.emacs
        if [[ $? -eq 0 ]]; then
            echo "link to .emacs was created in $install_directory"
        else
            echo "error: could not create symlink in $install_directory"
        fi
    else
        echo "this would overwrite the current .emacs in $install_directory; aborting"
    fi
}

link_vimrc() {
    if [[ `ls -a $install_directory|grep -w "^\.vimrc$"|wc -l` -eq 0 ]]; then
        ln -s .vimrc $install_directory/.vimrc 2>/dev/null
        if [[ $? -eq 0 ]]; then
            echo "link to .vimrc was created in $install_directory"
        else
            echo "error: could not create symlink in $install_directory"
        fi
    else
        echo "this would overwrite the current .vimrc in $install_directory; aborting"
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
