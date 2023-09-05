#!/bin/bash
# -*- mode: shell-script -*-

#
# Apt packages
#
echo "*****************************************************************************"
echo "                          INSTALLING APT PACKAGES                            "
echo "*****************************************************************************"

echo "Is this a personal machine?"
select yn in "Yes" "No"; do
    case $yn in
        Yes ) break;;
        No ) exit 0;;
    esac
done

declare -a PACKAGES=(
    "emacs"
    "jq"
    "gpg"
    "htop"
    "build-essential"
    "cmake"
    "ninja-build"
    "libcurl4-openssl-dev"
    "libarchive-dev"
    "zip"
    "unzip"
    "diffutils"
    "tree"
    "libsource-highlight-common"
    "source-highlight"
    "libarchive-tools"
    "p7zip"
)

echo "Installing apt packages:"
for pkg in ${PACKAGES[@]}; do
    echo "- $pkg"
done

sudo apt update -y &&
    sudo apt install ${PACKAGES[@]} -y
