#!/bin/bash -l
# -*- mode: shell-script -*-

# BROKEN
exit 0

#
# Apt packages
#
echo "*****************************************************************************"
echo "                          INSTALLING APT PACKAGES                            "
echo "*****************************************************************************"

if ! $PERSONAL_MACHINE; then
    exit 0
fi

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
