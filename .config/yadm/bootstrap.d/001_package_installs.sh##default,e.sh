#!/bin/bash
# -*- mode: shell-script -*-

#
# Apt packages
#
declare -a PACKAGES=(
    "emacs"
    "jq"
    "gpg"
    "htop"
    "firerfox"
    "thunderbird"
    "build-essential"
    "cmake"
    "ninja"
    "libcurl-dev"
    "libarchive-dev"
    "zip"
    "unzip"
    "diffutils"
)

echo "Installing apt packages:"
for pkg in ${PACKAGES[@]}; do
    echo "- $pkg"
done

sudo apt update -y &&
    sudo apt install ${PACKAGES[@]} -y
