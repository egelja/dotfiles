#!/usr/bin/env sh
# -*- mode: shell-script -*-

#
# Mingw packages
#
declare -a MINGW_PACKAGES=(
    "mingw-w64-x86_64-jq"
    "mingw-w64-x86_64-cmake"
    "mingw-w64-x86_64-ninja"
    "msys2-runtime-devel"
    "mingw-w64-x86_64-curl"
    "mingw-w64-x86_64-libarchive"
    "unzip"
)

echo "Installing mingw packages:"
for pkg in ${MINGW_PACKAGES[@]}; do
    echo "- $pkg"
done

pacman -Sy ${MINGW_PACKAGES[@]} --noconfirm

#
# Winget Packages
#
declare -a WINGET_PACKAGES=(
    "CoreyButler.NVMforWindows"
    "GNU.Emacs"
)

echo "Installing winget packages:"
for pkg in ${MINGW_PACKAGES[@]}; do
    echo "- $pkg"
    winget install $pkg
done

