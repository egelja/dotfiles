#!/bin/bash
# -*- mode: shell-script -*-

# WARNING: Do not edit this file.
# It was generated by processing {{ yadm.source }}

{% if yadm.os == "Msys" %}

echo "*****************************************************************************"
echo "                           INSTALLING GNUWIN TOOLS                           "
echo "*****************************************************************************"

mkdir -p "$HOME/.local"
cd "$HOME/.local"

__install_from_sourceforge() {
    curl -L $1 -o download.zip &&
        unzip -u download.zip &&  # unzip from MSys2
        rm download.zip
}

echo "Installing DiffUtils"
__install_from_sourceforge https://sourceforge.net/projects/gnuwin32/files/diffutils/2.8.7-1/diffutils-2.8.7-1-bin.zip/download

echo "Installing LibIntl (DiffUtils dep)"
__install_from_sourceforge https://sourceforge.net/projects/gnuwin32/files/libintl/0.14.4/libintl-0.14.4-bin.zip/download

echo "Installing LibIConv (DiffUtils dep)"
__install_from_sourceforge https://sourceforge.net/projects/gnuwin32/files/libiconv/1.9.2-1/libiconv-1.9.2-1-bin.zip/download

echo "Installing Zip"
__install_from_sourceforge https://sourceforge.net/projects/gnuwin32/files/zip/3.0/zip-3.0-bin.zip/download

echo "Installing UnZip"
__install_from_sourceforge https://sourceforge.net/projects/gnuwin32/files/unzip/5.51-1/unzip-5.51-1-bin.zip/download

{% else %}

# Lol Linux is actually good

{% endif %}
