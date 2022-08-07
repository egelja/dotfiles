#!/bin/bash
# -*- mode: shell-script -*-

echo "Installing UnTLDR!"

cd ~
git clone https://github.com/unInstance/untldr.git &&
    cd untldr

# Patch styles
cp "$HOME/.config/yadm/bootstrap.d/better_tldr_style.patch" .
patch -i better_tldr_style.patch

# Build and install
make
make install PREFIX="~"

# Remove build directory
cd ~
rm -rf ./untldr

# Set $HOME for Windows
if [[ $(uname -o) == "Msys" ]]; then
    echo 'Setting $HOME because windows is stupid!'
    cmd /c 'setx HOME "%userprofile%"'
fi

# Init tldr index
~/bin/tldr -u
