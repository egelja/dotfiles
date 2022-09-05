#!/bin/bash
# -*- mode: shell-script -*-

echo "Installing UnTLDR!"

cd "$HOME"
git clone https://github.com/unInstance/untldr.git &&
    cd untldr
git checkout a36cdcd

# Patch styles
cp "$HOME/.config/yadm/bootstrap.d/better_tldr_style.patch" .
patch -i better_tldr_style.patch

# Build and install
make
make install PREFIX="$HOME/.local"

# Remove build directory
cd "$HOME"
rm -rf ./untldr

# Init tldr index
"$HOME/.local/bin/tldr" -u
