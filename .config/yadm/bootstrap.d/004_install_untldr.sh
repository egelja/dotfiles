#!/bin/bash
# -*- mode: shell-script -*-
set -euo pipefail

echo "*****************************************************************************"
echo "                              INSTALLING UnTLDR                              "
echo "*****************************************************************************"

cd "$HOME"
rm -rf ./tinytldr

git clone https://github.com/kovmir/tinytldr &&
    cd tinytldr

# Patch styles
cp "$HOME/.config/yadm/bootstrap.d/better_tldr_style.patch" .
patch -i better_tldr_style.patch

# Build and install
make || $(echo "FAILED TO INSTALL UnTLDR" && exit 0)

if [[ $(uname -o) == "Msys" ]]; then
    cp -f ./tldr.exe "$HOME/.local/bin"
else
    cp -f ./tldr "$HOME/.local/bin"
fi

# Remove build directory
cd "$HOME"
rm -rf ./tinytldr

# Init tldr index
"$HOME/.local/bin/tldr" -u
