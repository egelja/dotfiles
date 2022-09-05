#!/bin/bash
# -*- mode: shell-script -*-

echo "Installing lesspipe.sh!"

cd "$HOME"
git clone https://github.com/wofr06/lesspipe.git &&
    cd lesspipe

# Build and install
./configure --prefix="$HOME/.local"
make
make test
make install

# Remove build directory
cd "$HOME"
rm -r ./lesspipe
