#!/bin/bash
# -*- mode: shell-script -*-

echo "Installing lesspipe.sh!"

cd "$HOME"
git clone https://github.com/wofr06/lesspipe.git &&
    cd lesspipe

# Build and install
./configure --prefix="$HOME/.local"
make
if [[ $(uname -o) == "Msys" ]]; then
    echo "Skipping tests as on windows"
else
    make test
fi
make install

# Remove build directory
cd "$HOME"
rm -rf ./lesspipe
