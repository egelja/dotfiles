#!/bin/bash

echo "*****************************************************************************"
echo "                              INSTALLING YADM                                "
echo "*****************************************************************************"

# Set $HOME for Windows
if [[ $(uname -o) == "Msys" ]]; then
    echo 'Setting $HOME because windows is stupid!'
    cmd /c 'setx HOME "%userprofile%"'
fi

completion_dir="${BASH_COMPLETION_USER_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/bash-completion}/completions"
local_bin="$HOME/.local/bin"

# Update to ssh url
echo "Updating the yadm repo origin URL"
yadm remote set-url origin "git@github.com:MrAwesomeRocks/dotfiles.git"

# Install yadm locally
echo "Installing YADM locally"

mkdir -p "$local_bin"
curl -fL https://github.com/TheLocehiliosan/yadm/raw/master/yadm -o "$local_bin/yadm" &&
    chmod a+x "$local_bin/yadm"

mkdir -p "$completion_dir"
curl -fL https://github.com/TheLocehiliosan/yadm/raw/master/completion/bash/yadm -o "$completion_dir/yadm"
