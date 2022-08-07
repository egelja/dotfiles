#!/usr/bin/env sh

completion_dir="${BASH_COMPLETION_USER_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/bash-completion}/completions"

# Update to ssh url
echo "Updating the yadm repo origin URL"
yadm remote set-url origin "git@github.com:MrAwesomeRocks/dotfiles.git"

# Install yadm locally
echo "Installing YADM locally"

mkdir -p ~/bin
curl -fL https://github.com/TheLocehiliosan/yadm/raw/master/yadm -o ~/bin/yadm &&
    chmod a+x ~/bin/yadm

mkdir -p "$completion_dir"
curl -fL https://github.com/TheLocehiliosan/yadm/raw/master/completion/bash/yadm -o "$completion_dir/yadm"
