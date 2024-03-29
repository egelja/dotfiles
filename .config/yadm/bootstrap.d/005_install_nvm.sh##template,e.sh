#!/bin/bash
# -*- mode: shell-script -*-

# WARNING: Do not edit this file.
# It was generated by processing {{ yadm.source }}

{% if yadm.os == "Msys" %}

# Do nothing, installed through Winget

{% else %}

echo "*****************************************************************************"
echo "                               INSTALLING NVM                                "
echo "*****************************************************************************"

nvm_latest_tag=$(
    curl -sL https://api.github.com/repos/nvm-sh/nvm/releases/latest |
        jq -r ".tag_name"
)
curl -o- "https://raw.githubusercontent.com/nvm-sh/nvm/$nvm_latest_tag/install.sh" | bash

{% endif %}
