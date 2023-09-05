#!/bin/bash
# -*- mode: shell-script -*-

echo "*****************************************************************************"
echo "                              INSTALLING SDKMAN                              "
echo "*****************************************************************************"

echo "Do you want to install NVM?"
select yn in "Yes" "No"; do
    case $yn in
        Yes ) break;;
        No ) exit;;
    esac
done

echo "Installing SDKMAN!"
curl -s "https://get.sdkman.io" | bash
