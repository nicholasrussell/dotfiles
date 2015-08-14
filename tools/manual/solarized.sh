#!/usr/bin/env bash

# Create new profile based on Default called Solarized

printf "Have you created a new profile based on Default called Solarized and are you switched to that profile? [Y/n] "
read -n 1 -r REPLY < /dev/tty
echo
if echo $REPLY | grep -E '^[Yy]$' > /dev/null
then
    git clone https://github.com/Anthony25/gnome-terminal-colors-solarized.git ~/gnome-terminal-colors-solarized
    ~/gnome-terminal-colors-solarized/install.sh -s dark -p Profile0
    rm -rf ~/gnome-terminal-colors-solarized

    rm -f ~/.dircolors
    #wget --no-check-certificate -O ~/.dircolors https://raw.github.com/seebi/dircolors-solarized/master/dircolors.256dark
    wget --no-check-certificate -O ~/.dircolors https://raw.github.com/seebi/dircolors-solarized/master/dircolors.ansi-dark
    eval `dircolors ~/.dircolors`
else
    exit 1
fi
