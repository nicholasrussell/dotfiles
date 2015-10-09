#!/usr/bin/env bash

# Java
add_ppa webupd8team/java
echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections

# Chrome
if [ ! -e /etc/apt/sources.list.d/google-chrome.list ]; then
    log_info "Adding Google Chrome sources..."
    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'
fi

# HipChat
if [ ! -e /etc/apt/sources.list.d/atlassian-hipchat.list ]; then
    log_info "Adding HipChat sources..."
    echo "deb http://downloads.hipchat.com/linux/apt stable main" | sudo tee /etc/apt/sources.list.d/atlassian-hipchat.list
    sudo wget -q -O - https://www.hipchat.com/keys/hipchat-linux.key | sudo apt-key add -
fi

# VirtualBox
if [ ! -e /etc/apt/sources.list.d/virtualbox.list ]; then
    log_info "Adding VirtualBox sources..."
    wget -q -O - https://www.virtualbox.org/download/oracle_vbox.asc | sudo apt-key add -
    sudo sh -c 'echo "deb http://download.virtualbox.org/virtualbox/debian trusty contrib" > /etc/apt/sources.list.d/virtualbox.list'
fi

# NodeJS
if [ ! -e /etc/apt/sources.list.d/nodesource.list ]; then
    log_info "Adding NodeJS sources..."
    curl -sL https://deb.nodesource.com/setup_0.12 | sudo bash -
fi

# Ansible
add_ppa ansible/ansible

## Apt
log_header2 "Updating apt..."
apt_update

# Potentially needed for meld:
#  libgtksourceview2.0-0 libgtksourceview2.0-common python-gnome2 python-gtksourceview2 python-pyorbit libglib2.0-dev

dotfiles_apt_tools=(
    wget
    curl
    git

    build-essential
    libssl-dev
    software-properties-common
    python-software-properties
    m4
    flex
    bison
    g++
    gperf
    libsqlite3-dev
    libfontconfig1-dev
    libicu-dev
    libfreetype6
    libpng-dev
    libjpeg-dev
    libX11-dev
    libxext-dev
    libreadline-dev
    bash-completion
    ttf-ancient-fonts
    gitk
    git-flow
    # ttf-mscorefonts-installer
    # zsh
    oracle-java7-installer
    oracle-java8-installer
    mono-complete
    libmono-winforms2.0-cil
    flashplugin-installer
    libffi-dev
    python
    python3
    python-dev
    python-pip
    haskell-platform
    htop
    xclip
    unzip
    p7zip-full
    p7zip-rar
    tree
    valgrind
    gdb
    gedit
    emacs
    vim
    google-chrome-stable
    firefox
    nodejs
    jq
    exif
    kdiff3
    unity-tweak-tool
    gnome-tweak-tool
    dconf-cli
    rdesktop
    dkms # Recommended for VirtualBox
    virtualbox-4.3 # Downloads: http://download.virtualbox.org/virtualbox/
    vagrant
    perl
    julia
    apache2
    tomcat7
    # tomcat8 # dne
    dia
    graphviz
    postgresql-9.3
    pgadmin3
    # mongodb
    # redis
    ansible
    krita
    hipchat
    hexchat
    # wireshark
)

dotfiles_apt_tools=($(setdiff "${dotfiles_apt_tools[*]}" "$(dpkg --get-selections | grep -v deinstall | awk '{print $1}' | sed 's/:.*//')"))

if (( ${#dotfiles_apt_tools[@]} > 0 )); then
    log_info "Installing apt tools..."

    for dotfiles_apt_tool in "${dotfiles_apt_tools[@]}"; do
        log_info "Installing ${dotfiles_apt_tool}..."
        apt_install "$dotfiles_apt_tool"
    done

    unset dotfiles_apt_tool
fi
unset dotfiles_apt_tools

log_success "Finished installing tools via apt\n"
