#!/usr/bin/env bash

log_header2 "Installing JVM tools..."

function install_jenv_debian {
    if [ -d ~/.jenv ]; then
        cd ~/.jenv && git pull -q && cd -
    else
        git clone https://github.com/jenv/jenv.git ~/.jenv
    fi
}

function install_jenv {
    if is_macos; then
        idempotent_brew_install jenv
    else
        install_jenv_debian
    fi
}

function install_jdk_macos {
    if [ ! -e /Library/Java/JavaVirtualMachines/ibm-semeru-open-17.jdk/ ]; then
        wget -q https://github.com/ibmruntimes/semeru17-binaries/releases/download/jdk-17.0.1%2B12_openj9-0.29.1/ibm-semeru-open-jdk_x64_mac_17.0.1_12_openj9-0.29.1.pkg | sudo installer -pkg -target /
        sudo installer -pkg ibm-semeru-open-jdk_x64_mac_17.0.1_12_openj9-0.29.1.pkg -target /
        rm ibm-semeru-open-jdk_x64_mac_17.0.1_12_openj9-0.29.1.pkg
    fi
}

function install_jdk_debian {
    if [ ! -d /usr/lib/jvm ]; then
        sudo mkdir -p /usr/lib/jvm
    fi

    if [ ! -d /usr/lib/jvm/jdk-17.0.5+8 ]; then
        wget -q https://github.com/adoptium/temurin17-binaries/releases/download/jdk-17.0.5%2B8/OpenJDK17U-jdk_x64_linux_hotspot_17.0.5_8.tar.gz
        tar xzf OpenJDK17U-jdk_x64_linux_hotspot_17.0.5_8.tar.gz
        sudo mv jdk-17.0.5+8/ /usr/lib/jvm/
        rm OpenJDK17U-jdk_x64_linux_hotspot_17.0.5_8.tar.gz
    fi
}

function install_jdk {
    if is_macos; then
        install_jdk_macos
    else
        install_jdk_debian
    fi
}

function configure_jenv {
    if is_macos; then
        jenv add /Library/Java/JavaVirtualMachines/ibm-semeru-open-17.jdk/Contents/Home/
    else
        jenv add /usr/lib/jvm/jdk-17.0.5+8/
    fi
    jenv rehash
    if is_macos; then
        jenv global ibm64-17.0.1
    else
        jenv global 17
    fi
}

function install_lein_debian {
    if [ ! -e /usr/local/bin/lein ]; then
        wget -q https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
        sudo mv lein /usr/local/bin
        sudo chmod a+x /usr/local/bin/lein
        /usr/local/bin/lein
    else
        log_info "lein is already installed"
    fi
}

function install_lein {
    if is_macos; then
        idempotent_brew_install leiningen
    else
        install_lein_debian
    fi
}

install_jenv
install_jdk
configure_jenv
install_lein

log_header2 "Finished installing JVM tools.\n"
