#!/usr/bin/env bash

log_header2 "Installing JVM tools..."

function install_jenv_debian {
    git clone https://github.com/jenv/jenv.git ~/.jenv
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

function install_jdk {
    if is_macos; then
        install_jdk_macos
    else
        log_warn "Implement for debian"
    fi
}

function configure_jenv {
    if is_macos; then
        jenv add /Library/Java/JavaVirtualMachines/ibm-semeru-open-17.jdk/Contents/Home/
    else
        log_warn "Implement for debian"
    fi
    jenv rehash
    jenv global ibm64-17.0.1
}

function install_lein_debian {
    # TODO
    # wget -q https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -o ~/bin/lein
    # chmod a+x ~/bin/lein
    log_warn "Implement for debian"
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
