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
    wget -q https://github.com/AdoptOpenJDK/openjdk14-binaries/releases/download/jdk-14.0.2%2B12_openj9-0.21.0/OpenJDK14U-jdk_x64_mac_openj9_14.0.2_12_openj9-0.21.0.pkg
    installer -pkg OpenJDK14U-jdk_x64_mac_openj9_14.0.2_12_openj9-0.21.0.pkg -target /
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
        jenv add /Library/Java/JavaVirtualMachines/adoptopenjdk-14-openj9.jdk/Contents/Home/
    else
        log_warn "Implement for debian"
    fi
    jenv rehash
    jenv global openjdk64-14.0.1
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
# install_jdk
# configure_jenv
install_lein

log_info "Finished installing JVM tools.\n"
