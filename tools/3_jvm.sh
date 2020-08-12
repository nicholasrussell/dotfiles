#!/usr/bin/env bash

function install_jenv_debian {
    git clone https://github.com/jenv/jenv.git ~/.jenv
}

function install_jenv_macos {
    brew install jenv
}

function install_jenv {
    if is_macos; then
        install_jenv_macos
    else
        install_jenv_debian
    fi
}

function install_jdk_debian {

}

function install_jdk_macos {
    wget -q https://github.com/AdoptOpenJDK/openjdk14-binaries/releases/download/jdk-14.0.2%2B12_openj9-0.21.0/OpenJDK14U-jdk_x64_mac_openj9_14.0.2_12_openj9-0.21.0.pkg
    installer -pkg OpenJDK14U-jdk_x64_mac_openj9_14.0.2_12_openj9-0.21.0.pkg -target /
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
        jenv add /Library/Java/JavaVirtualMachines/adoptopenjdk-14-openj9.jdk/Contents/Home/
    else
        # TODO
    fi
    jenv rehash
    jenv global openjdk64-14.0.1
}

function install_lein_debian {
    # TODO
}

function install_lein_macos {
    # wget -q https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -o ~/bin/lein
    # chmod a+x ~/bin/lein
    brew install leiningen
}

function install_lein {
    if is_macos; then
        install_lein_macos
    else
        install_lein_debian
    fi
}

install_jenv
install_jdk
configure_jenv
install_lein
