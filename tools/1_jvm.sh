#!/usr/bin/env bash

log_header2 "Installing JVM tools..."

function install_jenv_debian {
    log_info "Installing jenv..."
    if [ ! -d ~/.jenv ]; then
        git clone https://github.com/jenv/jenv.git ~/.jenv
        log_info "Finished installing jenv."
    elif [[ -v DOTFILES_TOOLS_FORCE ]]; then
        cd ~/.jenv && git pull -q && cd - > /dev/null
        log_info "Finished updating jenv."
    else
        log_info "jenv is already installed!"
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

    log_info "Installing JDK..."
    if [ ! -d /usr/lib/jvm/jdk-21+35 ] || [[ -v DOTFILES_TOOLS_FORCE ]]; then
        sudo rm -rf /usr/lib/jvm/jdk-21+35/
        wget https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21%2B35/OpenJDK21U-jdk_x64_linux_hotspot_21_35.tar.gz
        tar xzf OpenJDK21U-jdk_x64_linux_hotspot_21_35.tar.gz
        sudo mv jdk-21+35/ /usr/lib/jvm/
        rm OpenJDK21U-jdk_x64_linux_hotspot_21_35.tar.gz
        log_info "Finished installing JDK."
    else
        log_info "JDK is already installed!"
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
    log_info "Configuring jenv..."
    if is_macos; then
        jenv add /Library/Java/JavaVirtualMachines/ibm-semeru-open-17.jdk/Contents/Home/
        jenv rehash
    else
        if ! jenv versions | grep -q 21; then
            jenv add /usr/lib/jvm/jdk-21+35/ > /dev/null
            jenv rehash
        fi
    fi
    if is_macos; then
        jenv global ibm64-17.0.1
    else
        jenv global 21
    fi
    log_info "Finished configuring jenv."
}

function install_lein_debian {
    log_info "Installing leiningen..."
    if [ ! -e /usr/local/bin/lein ] || [[ -v DOTFILES_TOOLS_FORCE ]]; then
        wget -q https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
        sudo mv lein /usr/local/bin
        sudo chmod a+x /usr/local/bin/lein
        log_info "Finished installing leiningen."
    else
        log_info "leiningen is already installed!"
    fi
}

function install_lein {
    if is_macos; then
        idempotent_brew_install leiningen
    else
        install_lein_debian
    fi
}

function install_clojure_lsp {
    log_info "Installing Clojure LSP..."
    if ! [ -x $(command -v clojure-lsp) ] || [[ -v DOTFILES_TOOLS_FORCE ]]; then
        sudo bash < <(curl -s https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)
        log_info "Finished installing Clojure LSP."
    else
        log_info "Clojure LSP is already installed!"
    fi
}

install_jenv
install_jdk
configure_jenv
install_lein
install_clojure_lsp

log_header2 "Finished installing JVM tools.\n"
