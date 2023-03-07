#!/usr/bin/env bash

log_header2 "Installing miscellaneous tools..."

function install_fonts {
    if is_macos; then
        idempotent_brew_tap homebrew/cask-fonts
        idempotent_brew_install cask font-source-code-pro --fontdir=/Library/Fonts
    else
        wget -q https://fonts.google.com/download?family=Source%20Code%20Pro -O SourceCodePro.zip
        unzip -qq SourceCodePro.zip -d SourceCodePro
        sudo mv SourceCodePro/*.ttf /usr/local/share/fonts
        fc-cache -f -v
        rm -rf SourceCodePro*
    fi
}

function install_htop_debian {
    apt_install htop
}

function install_htop {
    if is_macos; then
        idempotent_brew_install htop
    else
        install_htop_debian
    fi
}

function install_docker_macos {
    idempotent_brew_install cask docker
    idempotent_brew_install docker-machine
    idempotent_brew_install docker-compose
}

function install_docker {
    if is_macos; then
        install_docker_macos
    else
        log_warn "Implement for debian"
    fi
}

function install_kcat {
    if is_macos; then
        idempotent_brew_install kcat
    else
        log_warn "Implement for debian"
    fi
}

function install_ripgrep {
    if is_macos; then
        idempotent_brew_install ripgrep
    else
        log_warn "Implement for debian"
    fi
}

function install_babashka {
    idempotent_brew_install borkdude/brew/babashka
}

function install_wm {
    if is_macos; then
        log_warn "Implement for Mac OS"
    else
        if ! command -v nvm > /dev/null 2>&1; then
            /usr/lib/apt/apt-helper download-file https://debian.sur5r.net/i3/pool/main/s/sur5r-keyring/sur5r-keyring_2023.02.18_all.deb keyring.deb SHA256:a511ac5f10cd811f8a4ca44d665f2fa1add7a9f09bef238cdfad8461f5239cc4
            sudo apt install ./keyring.deb
            rm keyring.deb
            echo "deb http://debian.sur5r.net/i3/ $(grep '^DISTRIB_CODENAME=' /etc/lsb-release | cut -f2 -d=) universe" | sudo tee /etc/apt/sources.list.d/sur5r-i3.list
            apt_update
            apt_install i3
        fi
    fi
}

function install_nyxt {
    if is_macos; then
        log_warn "Implement for Mac OS"
    else
        if ! command -v nyxt > /dev/null 2>&1; then
            sudo mkdir -p /opt/nyxt
            sudo chown -R nrussell:nrussell /opt/nyxt
            wget -qO- https://github.com/atlas-engineer/nyxt/releases/download/2.2.4/nyxt-2.2.4.tar.xz | tar -xJ -C /opt/nyxt
            sudo ln -sf /opt/nyxt/usr/local/bin/nyxt /usr/local/bin/nyxt
            sudo chown -R nrussell:nrussell /usr/local/bin/nyxt
        fi
    fi
}

if is_macos; then
    idempotent_brew_install coreutils
    idempotent_brew_install fd
    idempotent_brew_install cmake
    idempotent_brew_install libvterm
fi

install_fonts
install_htop
install_docker
install_kcat
install_ripgrep
install_babashka
install_wm
# install_nyxt

log_header2 "Finished installing miscellaneous tools.\n"
