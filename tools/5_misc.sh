#!/usr/bin/env bash

log_header2 "Installing miscellaneous tools..."

function install_fonts {
    if is_macos; then
        brew tap homebrew/cask-fonts
        brew_install cask font-sauce-code-pro-nerd-font --fontdir=/Library/Fonts
        brew_install cask font-jetbrains-mono-nerd-font --fontdir=/Library/Fonts
    else
        log_info "Installing fonts..."
        font_installed=false
        if ! fc-list | grep -q SauceCodePro || [[ -v DOTFILES_TOOLS_FORCE ]]; then
            wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/SourceCodePro.zip -O SauceCodePro.zip
            unzip -qq SauceCodePro.zip -d SauceCodePro
            sudo mv SauceCodePro/*.ttf /usr/local/share/fonts
            rm -rf SauceCodePro*
            font_installed=true
        fi
        if ! fc-list | grep -q JetBrainsMono || [[ -v DOTFILES_TOOLS_FORCE ]]; then
            wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/JetBrainsMono.zip -O JetBrainsMono.zip
            unzip -qq JetBrainsMono.zip -d JetBrainsMono
            sudo mv JetBrainsMono/*.ttf /usr/local/share/fonts
            rm -rf JetBrainsMono*
            font_installed=true
        fi
        if [ "$font_installed" = true ]; then
            fc-cache -f -v
        fi
        unset font_installed
        log_info "Finished installing fonts."
    fi
}

function install_htop {
    if is_macos; then
        brew_install htop
    fi
}

function install_docker_macos {
    brew_install cask docker
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
        brew_install kcat
    else
        log_warn "Implement for debian"
    fi
}

function install_ripgrep {
    if is_macos; then
        brew_install ripgrep
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

install_fonts
install_htop
install_docker
install_kcat
install_ripgrep
# install_nyxt

log_header2 "Finished installing miscellaneous tools.\n"
