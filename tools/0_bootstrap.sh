#!/usr/bin/env bash
## Misc tools

log_header2 "Ensuring bootstrapping tools exist..."

# https://github.com/cowboy/dotfiles/blob/bbb73a2143737f996913da7379ed20fbd2348d6b/bin/dotfiles#L134
function setdiff {
    local debug skip a b
    if [[ "$1" == 1 ]]; then debug=1; shift; fi
    if [[ "$1" ]]; then
        local setdiffA setdiffB setdiffC
        setdiffA=($1); setdiffB=($2)
    fi
    setdiffC=()
    for a in "${setdiffA[@]}"; do
        skip=
        for b in "${setdiffB[@]}"; do
          [[ "$a" == "$b" ]] && skip=1 && break
        done
        [[ "$skip" ]] || setdiffC=("${setdiffC[@]}" "$a")
    done
    [[ "$debug" ]] && for a in setdiffA setdiffB setdiffC; do
        echo "$a ($(eval echo "\${#$a[*]}")) $(eval echo "\${$a[*]}")" 1>&2
    done
    [[ "$1" ]] && echo "${setdiffC[@]}"
}

function add_ppa {
    grep -h "^deb.*$1" /etc/apt/sources.list.d/* > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        log_info "Adding ppa: $1"
        sudo add-apt-repository -y $1
        return 0
    fi

    # log_warn "Not adding ppa:$1, it already exists"
    return 1
}

function apt_install {
    local pkg
    pkg="$1"
    sudo apt-get -qq install "$pkg" > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        log_error "Failed to install apt package $pkg"
    fi
}

function apt_update {
    sudo apt-get -qq update
}

function is_brew_formula_installed {
    brew list "$1" > /dev/null 2>&1
}

function is_brew_tapped {
    ! brew tap-info "$1" | grep "Not installed" > /dev/null 2>&1
}

function idempotent_brew_tap {
    if ! is_brew_tapped "$1"; then
        log_info "Tapping $1..."
        brew tap "$1"
        log_info "Tapped."
    fi;
}

function idempotent_brew_install {
    local formula="$1"
    if [ "$1" == "cask" ]; then
        formula="$2"
    fi
    if ! is_brew_formula_installed "$formula"; then
        log_info "Installing Brew formula $formula..."
        if [ "$1" == "cask" ]; then
            local temp_args=($@)
            brew install --cask ${temp_args[@]:1}
        else
            brew install $@
        fi;
        log_info "Finished installing $formula."
    else
        log_info "Brew formula $formula is already installed!"
    fi
}

# Package Manager
function update_package_manager_debian {
    apt_update
}

function update_package_manager_macos {
    if ! which brew > /dev/null; then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    brew update > /dev/null 2>&1
}

function update_package_manager {
    log_info "Updating package managers..."
    if is_macos; then
        update_package_manager_macos
    else
        update_package_manager_debian
    fi
}

function install_bootstrap_tools_debian {
    dotfiles_apt_tools=(
        wget
        curl
        git
        build-essential
        software-properties-common
        libssl-dev
        apt-transport-https
        cmake
        libtool
	libtool-bin
        jq
        unzip
        fontconfig
    )
    dotfiles_apt_tools=($(setdiff "${dotfiles_apt_tools[*]}" "$(dpkg --get-selections | grep -v deinstall | awk '{print $1}' | sed 's/:.*//')"))
    for dotfiles_apt_tool in "${dotfiles_apt_tools[@]}"; do
        apt_install "$dotfiles_apt_tool"
    done
    unset dotfiles_apt_tool dotfiles_apt_tools
}

function install_bootstrap_tools_macos {
    idempotent_brew_install wget
    idempotent_brew_install curl
    idempotent_brew_install git --build_from_source
    idempotent_brew_install openssl
    idempotent_brew_install jq
}

function install_bootstrap_tools {
    log_info "Installing bootstrapping tools..."
    if is_macos; then
        install_bootstrap_tools_macos
    else
        install_bootstrap_tools_debian
    fi
}

update_package_manager
install_bootstrap_tools

log_header2 "Finished installing bootstrapping tools.\n"
