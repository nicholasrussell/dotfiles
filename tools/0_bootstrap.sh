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

function apt_update_package {
    local pkg
    pkg="$1"
    sudo apt-get -qq install --only-upgrade "$package" > /dev/null 2>&1
    if [ $? -ne 0]; then
        log_error "Failed to upgrade apt package $pkg"
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

function installed_apt_packages {
    local -n arr=$1
    arr=($(dpkg --get-selections | grep -v deinstall | awk '{print $1}' | sed 's/:.*//'))
}

function install_apt_packages {
    local apt_packages apt_package installed
    apt_packages=("$@")
    #if [[ ! -v DOTFILES_TOOLS_FORCE ]]; then
        installed_apt_packages installed
        apt_packages=($(setdiff "${apt_packages[*]}" "${installed[*]}"))
    #fi
    for apt_package in "${apt_packages[@]}"; do
        log_info "Installing apt package $apt_package..."
        apt_install "$dotfiles_apt_tool"
        log_info "Finished installing apt package $apt_package."
    done
}

function install_bootstrap_tools_debian {
    local dotfiles_apt_tools
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
        gcc
        # Emacs
        libgccjit0
        libgccjit-10-dev
        texinfo
        libxpm-dev
        libjpeg-dev
        libgif-dev
        giflib-tools # instead of libungif-bin
        libtiff-dev
        libtree-sitter-dev
        libjansson4
        libjansson-dev
        libacl1-dev
        # Rust
        lld
        clang
        # below are for Python 3, move to tools?
        zlib1g-dev
        libbz2-dev
        libreadline-dev
        libsqlite3-dev
        libncursesw5-dev
        xz-utils
        tk-dev
        libxml2-dev
        libxmlsec1-dev
        libffi-dev
        liblzma-dev
    )
    install_apt_packages "${dotfiles_apt_tools[@]}"
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
