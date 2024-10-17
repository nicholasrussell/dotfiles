#!/usr/bin/env bash

log_header2 "Picking self up by bootstraps..."

function macos_manual {
	export HOMEBREW_PREFIX=/opt/homebrew

	# Run this manually first

	# xcode-select --install

	## Install Homebrew
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

	## Install git
	brew install git --build_from_source

	## Install bash
	brew install bash
	echo "${HOMEBREW_PREFIX}/bin/bash" | sudo tee -a /etc/shells
	chsh -s "${HOMEBREW_PREFIX}/bin/bash"
	brew unlink md5sha1sum
}

function is_brew_formula_installed {
	brew list "$1" >/dev/null 2>&1
}

function brew_install {
	local formula="$1"
	if [ "$1" == "cask" ]; then
		formula="$2"
	fi
	if ! is_brew_formula_installed "$formula"; then
		log_info "Installing Brew formula $formula..."
		if [ "$1" == "cask" ]; then
			local temp_args=("$@")
			brew install --cask "${temp_args[@]:1}"
		else
			brew install "$@"
		fi
		log_info "Finished installing $formula."
	else
		log_info "Brew formula $formula is already installed!"
	fi
}

function install_homebrew {
	if [ ! -d "${HOMEBREW_PREFIX}" ]; then
		log_info "Installing Homebrew..."
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
		log_info "Finished installing Homebrew."
	fi
}

function bootstrap {
	if is_macos; then
		install_homebrew
		brew update >/dev/null 2>&1
		brew_install coreutils
		brew_install gcc
		brew_install libgccjit
		brew_install openssl
		brew_install fd
		brew_install cmake
		brew_install git --build_from_source
		brew_install libvterm
		brew_install wget
		brew_install curl
		brew_install jq
		brew_install tree-sitter
	elif is_debian; then
		sudo apt-get -qq update
		local tools
		tools=(
			apt-transport-https
			wget
			curl
			git
			build-essential
			software-properties-common
			libssl-dev
			gcc
			procps
			file
		)
		sudo apt-get -qq -y install "${tools[@]}" >/dev/null 2>&1
		install_homebrew
		brew update >/dev/null 2>&1
	fi
	if ! command -v bb >/dev/null 2>&1 || [[ -v DOTFILES_TOOLS_FORCE ]]; then
		curl -s https://raw.githubusercontent.com/babashka/babashka/master/install >bb-install
		sudo bash bb-install
		rm bb-install
	fi
}

bootstrap

log_header2 "Picked self up by bootstraps.\n"
