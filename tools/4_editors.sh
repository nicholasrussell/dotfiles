#!/usr/bin/env bash

log_header2 "Installing editors..."

# Emacs
function install_emacs_debian {
	log_info "Installing Emacs..."
	if [ ! -d /opt/emacs ] || [[ -v DOTFILES_TOOLS_FORCE ]]; then
		# sudo apt build-dep emacs
		if [[ -v DOTFILES_TOOLS_FORCE ]]; then
			cargo install --force tree-sitter-cli
		else
			cargo install tree-sitter-cli
		fi
		sudo rm -rf /opt/emacs
		sudo git clone --depth 1 --branch emacs-29 https://git.savannah.gnu.org/git/emacs.git /opt/emacs
		sudo chown -R nrussell:nrussell /opt/emacs
		cd /opt/emacs
		# export CC="gcc-10"
		export CFLAGS="$CFLAGS -O2"
		./autogen.sh
		./configure --with-imagemagick --with-json --with-mailutils --with-native-compilation=aot --with-pgtk --with-tree-sitter --with-xwidgets
		make -j$(nproc)
		sudo make install
		cd -
		log_info "Finished installing Emacs."
	else
		log_info "Emacs already installed!"
	fi
}

function install_emacs_macos {
	# brew reinstall gcc libgccjit
	brew tap d12frosted/emacs-plus
	brew_install cask emacs-plus --with-imagemagick --with-mailutils --with-native-comp
	launchctl load -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist
}

function install_emacs {
	if is_macos; then
		install_emacs_macos
	else
		install_emacs_debian
	fi
}

install_emacs

# nvim
function install_nvim {
	log_info "Installing nvim..."
	if is_macos; then
		brew_install neovim
	else
		if ! command -v nvim >/dev/null 2>&1 || [[ -v DOTFILES_TOOLS_FORCE ]]; then
			sudo apt-get -qq -y install neovim >/dev/null 2>&1
			sudo update-alternatives --install /usr/bin/vi vi /usr/bin/nvim 110
			sudo update-alternatives --install /usr/bin/vim vim /usr/bin/nvim 110
			log_info "Finished installing nvim."
		else
			log_info "nvim already installed!"
		fi
	fi
	log_info "Finished configuring nvim."
}

install_nvim

# JetBrains
function install_jetbrains_toolbox {
	log_info "Installing JetBrains Toolbox..."
	if is_macos; then
		brew_install cask jetbrains-toolbox
	else
		log_warn "Implement for Debian."
	fi
	log_info "Finished installing JetBrains Toolbox."
}

install_jetbrains_toolbox

# VSCode
function install_vs_code {
	log_info "Installing Visual Studio Code..."
	if is_macos; then
		brew_install cask visual-studio-code
	else
		log_warn "Implement for Debian."
	fi
	log_info "Finished installing Visual Studio Code."
}

log_header2 "Finished installing editors.\n"
