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
	brew_install cask emacs
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
		brew_install install neovim
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

# IntelliJ Idea
# TODO

# VSCode
# TODO

log_header2 "Finished installing editors.\n"
