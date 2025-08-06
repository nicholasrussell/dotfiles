#!/usr/bin/env bash

## had to stick these somewhere for now
function apt_tools {
	local dotfiles_apt_tools
	dotfiles_apt_tools=(
		cmake
		clangd-12
		libtool
		libtool-bin
		jq
		unzip
		fontconfig
		net-tools
		gnupg
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
		libvterm-dev
		xclip
		xdotool
		libmagickwand-dev
		libm17n-dev
		libwebkit2gtk-4.1
		# nvim
		neovim
		# Rust
		lld
		clang
		# Python
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
		# lisps
		guile-3.0
		guile-3.0-dev
		# misc
		htop
		ripgrep
	)

	# neovim
	sudo add-apt-repository -y ppa:neovim-ppa/unstable >/dev/null 2>&1

	sudo apt-get -qq update

	sudo apt-get -qq -y install "${dotfiles_apt_tools[@]}" >/dev/null 2>&1

	sudo apt-get build-dep emacs
}

if is_debian; then
	apt_tools
elif is_macos; then
	brew_install postgresql # TODO Update for 15?
	brew_install kubernetes-cli
	brew_install awscli
	brew_install watchman
	brew_install kustomize
fi
