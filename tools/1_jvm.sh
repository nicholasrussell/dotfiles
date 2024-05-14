#!/usr/bin/env bash

log_header2 "Installing JVM tools..."

function install_jenv_debian {
	log_info "Installing jenv..."
	if [ ! -d ~/.jenv ]; then
		git clone https://github.com/jenv/jenv.git ~/.jenv
		log_info "Finished installing jenv."
	elif [[ -v DOTFILES_TOOLS_FORCE ]]; then
		cd ~/.jenv && git pull -q && cd - >/dev/null
		log_info "Finished updating jenv."
	else
		log_info "jenv is already installed!"
	fi
}

function install_jenv {
	if is_macos; then
		brew_install jenv
	else
		install_jenv_debian
	fi
}

function install_jdk_macos {
	# TODO FIXME
	version="$1"
	#if [ ! -e "/Library/Java/JavaVirtualMachines/ibm-semeru-open-${version}.jdk/" ]; then
	#    latest=$(curl -s -L -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" "https://api.github.com/repos/ibmruntimes/semeru${version}-binaries/releases" | jq -c '.[0].assets | map(select(.name | contains("aarch64_mac"))) | map(select(.name | endswith(".pkg"))) | .[0]')
	#    file_name=$(echo "$latest" | jq '.name')
	#    download_url=$(echo "$latest" | jq '.browser_download_url')
	#    wget -q "$download_url" | sudo installer -pkg -target /
	#    sudo installer -pkg "$file_name" -target /
	#    rm "$file_name"
	#fi
	if [ ! -e "/Library/Java/JavaVirtualMachines/openjdk-${version}/" ]; then
		latest=$(curl -s -L -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" "https://api.github.com/repos/adoptium/temurin${version}-binaries/releases" | jq -c '.[0].assets | map(select(.name | contains("jdk_aarch64_mac_hotspot"))) | map(select(.name | endswith(".pkg"))) | .[0]')
		file_name=$(echo "$latest" | jq '.name')
		download_url=$(echo "$latest" | jq '.browser_download_url')
		wget -q "$download_url" | sudo installer -pkg -target /
		sudo installer -pkg "$file_name" -target /
		rm "$file_name"
	fi
}

function install_jdks_macos {
	install_jdk_macos 11
	install_jdk_macos 17
	install_jdk_macos 21
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
		install_jdks_macos
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
			jenv add /usr/lib/jvm/jdk-21+35/ >/dev/null
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
		brew_install leiningen
	else
		install_lein_debian
	fi
}

function install_clojure_lsp {
	log_info "Installing Clojure LSP..."
	if is_macos; then
		brew_install clojure-lsp/brew/clojure-lsp-native
	else
		if ! command -v clojure-lsp >/dev/null 2>&1 || [[ -v DOTFILES_TOOLS_FORCE ]]; then
			curl -s https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install >clojure-lsp-install
			sudo bash clojure-lsp-install
			rm clojure-lsp-install
			log_info "Finished installing Clojure LSP."
		else
			log_info "Clojure LSP is already installed!"
		fi
	fi
}

install_jenv
install_jdk
configure_jenv
install_lein
install_clojure_lsp

log_header2 "Finished installing JVM tools.\n"
