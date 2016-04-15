#!/usr/bin/env bash

if is_ubuntu; then
    exit 0
fi

# Brew
log_header2 "Installing brew..."
if which brew > /dev/null; then
    log_info "Brew already installed"
else
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew tap caskroom/versions
fi
log_success "Finished installing Brew\n"

# Git
log_header2 "Installing git..."
if which git > /dev/null; then
    log_info "Git already installed"
else
    brew install git --build-from-source
    brew cask install tcl
fi
if [ -n "${DOTFILES_GIT_USERNAME:+1}" ] && [ -n "${DOTFILES_GIT_EMAIL:+1}" ]; then
    # Replace .gitconfig values
    log_info "Setting .gitconfig user information..."
    sed -i '' "s/GIT_USERNAME/${DOTFILES_GIT_USERNAME}/g" ~/.gitconfig
    sed -i '' "s/GIT_EMAIL/${DOTFILES_GIT_EMAIL}/g" ~/.gitconfig
    log_info "Finished setting .gitconfig user information"
fi
log_success "Finished installing git\n"

# Bash completion
log_header2 "Installing bash completion..."
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    log_info "Bash completion already installed"
else
    brew install bash-completion
fi
log_success "Finished installing bash completion\n"

# Misc
log_header2 "Installing misc..."
brew install wget
brew install curl
brew install git-flow
brew install htop
brew install jq
log_success "Finished installing misc\n"

# Chrome
log_header2 "Installing Google Chrome..."
brew cask install google-chrome
log_success "Finished installing Google Chrome\n"

## JVM

# jenv
log_header2 "Installing jenv..."
brew install jenv
log_success "Finished install jenv\n"

# Java
log_header2 "Installing Java..."
brew cask install java
# jenv add /Library/Java/JavaVirtualMachines/jdk1.8.0_77.jdk/Contents/Home/
# jenv global oracle64-1.8.0.77
brew cask install java7
log_success "Finished installing Java\n"

log_header2 "Installing Leiningen..."
brew install leiningen
log_success "Finished installing Leiningen\n"

# Scala
log_header2 "Installing Scala..."
brew install scala
log_success "Finished installing Scala\n"

# sbt
log_header2 "Installing sbt..."
brew install sbt
log_success "Finished installing sbt\n"

# Activator
log_header2 "Installing Activator..."
brew install typesafe-activator
log_success "Finished installing Activator\n"

# Groovy
log_header2 "Installing Groovy..."
brew install groovy
log_success "Finished installing Groovy\n"

## JS

# Node
log_header2 "Installing NodeJS..."
brew install node

dotfiles_npm_modules=(
    bower
    jshint
    phantomjs
    less
    grunt
    grunt-cli
    yo
    gulp
    semver
    cordova
)
log_info "Installing npm modules..."
for dotfiles_npm_module in ${dotfiles_npm_modules[@]}; do
    if ! sudo npm list -g ${dotfiles_npm_module} > /dev/null 2>&1; then
        log_info "Installing npm module ${dotfiles_npm_module}..."
        sudo npm install -g ${dotfiles_npm_module} > /dev/null 2>&1
    else
        log_info "npm module ${dotfiles_npm_module} already installed"
    fi
done
unset dotfiles_npm_module dotfiles_npm_modules

log_success "Finished installing npm modules"
log_success "Finished installing NodeJS\n"

# SASS
log_header2 "Installing SASS..."
sudo gem install sass
log_success "Finished install SASS\n"

## Dev Tools

# Mongo
log_header2 "Installing MongoDB..."
brew install mongodb --with-openssl
log_success "Finished installing MongoDB\n"

# Sublime Text
log_header2 "Installing Sublime Text..."
brew cask install sublime-text3
log_success "Finished installing Sublime Text\n"

# Vim
log_header2 "Configuring Vim..."
if [ ! -e ~/.vim/autoload/pathogen.vim ]; then
    log_info "Installing pathogen..."
    mkdir -p ~/.vim/autoload ~/.vim/bundle && curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
fi
if [ ! -e ~/.vim/bundle/vim-airline ]; then
    log_info "Installing airline..."
    git clone -q https://github.com/bling/vim-airline ~/.vim/bundle/vim-airline
fi
if [ ! -e ~/.vim/bundle/nerdtree ]; then
    log_info "Installing NERDTree..."
    git clone -q https://github.com/scrooloose/nerdtree.git ~/.vim/bundle/nerdtree
fi
if [ ! -e ~/.vim/bundle/vim-sensible ]; then
    log_info "Installing sensible..."
    git clone -q git://github.com/tpope/vim-sensible.git ~/.vim/bundle/vim-sensible
fi
if [ ! -e ~/.vim/bundle/vim-fugitive ]; then
    log_info "Installing fugitive..."
    git clone -q git://github.com/tpope/vim-fugitive.git ~/.vim/bundle/vim-fugitive
fi
log_success "Finished configuring vim\n"

# Idea
log_header2 "Installing IntelliJ Idea..."
brew cask install intellij-idea-ce
log_success "Finished installing IntelliJ Idea\n"

# SourceTree
log_header2 "Installing SourceTree..."
brew cask install sourcetree
log_success "Finished installing SourceTree\n"

# Virtualbox
log_header2 "Installing Virtualbox..."
brew cask install virtualbox
log_success "Finished installing Virtualbox\n"

# Docker
log_header2 "Installing Docker..."
brew cask install docker
brew install docker-machine
brew install docker-compose
brew cask install kitematic
log_success "Finished installing Docker\n"

# Vagrant
log_header2 "Installing Vagrant..."
brew cask install vagrant
brew cask install vagrant-manager
log_success "Finished installing Vagrant\n"

# Gradle
log_header2 "Installing Gradle..."
brew install gradle
log_success "Finished installing Gradle\n"

# pip
log_header2 "Installing pip..."
if ! which pip > /dev/null; then
    sudo easy_install pip
else
    log_info "pip already installed"
fi
log_success "Finished installing pip\n"

# Ansible
log_header2 "Installing Ansible..."
brew install ansible
log_success "Finished installing Ansible\n"

# Android
log_header2 "Installing Android SDK..."
brew install android-sdk
log_success "Finished installing Android SDK\n"

# Slack
log_header2 "Installing Slack..."
brew cask install slack
log_success "Finished installing Slack\n"