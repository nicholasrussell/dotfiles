#!/usr/bin/env bash

# rbenv
log_header2 "Preparing to install Ruby..."

if [ ! -e ~/.rbenv/bin ]; then
    log_info "Installing rbenv..."
    if [ -e ~/.rbenv ]; then
        rm -rf ~/.rbenv
    fi
    git clone -q https://github.com/sstephenson/rbenv.git ~/.rbenv
    if [ -e $DOTFILES/link/.rbenv/default-gems ]; then
        ln -sf $DOTFILES/link/.rbenv/default-gems ~/.rbenv/default-gems
    fi
fi

if [ ! -e ~/.rbenv/plugins ]; then mkdir -p ~/.rbenv/plugins; fi

if [ ! -e ~/.rbenv/plugins/ruby-build ]; then
    log_info "Installing ruby-build rbenv plugin..."
    git clone -q https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
fi

if [ ! -e ~/.rbenv/plugins/rbenv-default-gems ]; then
    log_info "Installing rbenv-default-gems rbenv plugin..."
    git clone -q https://github.com/sstephenson/rbenv-default-gems.git ~/.rbenv/plugins/rbenv-default-gems
fi

if [ ! -e ~/.rbenv/plugins/rbenv-gem-rehash ]; then
    log_info "Installing rbenv-gem-rehash rbenv plugin..."
    git clone -q https://github.com/sstephenson/rbenv-gem-rehash.git ~/.rbenv/plugins/rbenv-gem-rehash
fi

ruby_version="2.2.2"
if [ "$(~/.rbenv/bin/rbenv global)" == "$ruby_version" ]; then
    log_info "Ruby ${ruby_version} already installed"
else
    log_info "Installing Ruby ${ruby_version}..."
    ~/.rbenv/bin/rbenv install -s $ruby_version
    ~/.rbenv/bin/rbenv global $ruby_version
fi
log_success "Finished installing Ruby ${ruby_version}\n"
unset ruby_version
