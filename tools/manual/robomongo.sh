#!/usr/bin/env bash

#log_header2 "Preparing to install Robomongo Idea..."
#
#robomongo_version="0.8.5"
#robomongo_binary_url="http://robomongo.org/files/linux/robomongo-${robomongo_version}-x86_64.deb"
#if wget -q --spider --timeout=30 ${robomongo_binary_url}; then
#    if [ ! -e /opt/robomongo ]; then
#        log_info "Downloading Robomongo ${robomongo_version}..."
#        wget -q -O /tmp/robomongo.deb ${robomongo_binary_url}
#        sudo dpkg -i /tmp/robomongo.deb
#        rm -f /tmp/robomongo.deb
#    else
#        log_info "Robomongo already installed"
#    fi
#    log_success "Finished installing Robomongo\n"
#else
#    log_error "Could not download Robomongo ${robomongo_version} because the binary does not exist on the remote server\n"
#fi
#unset robomongo_version robomongo_binary_url
