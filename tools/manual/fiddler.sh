#!/usr/bin/env bash

#log_header2 "Preparing to install Fiddler..."
#
#fiddler_version="4.4.8.4"
#fiddler_binary_url="http://ericlawrence.com/dl/MonoFiddler-v${fiddler_version//.}.zip"
#if wget -q --spider --timeout=30 ${fiddler_binary_url}; then
#    fiddler_distributions=($(ls /opt | grep fiddler\-.* | sed "s/^.*fiddler-\([0-9.]*\).*/\1/"))
#    if (( ${#fiddler_distributions[@]} > 0 )); then
#        for fiddler_distribution in "${fiddler_distributions[@]}"; do
#            if [ "$fiddler_distribution" != "$fiddler_version" ]; then
#                log_info "Removing Fiddler ${fiddler_distribution}..."
#                sudo rm -r /opt/fiddler-${fiddler_distribution}
#            fi
#        done
#        unset fiddler_distribution
#    fi
#    unset fiddler_distributions
#
#    if [ ! -e /opt/fiddler-${fiddler_version} ]; then
#        log_info "Downloading Fiddler ${fiddler_version}..."
#        wget -q -O /tmp/fiddler-${fiddler_version}.zip ${fiddler_binary_url}
#        sudo 7z x -o/opt /tmp/fiddler-${fiddler_version}.zip > /dev/null
#        sudo mv /opt/app /opt/fiddler-${fiddler_version}
#        rm -f /tmp/fiddler-${fiddler_version}.zip
#    else
#        log_info "Fiddler ${fiddler_version} already installed"
#    fi
#    sudo ln -sf /opt/fiddler-${fiddler_version} /opt/fiddler
#
#    log_success "Finished installing Fiddler ${fiddler_version}\n"
#else
#    log_error "Could not download Fiddler ${fiddler_version} because the binary does not exist on the remote server\n"
#fi
#unset fiddler_version fiddler_binary_url
