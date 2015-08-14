#!/usr/bin/env bash

log_header1 "Misc configuration..."

# Remove annoying flashplugin-installer pop up if it exists
if [ -e /var/lib/update-notifier/user.d/data-downloads-failed ]; then
    log_warn "Backing up /var/lib/update-notifier/user.d/data-downloads-failed to $DOTFILES/backup/misc..."
    mkdir -p $DOTFILES/backup/misc
    cp /var/lib/update-notifier/user.d/data-downloads-failed $DOTFILES/backup/misc
    log_info "Deleting /var/lib/update-notifier/user.d/data-downloads-failed..."
    sudo rm -f /var/lib/update-notifier/user.d/data-downloads-failed
fi
if [ -e /var/lib/update-notifier/user.d/data-downloads-failed-permanently ]; then
    log_warn "Backing up /var/lib/update-notifier/user.d/data-downloads-failed-permanently to $DOTFILES/backup/misc..."
    mkdir -p $DOTFILES/backup/misc
    cp /var/lib/update-notifier/user.d/data-downloads-failed-permanently $DOTFILES/backup/misc
    log_info "Deleting /var/lib/update-notifier/user.d/data-downloads-failed-permanently..."
    sudo rm -f /var/lib/update-notifier/user.d/data-downloads-failed-permanently
fi

# gconftool-2 --set /apps/compiz1/gnomecompat/screen0/options/run_command_terminal_key --type string "<Control><Alt>T"
# gconftool-2 --set /apps/compiz1/gnomecompat/screen0/options/run_key --type string "<Alt>F2"

# gconftool-2 --set /desktop/gnome/applications/browser/exec --type string "/opt/google/chrome/google-chrome"
# gconftool-2 --set /desktop/gnome/applications/terminal/exec --type string "gnome-terminal"
# gconftool-2 --set /desktop/gnome/applications/terminal/exec_arg --type string "-x"

timedatectl set-timezone America/Chicago

# These don't work
# gconftool-2 --set /com/canonical/indicator/datetime/custom-time-format --type string "%d"
# gconftool-2 --set /com/canonical/indicator/datetime/locations --type list --list-type string "[UTC]"
# gconftool-2 --set /com/canonical/indicator/datetime/show-calendar --type bool 0
# gconftool-2 --set /com/canonical/indicator/datetime/show-clock --type bool 0
# gconftool-2 --set /com/canonical/indicator/datetime/show-date --type bool 0
# gconftool-2 --set /com/canonical/indicator/datetime/show-day --type bool 0
# gconftool-2 --set /com/canonical/indicator/datetime/show-events --type bool 0
# gconftool-2 --set /com/canonical/indicator/datetime/show-locations --type bool 0
# gconftool-2 --set /com/canonical/indicator/datetime/show-seconds --type bool 0
# gconftool-2 --set /com/canonical/indicator/datetime/show-week-numbers --type bool 0
# gconftool-2 --set /com/canonical/indicator/datetime/time-format --type string "custom"
# gconftool-2 --set /com/canonical/indicator/datetime/timezone-name --type string "America/Chicago"

# GSettings
# org.gnome.desktop.wm.preferences application-based false
# org.gnome.desktop.wm.preferences disable-workarounds false
# org.gnome.desktop.wm.preferences action-right-click-titlebar 'menu'
# org.gnome.desktop.wm.preferences mouse-button-modifier '<Alt>'
# org.gnome.desktop.wm.preferences focus-new-windows 'smart'
# org.gnome.desktop.wm.preferences action-middle-click-titlebar 'lower'
# org.gnome.desktop.wm.preferences titlebar-uses-system-font false
# org.gnome.desktop.wm.preferences theme 'Ambiance'
# org.gnome.desktop.wm.preferences num-workspaces 4
# org.gnome.desktop.wm.preferences raise-on-click true
# org.gnome.desktop.wm.preferences auto-raise false
# org.gnome.desktop.wm.preferences visual-bell false
# org.gnome.desktop.wm.preferences focus-mode 'click'
# org.gnome.desktop.wm.preferences action-double-click-titlebar 'toggle-maximize'
# org.gnome.desktop.wm.preferences auto-raise-delay 500
# org.gnome.desktop.wm.preferences button-layout 'close,minimize,maximize:'
# org.gnome.desktop.wm.preferences workspace-names @as []
# org.gnome.desktop.wm.preferences audible-bell true
# org.gnome.desktop.wm.preferences resize-with-right-button false
# org.gnome.desktop.wm.preferences titlebar-font 'Ubuntu Bold 11'
# org.gnome.desktop.wm.preferences visual-bell-type 'fullscreen-flash'
# org.gnome.desktop.interface menus-have-icons false
# org.gnome.desktop.interface cursor-blink-timeout 10
# org.gnome.desktop.interface gtk-color-palette 'black:white:gray50:red:purple:blue:light blue:green:yellow:orange:lavender:brown:goldenrod4:dodger blue:pink:light green:gray10:gray30:gray75:gray90'
# org.gnome.desktop.interface gtk-timeout-repeat 20
# org.gnome.desktop.interface automatic-mnemonics true
# org.gnome.desktop.interface toolkit-accessibility false
# org.gnome.desktop.interface ubuntu-overlay-scrollbars true
# org.gnome.desktop.interface cursor-theme 'DMZ-White'
# org.gnome.desktop.interface show-input-method-menu true
# org.gnome.desktop.interface clock-show-seconds false
# org.gnome.desktop.interface can-change-accels false
# org.gnome.desktop.interface cursor-blink-time 1200
# org.gnome.desktop.interface toolbar-detachable false
# org.gnome.desktop.interface enable-animations true
# org.gnome.desktop.interface menubar-detachable false
# org.gnome.desktop.interface toolbar-style 'both-horiz'
# org.gnome.desktop.interface toolbar-icons-size 'large'
# org.gnome.desktop.interface gtk-im-module 'gtk-im-context-simple'
# org.gnome.desktop.interface icon-theme 'ubuntu-mono-dark'
# org.gnome.desktop.interface gtk-timeout-initial 200
# org.gnome.desktop.interface scaling-factor uint32 1
# org.gnome.desktop.interface clock-format '24h'
# org.gnome.desktop.interface buttons-have-icons false
# org.gnome.desktop.interface monospace-font-name 'Ubuntu Mono 13'
# org.gnome.desktop.interface text-scaling-factor 1.0
# org.gnome.desktop.interface menus-have-tearoff false
# org.gnome.desktop.interface gtk-im-preedit-style 'callback'
# org.gnome.desktop.interface show-unicode-menu true
# org.gnome.desktop.interface cursor-blink true
# org.gnome.desktop.interface gtk-theme 'Ambiance'
# org.gnome.desktop.interface gtk-key-theme 'Default'
# org.gnome.desktop.interface gtk-im-status-style 'callback'
# org.gnome.desktop.interface menubar-accel 'F10'
# org.gnome.desktop.interface cursor-size 24
# org.gnome.desktop.interface font-name 'Ubuntu 11'
# org.gnome.desktop.interface clock-show-date false
# org.gnome.desktop.interface document-font-name 'Sans 11'
# org.gnome.desktop.interface gtk-color-scheme ''
# org.gnome.desktop.screensaver picture-opacity 100
# org.gnome.desktop.screensaver logout-enabled false
# org.gnome.desktop.screensaver lock-enabled true
# org.gnome.desktop.screensaver logout-delay uint32 7200
# org.gnome.desktop.screensaver embedded-keyboard-enabled false
# org.gnome.desktop.screensaver primary-color '#023c88'
# org.gnome.desktop.screensaver idle-activation-enabled true
# org.gnome.desktop.screensaver secondary-color '#5789ca'
# org.gnome.desktop.screensaver logout-command ''
# org.gnome.desktop.screensaver color-shading-type 'solid'
# org.gnome.desktop.screensaver embedded-keyboard-command ''
# org.gnome.desktop.screensaver show-notifications false
# org.gnome.desktop.screensaver picture-options 'zoom'
# org.gnome.desktop.screensaver lock-delay uint32 0
# org.gnome.desktop.screensaver show-full-name-in-top-bar true
# org.gnome.desktop.screensaver picture-uri 'file:///usr/share/backgrounds/gnome/Locked.jpg'
# org.gnome.desktop.screensaver status-message-enabled true
# org.gnome.desktop.screensaver ubuntu-lock-on-suspend true
# org.gnome.desktop.screensaver user-switch-enabled true
# com.canonical.indicator.datetime locations ['UTC']
# com.canonical.indicator.datetime show-day true
# com.canonical.indicator.datetime timezone-name ''
# com.canonical.indicator.datetime show-seconds true
# com.canonical.indicator.datetime show-date true
# com.canonical.indicator.datetime time-format '24-hour'
# com.canonical.indicator.datetime show-week-numbers false
# com.canonical.indicator.datetime show-locations false
# com.canonical.indicator.datetime show-year false
# com.canonical.indicator.datetime show-calendar true
# com.canonical.indicator.datetime custom-time-format '%l:%M %p'
# com.canonical.indicator.datetime show-clock true
# com.canonical.indicator.datetime show-events true
# com.canonical.indicator.datetime show-auto-detected-location false

# Set wallpaper
if [ -e $DOTFILES/conf/images/wallpaper.jpg ]; then
    log_info "Setting wallpaper..."
    if [ ! -e "~/Pictures/wallpaper.jpg" ]; then
        if [ ! -d "~/Pictures" ]; then
            mkdir -p ~/Pictures
        fi
        cp $DOTFILES/conf/images/wallpaper.jpg ~/Pictures/wallpaper.jpg
    fi
    gsettings set org.gnome.desktop.background picture-opacity 100
    gsettings set org.gnome.desktop.background secondary-color '#000000'
    gsettings set org.gnome.desktop.background show-desktop-icons true
    gsettings set org.gnome.desktop.background primary-color '#000000'
    gsettings set org.gnome.desktop.background color-shading-type 'solid'
    gsettings set org.gnome.desktop.background picture-options 'zoom'
    gsettings set org.gnome.desktop.background picture-uri "file://${HOME}/Pictures/wallpaper.jpg"
    gsettings set org.gnome.desktop.background draw-background true
fi

log_success "Finished configuration\n"
