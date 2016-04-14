#!/usr/bin/env bash

## Terminal
log_header1 "Configuring terminal..."

if is_ubuntu; then
    # Global
    gconftool-2 --set /apps/gnome-terminal/global/active_encodings --type list --list-type string "[UTF-8,current]"
    gconftool-2 --set /apps/gnome-terminal/global/confirm_window_close --type bool 1
    #gconftool-2 --set /apps/gnome-terminal/global/default_profile --type string "Default"
    #gconftool-2 --set /apps/gnome-terminal/global/profile_list --type list --list-type string "[Default]"
    gconftool-2 --set /apps/gnome-terminal/global/use_menu_accelerators --type bool 1
    gconftool-2 --set /apps/gnome-terminal/global/use_mnemonics --type bool 1

    # Keybindings
    # /apps/gnome-terminal/keybindings/close_tab "<Ctrl><Shift>w"
    # /apps/gnome-terminal/keybindings/close_window "<Ctrl><Shift>q"
    # /apps/gnome-terminal/keybindings/copy "<Ctrl><Shift>c"
    # /apps/gnome-terminal/keybindings/detach_tab
    # /apps/gnome-terminal/keybindings/full_screen "F11"
    # /apps/gnome-terminal/keybindings/help "F1"
    # /apps/gnome-terminal/keybindings/move_tab_left "<Ctrl><Shift>Page_Up"
    # /apps/gnome-terminal/keybindings/move_tab_right "<Ctrl><Shift>Page_Down"
    # /apps/gnome-terminal/keybindings/new_profile
    # /apps/gnome-terminal/keybindings/new_tab "<Ctrl><Shift>t"
    # /apps/gnome-terminal/keybindings/new_window "<Ctrl><Shift>n"
    # /apps/gnome-terminal/keybindings/next_tab "<Primary>Page_Down"
    # /apps/gnome-terminal/keybindings/paste "<Ctrl><Shift>v"
    # /apps/gnome-terminal/keybindings/prev_tab "<Primary>Page_Up"
    # /apps/gnome-terminal/keybindings/reset
    # /apps/gnome-terminal/keybindings/reset_and_clear
    # /apps/gnome-terminal/keybindings/save_contents disabled
    # /apps/gnome-terminal/keybindings/set_terminal_title
    # /apps/gnome-terminal/keybindings/switch_to_tab_1 "<Alt>1"
    # /apps/gnome-terminal/keybindings/switch_to_tab_10
    # /apps/gnome-terminal/keybindings/switch_to_tab_11
    # /apps/gnome-terminal/keybindings/switch_to_tab_12
    # /apps/gnome-terminal/keybindings/switch_to_tab_2 "<Alt>2"
    # /apps/gnome-terminal/keybindings/switch_to_tab_3 "<Alt>3"
    # /apps/gnome-terminal/keybindings/switch_to_tab_4 "<Alt>4"
    # /apps/gnome-terminal/keybindings/switch_to_tab_5 "<Alt>5"
    # /apps/gnome-terminal/keybindings/switch_to_tab_6 "<Alt>6"
    # /apps/gnome-terminal/keybindings/switch_to_tab_7 "<Alt>7"
    # /apps/gnome-terminal/keybindings/switch_to_tab_8 "<Alt>8"
    # /apps/gnome-terminal/keybindings/switch_to_tab_9 "<Alt>9"
    # /apps/gnome-terminal/keybindings/toggle_menubar
    # /apps/gnome-terminal/keybindings/zoom_in "<Ctrl>plus"
    # /apps/gnome-terminal/keybindings/zoom_normal "<Ctrl>0"
    # /apps/gnome-terminal/keybindings/zoom_out "<Ctrl>minus"

    # Profiles/Default
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/allow_bold --type bool 1
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/alternate_screen_scroll --type bool 1
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/background_color --type string "#000000000000"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/background_darkness --type float 0.5
    # /apps/gnome-terminal/profiles/Default/background_image
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/background_type --type string "solid"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/backspace_binding --type string "ascii-del"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/bold_color --type string "#000000000000"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/bold_color_same_as_fg --type bool 1
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/cursor_blink_mode --type string "system"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/cursor_shape --type string "block"
    # /apps/gnome-terminal/profiles/Default/custom_command
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/default_show_menubar --type bool 1
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/default_size_columns --type int 80
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/default_size_rows --type int 24
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/delete_binding --type string "escape-sequence"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/encoding --type string "current"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/exit_action --type string "close"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/font --type string "Monospace 12"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/foreground_color --type string "#FFFFFFFFFFFF"
    # /apps/gnome-terminal/profiles/Default/icon
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/login_shell --type bool 0
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/palette --type string "#000000000000:#CCCC00000000:#4E4E9A9A0606:#C4C4A0A00000:#34346565A4A4:#757550507B7B:#060698209A9A:#D3D3D7D7CFCF:#555557575353:#EFEF29292929:#8A8AE2E23434:#FCFCE9E94F4F:#72729F9FCFCF:#ADAD7F7FA8A8:#3434E2E2E2E2:#EEEEEEEEECEC"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/scroll_background --type bool 1
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/scrollback_lines --type int 512
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/scrollback_unlimited --type bool 1
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/scrollbar_position --type string "right"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/scroll_on_keystroke --type bool 1
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/scroll_on_output --type bool 0
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/silent_bell --type bool 1
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/title --type string "Terminal"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/title_mode --type string "replace"
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/update_records --type bool 1
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/use_custom_command --type bool 0
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/use_custom_default_size --type bool 0
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/use_system_font --type bool 1
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/use_theme_colors --type bool 0
    gconftool-2 --set /apps/gnome-terminal/profiles/Default/visible_name --type string "Default"
    # gconftool-2 --set /apps/gnome-terminal/profiles/Default/word_chars --type string "-A-Za-z0-9,./?%&#:_=+@~"
fi

log_success "Finished configuring terminal\n"
