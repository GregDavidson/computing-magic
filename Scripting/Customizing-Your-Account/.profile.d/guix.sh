# * login time guix setup
# Sourced by ~/.local.bash

# ** The code for guix 1.3.0

# Maintenance Notes below!

export GUIX_PROFILE=~/.guix-profile
if [ -e "$GUIX_PROFILE" ]; then


# emacs_load_path_add () { pathvar_add EMACSLOADPATH --dots=no -zDV "$@"; }
# info_load_path_add () { pathvar_add INFOLOADPATH --dots=no -zDV "$@"; }
emacs_load_path_add () { EMACS_LOAD_PATH=$(path_list "$EMACS_LOAD_PATH" "$@"); export EMACS_LOAD_PATH ; }
info_load_path_add () { INFO_LOAD_PATH=$(path_list "$INFO_LOAD_PATH" "$@"); export INFO_LOAD_PATH ; }

# path_add "$GUIX_PROFILE/bin" ~/Bin.guix
path_add ~/Bin.guix


guix_emacs_version=$(emacs --batch --eval '(princ emacs-version)' --kill)
emacs_load_path_add "$GUIX_PROFILE/share/emacs/site-lisp" "$GUIX_PROFILE/share/emacs/$guix_emacs_version/lisp"
info_load_path_add "$GUIX_PROFILE/share/info"

fi
