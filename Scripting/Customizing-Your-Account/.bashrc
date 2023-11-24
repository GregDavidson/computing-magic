#!/bin/bash
# where: $HOME/.bashrc
#  what: Bourne Again SHell customization script
#   who: J. Greg Davidson
#  when: February 1995
#  revised to use my simples package: April 2008

HOME_BASHRC=begun

# ** Debugging Notions
# set -xv
# set -xve
# trap 'echo Error $? on line $LINENO; trap - ERR; set +xve; return $? 2>/dev/null || exit $?' ERR
# echo "$PATH" | tr ':' '\n' | cat -n
# echo "$PATH" | /usr/bin/tr ':' '\n' | /bin/cat -n

# too fancy, commented out!
# ** Ensure login profile was sourced
# f=~/.bashrc
# ff=`realpath "$f"`
# type -p simple_src && ! simple_src --get "$f" &&
#     { return 2>/dev/null || exit; }

# ensure ~/.bash_profile sourced
ensure_bash_profile() {
    local f="$HOME/.bash_profile"
    ! [ -v HOME_BASH_PROFILE ] || return # already sourced
    [ -f "$f" ] || return                # doesn't exist
    . "$f"                               # source it now!
}
ensure_bash_profile

# ** Bring in features for an Interactive Shell

HOME_BASHRC=done-noninteractive

# return if we're in a non-interactive shell
x="$?" # preserve any existing error code
[[ -t 0 ]] ||  [[ "$-" == *i* ]] ||
    { return "$x" 2> /dev/null || exit "$x"; }

HOME_BASHRC=begun-interactive

# this brings in way too many things!!
simple_require interactive

# maybe nicer to bring in selected bundles of nice features
simple_src_dir ~/.bashrc.d

# ** Show we're complete

simple_src --set ~/.bashrc

HOME_BASHRC=done-interactive

# ** Debugging Notions Cleanup
# set +xv
# set +xve
# trap - ERR
