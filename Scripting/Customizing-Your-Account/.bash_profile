# * where: ~/.bash_profile
#    what: bash login profile - sourced by login Bourne Again Shell
#    who: J. Greg Davidson
#   when: April 1996 - December 2021

# This is sourced automatically
# - if our login shell in /etc/passwd is bash
# - and we're logging in through a non-graphical terminal
# - which includes remote logins
#   - e.g. via ssh or emacs tramp
# Recent GUI Display Managers and Session Managers
# - seem to be running /bin/sh which is often a link to dash
# - sh sources $HOME/.profile or $HOME/.xprofile instead of this file
#   - have your .profile source this script
#     - if vice-versa, be sure and guard against infinite sourcing loops
#     - by using environment variables as shown here
#   - link your $HOME/.profile to $HOME/.xprofile
# - dash also prunes the environment, eliminating sourced bash functions!
#     - make sure /bin/sh is NOT a link to dash
#     - when called as sh bash will behave compatibly with Posix sh
#     - so link /bin/bash to /bin/sh instead of dash
#       - manually or by reconfiguring the dash package

export HOME_BASH_PROFILE=begun  # protect against infinite sourcing loops!

# ensure guard-variable-protected profile-script is sourced
# ensure_profile guard-variable script-path
ensure_profile() {
    local this='.bash_profile->ensure_profile'
    local -n v="$1"             # alias the guard variable
    local f="$2"                # the script file
    [ -f "$f" ] || {
        >&2 printf '%s warning: %s\n' "$this" "no script file %f"
        return
    }
    grep -qs "\<$1\>" "$f" || {
        >&2 printf '%s warning: %s\n' "$this" "no guard %1 in script %2"
        return
    }
    ! [ -v v ] || return        # already sourced
    . "$f"                      # source it now!
}
ensure_profile HOME_PROFILE ~/.profile

# ** Load the Simples System

: "${simples_bash:=$HOME/Lib/Shell/Simples-Bash/simples-export.bash}"
if ! [ -f "$simples_bash" ]; then 
    >&2 echo ".bash_profile no file $simples_bash"
else
    export simples_bash
    if ! . "$simples_bash"; then
        >&2 echo ".bash_profile failed to load $simples_bash"
    else
        simple_require --export paths
    fi
fi

# ** Source User-Supplied Scripts

home_bash_inits=( $HOME/.bash_profile.d/* )
[ ${#home_bash_inits[@]} -ne 1 ] || # no false matches possible
    [ "${home_bash_inits[0]##*/}" = '*' ] || # false match, or
    for f in "${home_bash_inits[@]}"; do     # for all matches
        . "$f" || >&2 echo ".bash_profile warning: error sourcing $f"
    done

# ** Interactive Shell Features

export HOME_BASH_PROFILE=done-noninteractive

# unless we're in a non-interactive shell, we're done
x="$?" # preserve any existing error code
[[ -t 0 ]] ||  [[ "$-" == *i* ]] ||
    { return "$x" 2> /dev/null || exit "$x"; }

# is this ancient s**t still meaningful?
stty erase '^?' kill '^u' intr '^c' quit '^\' susp '^z'

ensure_profile HOME_BASHRC ~/.bashrc

export HOME_BASH_PROFILE=done-interactive
