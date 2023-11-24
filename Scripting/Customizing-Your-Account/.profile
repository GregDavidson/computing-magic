#!/bin/sh
# ~/.profile - login preferences for sh compatible shells
# Directly Sourced by:
# - Many GUI display managers and session managers
#   - e.g. light-dm and mate-session
#   which use /bin/sh which is usually a link to
#   dash, bash or another Posix Shell
#   which emulate the Bourne Shell when called as sh
# - ~/.bash_profile when you login using bash

export HOME_PROFILE=begun  # protect against infinite sourcing loops!

# Many login systems are using /bin/sh instead of a user's
# chosen shell. /bin/sh is often a link to dash although it
# can be arranged to be any Posix-compatible shell such as
# Bash or zsh. This script will be called automatically and
# should only use Posix sh features. But if it is being
# called by a more powerful shell, it would be nice if it
# were to source the login scripts of that shell!

# ensure ~/.bash_profile sourced if we're really running bash
# this will be called near the end
# TODO: for mac folks, it would be good to generalize this to
# check if it's being run by zsh and if so, ensure that the
# suitable zsh login script is sourced!!!
ensure_bash_profile() {
    local this='.profile->ensure_bash_profile'
    local f="$HOME/.bash_profile"
    local notice="$this notice: %s\n"
    local proc_exe="/proc/$$/exe"
    [ -n "$BASH_VERSION" ] || {
        printf "$notice" "No variable BASH_VERSION"
        if [ -f "$proc_exe" ]; then
            printf "$notice" "this shell's $(realpath "$proc_exe") exiting"
        else
            printf "$notice" "this shell's not bash, exiting"
        fi
        return
    } >&2
    ! [ -v HOME_BASH_PROFILE ] || return # already sourced
    [ -f "$f" ] || {
        printf '%s warning: %s\n' "$this" "no script file %f"
        return
    } >&2
    . "$f"                               # source it now!
}

# We define some generic functions in this script
# then source some local scripts to customize things
# to the user's tastes

# Our account might "inherit" configurations from a "super" one
# - fancy extensions would go in the "super" account
# Therefore scripts might be under $super or under $HOME
sh_profiles_dir='.profile.d'

# Intended for use by if_src_super
# Try sourcing "$1"
# If it exists and we succeed, add it to $if_src_list
if_src_one() {
    local this='.profile->if_src_one'
    unset if_src_status
    [ -f "$1" -a -r "$1" ] || return 1 # signal failure silently
    . "$1"
    if_src_status=$?            # status from sourced script
    if_src_list="${if_src_list:+$if_src_list:}$1"
    return "$if_src_status"
}

# Intended for use by if_src
# Using if_src_one
#		Try sourcing $1 if it's an absolute path
#		otherwise try $super/$1 and $HOME/$1
# Succeed iff we source at least one
if_src_super() {
  case "$1" in
    /*) if_src_one "$1" ; return $? ;;
  esac
  unset if_src_home_status if_src_super_status
	! [ -d "$super" ] || if_src_one "$super/$1"
  if_src_super_status="$?"
  ! [ -f "$HOME/$1" ] || {
      if_src_one "$HOME/$1"
      if_src_home_status="$?"
  }
  return ${if_src_home_status:-${if_src_super_status:-1}}
}

# if_src SCRIPT...
# Try sourcing the specified scripts under $super and/or $HOME.
# Return 0 iff at least one file is successfully sourced.
# Report if no files are successfully sourced.
if_src() {
  if_src_list=''
  for f; do if_src_super "$f"; done
	[ -n "$if_src_list" ] && return 0
  >&2 echo "if_src: no file(s) $*" 
  return 1
}

# Determine our machine architecture, if possible
# because some things in Bin directories are
# binaries compiled for specific architectures!
if type -p arch >/dev/null      # do we have the arch program?
then arch=`arch`
else type -p uname >/dev/null || # or do we have the uname program?
arch=`uname -m` ||               # or hope it has the -m option!
arch='any'                       # or the default when no suffix
fi

# path_list [ -a ] [ -TEST ] EXISTING_PATH MAYBE_NEW_ITEM...
# Return a colon-separated list of items
# if they pass [ -TEST ] and are not already present.
# man test | grep '^ *-. ' | grep FILE | sort
path_list() {
    path_list_test='-n'        # bash: help test
    path_list_append='false'   # new items go in front
    path_list_delim=':'
    while [ $# -gt 0 ]; do
          case "$1" in
	            -a) path_list_append='true' ;; # new items go in back
	            -[bcdefgGhkLOprsSuwx]) path_list_test="$1" ;;
	            -?) >&2 echo "path_list warning: unknown option $1" ;;
	            *) break ;;
          esac
          shift
    done
    path_list_items="$1"; shift
# >&2 echo "path_list: Initial path_list_items=$path_list_items"
    for path_list_item; do
        ! [ "$path_list_test" "$path_list_item" ] ||
            case ":$path_list_items:" in
                *":$path_list_item:"*) ;; 
                *) if $path_list_append
                   then path_list_items="$path_list_items:$path_list_item"
                   else path_list_items="$path_list_item:$path_list_items"
                   fi ;;
            esac
    done
    printf "%s\n" "$path_list_items"
}

path_add () { PATH=$(path_list -d "$PATH" "$@"); export PATH ; }

# We store some of our collections of legacy software and some of the fancy
# software packages which we've built from source in their own directories which
# have subdirectories which need to be added to appropriate path variables
# before we can use them.
collection_add() {
    for ddd; do                 # e.g. /usr/local/SW for local software packages
        [ -d "$ddd" ] && for dd in "$ddd"/*; do # e.g. /usr/local/SW/pgsql
            [ -d "$dd" ] && for d in "$dd"/*; do # e.g. /usr/local/SW/pgsql/bin
                [ -d "$d" ] && case "$d" in
                    */[Bb]in|*/[Bb]in-"$arch") PATH=$(path_list "$PATH" "$d") ;;
                    */[Ii]nfo) INFOPATH=$(path_list "$INFOPATH" "$d") ;;
                    */site-lisp) EMACSLOADPATH=$(path_list "$EMACSLOADPATH" "$d") ;;
                    */Tcl) TCLLIBPATH=$(path_list "$TCLLIBPATH" "$d") ;;
                    */JVM) CLASSPATH=$(path_list -f "$CLASSPATH" $(find "$d/JVM" -name '*.jar' -print))
                    # Do you need to manage other kinds of paths?
                    # Run: env | cut -d= -f1 | grep PATH
                esac
            done
        done
    done
}

# Run any other user account profile scripts
if [ -n "$super" ]; then
    if_src "$sh_local" $super/$sh_profiles_dir/* $HOME/$sh_profiles_dir/*
else
    if_src "$sh_local" $HOME/$sh_profiles_dir/*
fi

# Construction notes:
# - Scripts can easily add new path components
#   - At the front or back -- DONE
#   - At an intermediate point -- TODO
#     - splicing in between the home and system directories
#     - splicing after a particular component
# - Check everything under ~/.home-inits -- TODO

# Put this where it belongs, uncommented!!
# collection_add ~/SW /usr/local/SW

ensure_bash_profile

export HOME_PROFILE=done
# kludge PATH
. $HOME/path-update
