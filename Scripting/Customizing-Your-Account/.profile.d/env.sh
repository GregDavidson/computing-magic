#!/bin/sh
# where: $HOME/.sh.d/env.sh
#  what: Miscellaneous environment settings for sh-compatible shells
#   who: J. Greg Davidson
#  pulled from: a larger file dated February 1995

HOSTNAME=${HOSTNAME:-`hostname`} ; export HOSTNAME

# PAGER, MANPAGER, EDITOR, LS_OPTIONS now set in .bashrc
# so that they can customize based on whether we're
# in a terminal or inside of an emacs shell buffer.

# VERSION_CONTROL controls backup style used by GNU mv,cp,ln commands
VERSION_CONTROL=numbered
mv_cp_opt=-b                    # for use with aliases in .bashrc
export VERSION_CONTROL mv_cp_opt
