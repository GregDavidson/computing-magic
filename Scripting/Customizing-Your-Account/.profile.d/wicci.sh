#!/bin/sh
# source this file to setup paths for hacking on the wicci

WICCI_TOOLS_BIN=~/Projects/Wicci/Tools/Bin
[ -d $WICCI_TOOLS_BIN ] || {
    >&2 echo "wicci.bash: No directory $WICCI_TOOLS_BIN"
    return 1
}

path_add -a $WICCI_TOOLS_BIN 

eval `wicci-paths + simples`
