#!/bin/sh
# This should only be run or sourced once!
# I like to run it in a text console
# Alternatively one might use systemd
# to provide an emacs server 
# Note: It *is* possible for an emacs script
# to add environment variables after starting!
emacs_server_in_tty() {
  DISPLAY=:0 emacs -nw --eval '(server-start)'
}

# https://www.srijn.net/running-multiple-emacs-daemons/
# Changing aliases into shell functions, etc.

run_emacs() {
  local this=run_emacs server="${1:-default}" tries=1
  [ $# -gt 0 ] && shift
  while ! emacsclient -s "$server" "$@" && [ $tries -lt 2 ]; do
    tries=`expr $tries + 1`
    >&2 echo "$this: Starting emacs server $server"
    emacs --daemon="$server"
  done
}

# Create a new frame in the default daemon
e() { run_emacs default -n -c "$@"; }

# Create a new terminal (TTY) frame in the default daemon
en() { run_emacs default -t "$@"; }

# Open a file to edit using sudo
es() { e "/sudo:root@localhost:$@"; }

# Open a new frame in the `mail` daemon, and start notmuch in the frame
em() { run_emacs mail -n -c -e '(notmuch-hello)' "$@"; }
