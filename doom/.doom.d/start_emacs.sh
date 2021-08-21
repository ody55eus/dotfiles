#!/usr/bin/env bash
set -euo pipefail
EMACS=emacs
EMACSCLIENT='emacsclient -c' # or -nw for terminal mode
CMD=$($EMACSCLIENT || $EMACS)

$(CMD)
