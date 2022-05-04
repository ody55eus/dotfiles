#!/usr/bin/env bash
set -euo pipefail

EMACS="/opt/jp/bin/emacs"
EMACSCLIENT="/opt/jp/bin/emacsclient -c" # or -nw for terminal mode
CMD=$($EMACSCLIENT || $EMACS)

$(CMD)
