#/usr/bin/env bash
PATHS=$(jump --names)
# echo $PATHS
complete -W "--clear --bookmark --list $PATHS" jump
