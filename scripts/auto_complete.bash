#/usr/bin/env bash
BOOKMARKS=$(jump --names)
# echo $BOOKMARKS
complete -W "--clear --bookmark --list $BOOKMARKS" jump
