#!/bin/bash

function remove_from_name () {
    if (( $# != 2 ))
    then
        echo "Usage: $0 word-to-remove filename"
        false
    else
        local target="$(sed "s/$1//g"<<<"$2")"
        if [[ -f "$target" ]]
        then
            false
            else
            mv "$2" "$target"
        fi
    fi
}

