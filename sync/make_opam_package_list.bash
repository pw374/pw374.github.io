#!/bin/bash


while true ; do mv .package-list{,-old} ; cd ~/opam-repository/ && git pull && cd packages && for i in * ; do v=$(echo "$i"|sed 's/[^.][^.]*\.//') ; p=$(echo "$i"|sed 's/\..*//g') ; d="$(git log $i|grep Date|head -n 1|sed 's/[^:]*: *//'|sed 's/\(201[234]\).*/\1/')" ; echo -e "$(date -d "$d" +%Y%m%d%H%M%S)\t<tr><td><a href='http://opam.ocaml.org/pkg/$p/'>$p</a></td><td><a href='http://opam.ocaml.org/pkg/$p/$v/'>$v</a></td><td>$(date -d "$d" "+%d %b %Y")</td></tr>" ; done > .package-list ; rm -f .package-list-old; sleep 3600; done

