#!/bin/bash

while true ;
do cd ~/opam-repository/ && git pull && cd packages && mv .package-list{,-old} ;
    for i in $(ls -1dt *) ;
    do v=$(echo "$i"|sed 's/[^.][^.]*\.//') ;
	p=$(echo "$i"|sed 's/\..*//g') ;
	d="$(git log -n 1 $i|grep 'Date:'|head -n 1|sed 's/[^:]*: *//'|sed 's/\(201[234]\).*/\1/')" ;
	if grep -q "$p" .package-list
	then
	    true
	else
	    echo -e "$(date -d "$d" +%Y%m%d%H%M%S)\t<tr><td><a href='http://opam.ocaml.org/pkg/$p/$v/'>$p</a></td><td><a href='http://opam.ocaml.org/pkg/$p/$v/'>$v</a></td><td>$(date -d "$d" "+%d %b %Y")</td></tr>" ;
	fi
    done > .package-list ;
    rm -f .package-list-old;
    echo "Sleeping now for 3600 seconds..."
    sleep 3600;
    echo "Done sleeping."
done

