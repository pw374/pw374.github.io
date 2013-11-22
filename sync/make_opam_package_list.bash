#!/bin/bash
	
while true ;
do 
    cd ~/opam-repository/packages && git pull
    for j in $(ls -1dt *)
    do
        if echo $j | grep -q '\.'
	then
	    continue
	fi
	i="$(cd "$j" && ls -1dt "$j"*|head -n 1)";
	v=$(echo "$i"|sed 's/[^.][^.]*\.//') ;
	p=$(echo "$i"|sed 's/\..*//g') ;
	d="$(git log -n 1 "$j/$i"|grep 'Date:'|head -n 1|sed 's/[^:]*: *//'|sed 's/\(201[234]\).*/\1/')" ;
	echo -e "$(date -d "$d" +%Y%m%d%H%M%S)\t<tr><td><a href='http://opam.ocaml.org/pkg/$p/$v/'>$p</a></td><td><a href='http://opam.ocaml.org/pkg/$p/$v/'>$v</a></td><td>$(date -d "$d" "+%d %b %Y")</td></tr>" ;
    done > .package-list-tmp;
    cp .package-list{,-old}; 
    cp .package-list{-tmp,};
    rm .package-list-{old,tmp}
    date
    echo "Sleeping now for 3600 seconds..."
    sleep 3600;
    date
    echo "Done sleeping."
done

