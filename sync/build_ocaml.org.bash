#!/bin/bash

if [[ -f /tmp/build_ocaml.org ]] && [[ "$(ps auwx|grep -v grep|grep $(cat /tmp/build_ocaml.org)|wc -l)" != "0" ]]
then
    echo "Exiting because another process is already doing this job." >&2
    exit 1
fi

echo 'echo $PPID'|bash > /tmp/build_ocaml.org

touch /tmp/build_ocaml.org
mv $HOME/stdout.log{,-old}
mv $HOME/stderr.log{,-old}

i=1

FLAG=$HOME/flag/force-build.flag

while true
do
    if (( i % 20 == 0 ))
    then
i=1
rm -fr ~/ocaml.org/ocaml.org/
    fi
    cd ~/ocaml.org/ || exit 1
    git pull
    make && rsync -r ocaml.org/* /var/www/
    # rm -f /tmp/code* # fix (until omd 0.6.2 gets into opam)
    (( i ++ ))
    for s in stdout stderr
    do
klm="$(du -s $HOME/$s.log|sed 's|\t.*||g')"
if (( klm > 3000 ))
then
    gzip -c $HOME/$s.log > $HOME/$s.log.gz
fi
    done
    date
    echo "Next build in 200 seconds, or maybe less."
    echo "If you're in a hurry, run this:"
    echo "touch ${FLAG}"
    for ((x=0;x<100;x++))
    do
sleep 2
if [[ -f "${FLAG}" ]]
then
    rm -f "${FLAG}"
    i=20
    break
fi
    done
    
done > $HOME/stdout.log 2> $HOME/stderr.log

