#!/bin/bash
# set -x

function CAT () {
    for i in "$@" ; do
        echo "# 0 \"$i\""
        cat "$i"
    done
}

rm -fr tags
mkdir tags
rm -fr _tmp
mkdir _tmp

echo > blogposts.contents.tmp.md.ml

for i in posts/*.md.ml.mpp ; do
    if [[ -f "$i" ]] ; then true ; else continue ; fi
    bn="_tmp/$(basename "$i" .md.ml.mpp)"
    export contents="$bn.main.html"
    export toc="$bn.toc.html"
    i="posts/$(basename "$i")"
    export self="posts/$(basename "$i" .md.ml.mpp).html"

    # .md.ml
    mpp -soc '' -scc '' -its -snl -l ocaml < "$i" > "$bn.md.ml"
    # .main.ml
    CAT common.ml "$bn.md.ml" ml_to_md.ml > "$bn.main.ml"

    ## add to blogposts.contents.tmp.md.ml
    CAT "$bn.main.ml" >> blogposts.contents.tmp.md.ml

    # .main.md
    ocaml "$bn.main.ml" > "$bn.main.md"
    # .main.html
    omd < "$bn.main.md" > "$bn.main.html"
    # .toc.html
    omd -otoc < "$bn.main.md" > "$toc"
    n=$(wc -l < "$toc")
    tail -n $((n-1)) "$toc" | head -n $((n-2)) > "$toc.tmp"
    mv "$toc"{.tmp,}
    # .tags.ml
    CAT common.ml "$bn.md.ml" tags.ml > "$bn.tags.ml"
    # create tags (symb links)
    ocaml "$bn.tags.ml"
    # .html.ml
    (CAT common.ml "$bn.md.ml" ; mpp -soc '' -scc '' -its -snl -l ocaml < _templates/main.mpp) > "$bn.html.ml"
    # .html
    ocaml "$bn.html.ml" > "$bn.html"
    mv "$bn.html" posts/
done

## make blogposts.contents.tmp.md
ocaml blogposts.contents.tmp.md.ml > blogposts.contents.tmp.md


    ## make a tag-specific page per tag
for x in tags/* ; do
    echo "x=$x <-----------"
    if [[ -d $x ]] ; then true ; else continue ; fi
    echo > "$x/index.ml"
    for j in "$x"/201*.html ; do
        if [[ -f "$j" ]] ; then true ; else continue ; fi
        CAT _tmp/"$(basename "$j" .html).main.ml" >> "$x"/index.ml
    done
    echo "ocaml $x/index.ml > $x/index.contents.md"
    ocaml "$x"/index.ml > "$x"/index.contents.md
    cat > "$x"/index.md.ml.mpp <<EOF
%% Copyright (C) 2013  Philippe Wang
{< module Meta : Meta = struct
 let title = "blog&lt;$(basename $x)"
 let id = "pw374.github.io--" ^ input_command "date +%Y-%m-%d-%H-%M-%S" ^ "--index"
 let xmldate = input_command "date --rfc-3339=seconds|tr ' ' T"
 let date = input_command "date --rfc-3339=seconds|tr ' ' T"
 let tags = [ "$(basename $x)"; ]
end
include Meta
>}

{< module Post(Unit:Unit) = struct >}
# {< let _ = !!title >}


{< let _ = cat "$x/index.contents.md" >}

{< end >}
EOF
done


    ## put title list and tag list on index.html
for bn in index projects blog tags/*/index; do
    export toc="$bn.toc.html"
    export contents="$bn.main.html"
    export self="$bn.html"
    # .md.ml
    mpp -soc '' -scc '' -its -snl -l ocaml < "$bn.md.ml.mpp" > "$bn.md.ml"
    # .main.ml
    CAT common.ml "$bn.md.ml" ml_to_md.ml > "$bn.main.ml"
    # .main.md
    ocaml "$bn.main.ml" > "$bn.main.md"
    # .main.html
    omd < "$bn.main.md" > "$bn.main.html"
    # .toc.html
    if [[ "$bn" == index ]]
    then
    for i in tags/* ; do
        if [[ "$(basename "$i")" != "root" ]] ;
        then
            echo "<li><a href='$i'>$(basename "$i")</a></li>"
        fi
    done > "$toc"
    else
    omd -otoc < "$bn.main.md" > "$toc"
    n=$(wc -l < "$toc")
    tail -n $((n-1)) "$toc" | head -n $((n-2)) > "$toc.tmp"
    mv "$toc"{.tmp,}
    fi
    # .html.ml
    (CAT common.ml "$bn.md.ml"  ; mpp -soc '' -scc '' -its -snl -l ocaml < _templates/main.mpp) > "$bn.html.ml"
    # .html
    ocaml "$bn.html.ml" > "$bn.html"
done


    # generate RSS feed
    ## general (all-tags)
    ## tag-feed
    
    # generate Atom feed
    ## general (all-tags)
    ## tag-feed
