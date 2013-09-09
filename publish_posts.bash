#!/bin/bash

if [[ "$DEBUG" != "" ]]
then
set -x
unset DEBUG
fi

function OMD () {
    omd -r ocaml=ocamltohtml
}

function CAT () {
    for i in "$@" ; do
        echo "# 0 \"$i\""
        cat "$i"
    done
}

function MPPOCAML () {
    mpp -soc '' -scc '' -its -snl -l ocaml
}


rm -fr tags
mkdir tags
rm -fr _tmp
mkdir _tmp

echo > blogposts.contents.tmp.md.ml
echo > blogposts.contents-atom.xml

cat common.ml > blog-atom-body.ml
cat common.ml > blog-rss-body.ml

for i in posts/*.md.ml.mpp ; do
    if [[ -f "$i" ]] ; then true ; else continue ; fi
    bn="_tmp/$(basename "$i" .md.ml.mpp)"
    export contents="$bn.main.html"
    export toc="$bn.toc.html"
    i="posts/$(basename "$i")"
    export self="posts/$(basename "$i" .md.ml.mpp).html"
    export selfbn="posts/$(basename "$i" .md.ml.mpp)"

    # .md.ml
    MPPOCAML < "$i" > "$bn.md.ml"
    # .main.ml
    CAT common.ml "$bn.md.ml" ml_to_md.ml > "$bn.main.ml"

    ## add to blogposts.contents.tmp.md.ml
    CAT "$bn.main.ml" >> blogposts.contents.tmp.md.ml

    # .main.md
    ocaml "$bn.main.ml" > "$bn.main.md"
    # .main.html
    OMD < "$bn.main.md" > "$bn.main.html"
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
    (CAT common.ml "$bn.md.ml" ; MPPOCAML < _templates/main.mpp) > "$bn.html.ml"
    # .html
    ocaml "$bn.html.ml" > "$bn.html"
    mv "$bn.html" posts/

    # atom feed
    CAT "$bn.md.ml" >> blog-atom-body.ml
    MPPOCAML < _templates/atom-body.xml.ml.mpp >> blog-atom-body.ml
    # rss feed
    CAT "$bn.md.ml" >> blog-rss-body.ml
    MPPOCAML < _templates/rss-body.xml.ml.mpp >> blog-rss-body.ml

done

cat > blog-atom.xml <<EOF
<?xml version="1.0"?>
<feed xmlns="http://www.w3.org/2005/Atom">
 
  <title>pw374</title>
  <link href="http://pw374.github.io/"/>
  <link type="application/atom+xml" rel="self" href="http:///pw374.github.io/blog-atom.xml"/>
  <updated></updated>
  <id>http://pw374.github.io/</id>
  <author>
    <name>Philippe Wang</name>
    <email>philippe.wang@cl.cam.ac.uk</email>
  </author>
EOF
echo foobar
ocaml blog-atom-body.ml >> blog-atom.xml
cat >> blog-atom.xml <<EOF
</feed>
EOF

cat > blog-rss.xml <<EOF
<?xml version="1.0"?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title>pw374</title>
    <link>http://pw374.github.io/</link>
    <atom:link href="http://pw374.net/rss.xml" rel="self" type="application/rss+xml" />
    <description>pw374 blog</description>
    <language>en-gb</language>
    <pubDate>{< let _ = input_command "date '+%a, %d %b %Y %H:%M:%S %z'" >}</pubDate>
    <lastBuildDate>{< let _ = input_command "date '+%a, %d %b %Y %H:%M:%S %z'" >}</lastBuildDate>
EOF
echo foobar2
ocaml -stdin < blog-rss-body.ml >> blog-rss.xml
cat >> blog-rss.xml <<EOF
  </channel>
</rss>
EOF

# CAT common.ml > rss.ml
# echo \#5
# MPPOCAML < rss.xml >> rss.ml
# echo \#6
# contents=blog-rss-body.xml ocaml rss.ml > blog-rss.xml
# echo \#7

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
 let title = "blog#$(basename "$x")"
 let id = "pw374.github.io--" ^ input_command "date +%Y-%m-%d-%H-%M-%S" ^ "--index"
 let xmldate = input_command "date --rfc-3339=seconds|tr ' ' T"
 let rssdate = input_command "date '+%a, %d %b %Y %H:%M:%S %z'"
 let date = input_command "date --rfc-3339=seconds|tr ' ' T"
 let tags = [ "$(basename "$x")"; ]
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
    export selfbn="$bn"
    # .md.ml
    MPPOCAML < "$bn.md.ml.mpp" > "$bn.md.ml"
    # .main.ml
    CAT common.ml "$bn.md.ml" ml_to_md.ml > "$bn.main.ml"
    # .main.md
    ocaml "$bn.main.ml" > "$bn.main.md"
    # .main.html
    OMD < "$bn.main.md" > "$bn.main.html"
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
    (CAT common.ml "$bn.md.ml"  ; MPPOCAML < _templates/main.mpp) > "$bn.html.ml"
    # .html
    ocaml "$bn.html.ml" > "$bn.html"
done


    # generate RSS feed
    ## general (all-tags)
    ## tag-feed
    
    # generate Atom feed
    ## general (all-tags)
    ## tag-feed
