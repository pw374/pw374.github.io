# Copyright (C) 2013  Philippe Wang

if [[ "$1" != "" ]]
then
    target=_drafts/"$(date -u +%Y-%m-%d-%H-%M-%S)-$1.md.ml.mpp"
else
    target=_drafts/"$(date -u +%Y-%m-%d-%H-%M-%S).md.ml.mpp"
fi

cat > $target <<EOF
%% Copyright (C) 2013  Philippe Wang -*- coding: utf-8 -*-
{< module Meta : Meta = struct
 let title = ""
 let id = "posts/$(basename $target .md.ml.mpp).html"
 let xmldate = "$(date -u --rfc-3339=seconds|tr ' ' T)"
 let xmldate_updated = xmldate
 let rssdate = "$(date -u '+%a, %d %b %Y %H:%M:%S %z')"
 let date = "$(date -u --rfc-3339=seconds)"
 let tags = [ ]
 let disqus = true
 let stamp = true
 let tryocaml = false
end
include Meta

module Post(Printer:Printer) = struct open Printer >}

Lorem  ipsum dolor  sit  amet, consectetur  adipisicing  elit, sed  do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
minim  veniam,  quis  nostrud  exercitation ullamco  laboris  nisi  ut
aliquip   ex  ea  commodo   consequat.  Duis   aute  irure   dolor  in
reprehenderit in  voluptate velit esse  cillum dolore eu  fugiat nulla
pariatur.  Excepteur sint  occaecat  cupidatat non  proident, sunt  in
culpa qui officia deserunt mollit anim id est laborum.



{< end >}%% don't write anything after this line
EOF

echo "# File $target has been created. Some commands you might want to run:"
echo "emacs -nw $target"
echo "open -a Emacs $target"
echo "open -a 'Carbon Emacs' $target"
echo "rm -f '$target'"
echo "mv '$(dirname $target)/$(basename $target .md.ml.mpp){,}.md.ml.mpp'"



