# Copyright (C) 2013  Philippe Wang

if [[ "$1" != "" ]]
then
    target="$1".md.ml.mpp
else
    target=_drafts/"$(date -u +%Y-%m-%d-%H-%M-%S).md.ml.mpp"
fi

cat > $target <<EOF
%% Copyright (C) 2013  Philippe Wang
{< module Meta : Meta = struct
 let title = ""
 let id = "pw374.github.io--$(date +%Y-%m-%d-%H-%M-%S)--$RANDOM"
 let xmldate = "$(date --rfc-3339=seconds|tr ' ' T)"
 let rssdate = "$(date '+%a, %d %b %Y %H:%M:%S %z')"
 let date = "$(date --rfc-3339=seconds)"
 let tags = [ "ocaml"; "cooking"; "pastry"; "raspberrypi"; "linux"; "osx"; ]
end
include Meta
>}

{< module Post(Unit:Unit) = struct >}
## {< let _ = !!title >}


Lorem  ipsum dolor  sit  amet, consectetur  adipisicing  elit, sed  do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
minim  veniam,  quis  nostrud  exercitation ullamco  laboris  nisi  ut
aliquip   ex  ea  commodo   consequat.  Duis   aute  irure   dolor  in
reprehenderit in  voluptate velit esse  cillum dolore eu  fugiat nulla
pariatur.  Excepteur sint  occaecat  cupidatat non  proident, sunt  in
culpa qui officia deserunt mollit anim id est laborum.



{< end >}

EOF

echo "# File $target has been created. Some commands you might want to run:"
echo "emacs -nw $target"
echo "open -a Emacs $target"
echo "open -a 'Carbon Emacs' $target"
echo "rm -f '$target'"



