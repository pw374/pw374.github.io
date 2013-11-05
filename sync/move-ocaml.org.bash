#!/bin/bash
# $Id: sh-insert.sh,v 1.1 2006/08/26 01:08:18 philippej Exp $

# (cd src/html && find . -name \*.html) |while read l ; do if [[ -f md/"$(dirname "$l")/$(basename "$l" html)md" ]] ; then echo git mv "$l" md/"$(dirname "$l")/$(basename "$l" html)md" ; else echo "$l has no md equiv"; fi ; done|sort

cd ~/OCL/
rm -fr ocaml.org
cp -a ocaml.org-before-branch ocaml.org || exit
ls ocaml.org sandbox-ocaml.org || exit
cd ocaml.org/ || exit
git checkout -b redesign || exit
mkdir -p src/{site,tpl}

\cp -a ../sandbox-ocaml.org/md-pages/* src/site/
find src/site -type f -delete

mv src/site/{releases,}/caml-light/ 

echo '(redesign) html->md: name change' > /tmp/msg
x=0
for i in \
 src/html/index.html                                                src/site/index.md \
 src/html/meetings/index.html                                       src/site/meetings/index.md \
 src/html/meetings/ocaml/2013/call.html                             src/site/meetings/ocaml/2013/call.md \
 src/html/meetings/ocaml/2013/index.html                            src/site/meetings/ocaml/2013/index.md \
 src/html/meetings/ocaml/2013/program.html                          src/site/meetings/ocaml/2013/program.md \
 src/html/meetings/ocaml/2013/talks/index.html                      src/site/meetings/ocaml/2013/talks/index.md \
 src/html/meetings/ocaml/index.html                                 src/site/meetings/ocaml/index.md \
 src/html/menu.fr.html                                              src/site/menu.fr.md \
 src/html/menu.html                                                 src/site/menu.md \
 src/html/menu.it.html                                              src/site/menu.it.md \
 src/html/releases/3.12.1.html                                      src/site/releases/3.12.1.md \
 src/html/releases/4.00.1.html                                      src/site/releases/4.00.1.md \
 src/html/releases/index.html                                       src/site/releases/index.md \
 src/html/releases/svn.html                                         src/site/releases/svn.md \
 src/html/videos.html                                               src/site/docs/videos.md \
 src/html/books.html                                                src/site/learn/books.md \
 src/html/caml-light/faq.html                                       src/site/caml-light/faq.md \
 src/html/caml-light/index.html                                     src/site/caml-light/index.md \
 src/html/caml-light/license.html                                   src/site/caml-light/license.md \
 src/html/caml-light/releases/0.75.html                             src/site/caml-light/releases/0.75.md \
 src/html/caml-light/releases/index.html                            src/site/caml-light/releases/index.md \
 src/html/cheat_sheets.html                                         src/site/docs/cheat_sheets.md \
 src/html/companies.html                                            src/site/learn/companies.md \
 src/html/consortium/license.fr.html                                src/site/docs/consortium-license.fr.md \
 src/html/consortium/license.html                                   src/site/docs/consortium-license.md \
 src/html/debug.html                                                src/site/learn/tutorials/debug.md \
 src/html/description.html                                          src/site/learn/description.md \
 src/html/dev_tools.html                                            src/site/learn/tutorials/dev_tools.md \
 src/html/faq.html                                                  src/site/learn/faq.md \
 src/html/history.fr.html                                           src/site/learn/history.fr.md \
 src/html/history.html                                              src/site/learn/history.md \
 src/html/install.fr.html                                           src/site/docs/install.fr.md \
 src/html/install.html                                              src/site/docs/install.md \
 src/html/libraries.html                                            src/site/learn/libraries.md \
 src/html/license.fr.html                                           src/site/docs/license.fr.md \
 src/html/license.html                                              src/site/docs/license.md \
 src/html/logos.html                                                src/site/docs/logos.md \
 src/html/mailing_lists.fr.html                                     src/site/community/mailing_lists.fr.md \
 src/html/mailing_lists.html                                        src/site/community/mailing_lists.md \
 src/html/papers.html                                               src/site/docs/papers.md \
 src/html/planet/index.html                                         src/site/community/planet.md \
 src/html/portability.html                                          src/site/learn/portability.md \
 src/html/success.fr.html                                           src/site/learn/success.fr.md \
 src/html/success.html                                              src/site/learn/success.md \
 src/html/support.fr.html                                           src/site/community/support.fr.md \
 src/html/support.html                                              src/site/community/support.md \
 src/html/taste.fr.html                                             src/site/learn/taste.fr.md \
 src/html/taste.html                                                src/site/learn/taste.md \
 src/html/tutorials/99problems.html                                 src/site/learn/tutorials/99problems.md \
 src/html/tutorials/acknowledgements.html                           src/site/learn/tutorials/acknowledgements.md \
 src/html/tutorials/basics.fr.html                                  src/site/learn/tutorials/basics.fr.md \
 src/html/tutorials/basics.html                                     src/site/learn/tutorials/basics.md \
 src/html/tutorials/basics.it.html                                  src/site/learn/tutorials/basics.it.md \
 src/html/tutorials/calling_c_libraries.html                        src/site/learn/tutorials/calling_c_libraries.md \
 src/html/tutorials/calling_fortran_libraries.html                  src/site/learn/tutorials/calling_fortran_libraries.md \
 src/html/tutorials/camlp4_3.10/dynamic_functor_example.html        src/site/learn/tutorials/camlp4_3.10/dynamic_functor_example.md \
 src/html/tutorials/camlp4_3.10/dynamic_old_syntax.html             src/site/learn/tutorials/camlp4_3.10/dynamic_old_syntax.md \
 src/html/tutorials/camlp4_3.10/foreach_tutorial.html               src/site/learn/tutorials/camlp4_3.10/foreach_tutorial.md \
 src/html/tutorials/camlp4_3.10/quick_non_extensible_example.html   src/site/learn/tutorials/camlp4_3.10/quick_non_extensible_example.md \
 src/html/tutorials/camlp4_3.10/static_functor_example.html         src/site/learn/tutorials/camlp4_3.10/static_functor_example.md \
 src/html/tutorials/camlp4_3.10/static_old_syntax.html              src/site/learn/tutorials/camlp4_3.10/static_old_syntax.md \
 src/html/tutorials/camlp4_3.10.html                                src/site/learn/tutorials/camlp4_3.10.md \
 src/html/tutorials/camlp5.html                                     src/site/learn/tutorials/camlp5.md \
 src/html/tutorials/command-line_arguments.html                     src/site/learn/tutorials/command-line_arguments.md \
 src/html/tutorials/common_error_messages.html                      src/site/learn/tutorials/common_error_messages.md \
 src/html/tutorials/comparison_of_standard_containers.html          src/site/learn/tutorials/comparison_of_standard_containers.md \
 src/html/tutorials/compiling_ocaml_projects.html                   src/site/learn/tutorials/compiling_ocaml_projects.md \
 src/html/tutorials/compiling_with_gnu_make.html                    src/site/learn/tutorials/compiling_with_gnu_make.md \
 src/html/tutorials/compiling_with_omake.html                       src/site/learn/tutorials/compiling_with_omake.md \
 src/html/tutorials/data_types_and_matching.fr.html                 src/site/learn/tutorials/data_types_and_matching.fr.md \
 src/html/tutorials/data_types_and_matching.html                    src/site/learn/tutorials/data_types_and_matching.md \
 src/html/tutorials/file_manipulation.html                          src/site/learn/tutorials/file_manipulation.md \
 src/html/tutorials/filenames.html                                  src/site/learn/tutorials/filenames.md \
 src/html/tutorials/format.fr.html                                  src/site/learn/tutorials/format.fr.md \
 src/html/tutorials/format.html                                     src/site/learn/tutorials/format.md \
 src/html/tutorials/functional_programming.fr.html                  src/site/learn/tutorials/functional_programming.fr.md \
 src/html/tutorials/functional_programming.html                     src/site/learn/tutorials/functional_programming.md \
 src/html/tutorials/garbage_collection.html                         src/site/learn/tutorials/garbage_collection.md \
 src/html/tutorials/guidelines.html                                 src/site/learn/tutorials/guidelines.md \
 src/html/tutorials/hashtbl.html                                    src/site/learn/tutorials/hashtbl.md \
 src/html/tutorials/if_statements_loops_and_recursion.html          src/site/learn/tutorials/if_statements_loops_and_recursion.md \
 src/html/tutorials/index.html                                      src/site/learn/tutorials/index.md \
 src/html/tutorials/introduction_to_gtk.html                        src/site/learn/tutorials/introduction_to_gtk.md \
 src/html/tutorials/labels.html                                     src/site/learn/tutorials/labels.md \
 src/html/tutorials/map.html                                        src/site/learn/tutorials/map.md \
 src/html/tutorials/modules.html                                    src/site/learn/tutorials/modules.md \
 src/html/tutorials/null_pointers_asserts_and_warnings.fr.html      src/site/learn/tutorials/null_pointers_asserts_and_warnings.fr.md \
 src/html/tutorials/null_pointers_asserts_and_warnings.html         src/site/learn/tutorials/null_pointers_asserts_and_warnings.md \
 src/html/tutorials/objects.html                                    src/site/learn/tutorials/objects.md \
 src/html/tutorials/ocaml_and_the_web.html                          src/site/learn/tutorials/ocaml_and_the_web.md \
 src/html/tutorials/performance_and_profiling.html                  src/site/learn/tutorials/performance_and_profiling.md \
 src/html/tutorials/pointers.html                                   src/site/learn/tutorials/pointers.md \
 src/html/tutorials/set.html                                        src/site/learn/tutorials/set.md \
 src/html/tutorials/standard_library_examples.html                  src/site/learn/tutorials/standard_library_examples.md \
 src/html/tutorials/stream_expressions.html                         src/site/learn/tutorials/stream_expressions.md \
 src/html/tutorials/streams.html                                    src/site/learn/tutorials/streams.md \
 src/html/tutorials/structure_of_ocaml_programs.fr.html             src/site/learn/tutorials/structure_of_ocaml_programs.fr.md \
 src/html/tutorials/structure_of_ocaml_programs.html                src/site/learn/tutorials/structure_of_ocaml_programs.md \
 src/html/tutorials/structure_of_ocaml_programs.it.html             src/site/learn/tutorials/structure_of_ocaml_programs.it.md 
do
if (( x % 2 == 0 ))
then
old=$i
else
# echo git mv "$old" "$i"
git mv "$old" "$i" || exit 1
echo -e "$old -> $i" >> /tmp/msg
fi
(( x++ ))
done
EDITOR='cp /tmp/msg' git commit -a
rm -f /tmp/msg

\cp -a ../sandbox-ocaml.org/md-pages/* src/site/

function fixtpl () {
find src/site src/tpl -type f -exec \
sed -i.old \
 -e 's|main_tpl\.mpp|tpl/main.mpp|g' \
 -e 's| tryocaml\.html| tpl/tryocaml.html|g' \
 -e 's|navbar_tpl\.mpp|tpl/navbar.mpp|g' \
 -e 's|core_tpl\.mpp|tpl/core.mpp|g' \
 -e 's|front_package_tpl\.mpp|tpl/front_package.mpp|g' \
 -e 's|front_news_tpl\.mpp|tpl/front_news.mpp|g' \
 -e 's|front_code_snippet_tpl\.html|tpl/front_code_snippet.html|g' \
 -e 's|last_ml_topics_tpl\.mpp|tpl/last_ml_topics.mpp|g' \
 -e 's|/static/|/|g' \
{} \;
find src/site src/tpl -name '*.old' -delete
}

fixtpl
git commit -a -m '(redesign) html->md: actual conversion'

cp ~/OCL/sandbox-ocaml.org/last_ml_topics_tpl.mpp src/tpl/last_ml_topics.mpp
cp ~/OCL/sandbox-ocaml.org/main_tpl.mpp src/tpl/main.mpp
cp ~/OCL/sandbox-ocaml.org/tryocaml.html src/tpl/tryocaml.html
sed -e 's|="/pkg/"|="http://opam.ocaml.org/"|g' ~/OCL/sandbox-ocaml.org/navbar_tpl.mpp > src/tpl/navbar.mpp
cp ~/OCL/sandbox-ocaml.org/core_tpl.mpp src/tpl/core.mpp
cp ~/OCL/sandbox-ocaml.org/front_package_tpl.mpp src/tpl/front_package.mpp
cp ~/OCL/sandbox-ocaml.org/front_news_tpl.mpp src/tpl/front_news.mpp
cp ~/OCL/sandbox-ocaml.org/front_code_snippet_tpl.md src/tpl/front_code_snippet.md
fixtpl
git add tpl
git commit -a -m '(redesign) Add template files'

# cp ~/OCL/sandbox-ocaml.org/tryocaml.js src/tryocaml.js


(cd src/site/docs/ && ln -sf consortium-license.md consortium-license.fr.md)
(cd src/site/releases/caml-light/releases/ && ln -sf 0.75.md index.md)
(cd src/site/releases/ && ln -sf 4.00.1.md index.md)

git commit src/site/docs/consortium-license.fr.md src/site/releases/caml-light/releases/index.md src/site/releases/index.md -m '(redesign) fix symb links' 

rm -f src/site/packages/core-list.html.mpp
find src -name '.*' -delete
git add src/site
git commit -a -m '(redesign) Add missing pieces'

mkdir -p src/site/meetings/ocaml/2013/proposals/
git mv src/html/meetings/ocaml/2013/proposals/* src/site/meetings/ocaml/2013/proposals/
mkdir -p src/site/meetings/ocaml/2013/slides/
git mv src/html/meetings/ocaml/2013/slides/* src/site/meetings/ocaml/2013/slides/
git mv src/{html,site}/robots.txt
mkdir -p src/site/tutorials/camlp4_3.10/
git mv src/html/tutorials/camlp4_3.10/*.ml src/site/learn/tutorials/camlp4_3.10/
mkdir -p src/site/img/
git mv src/html/img/* src/site/img/
git mv src/{html,site}/CNAME
mkdir -p src/site/js/
git mv src/html/js/getElementsByClassName-1.0.1.js  src/site/js/
git commit -a -m '(redesign) Move non-HTML files from src/html to src/site'

cp -a ~/OCL/sandbox-ocaml.org/skin/static/{css,img} src/site/
cp ~/OCL/sandbox-ocaml.org/{rss2html.ml,ocamlapplet.bash,ocamltohtml.ml,lexer.ml} src/
find src -name '.*' -delete
git add src/site/{css,img} src/{rss2html.ml,ocamlapplet.bash,ocamltohtml.ml,lexer.ml} src/tpl
git commit -a -m '(redesign) Add non-html files'

find src/html/ext src/html/css
git rm -r src/html/ext/jquery-1.8.0.min.js src/html/ext/bootstrap-v2.0.4 src/html/ext/bootstrap
git rm -r src/html/css/ocaml.css
git rm src/html/ocaml_license.inc
git rm src/html/consortium/index.html
git commit -a -m '(redesign) Remove non relevant files'

find src/html -type d -delete
rmdir src/html

git commit -a -m '(redesign) Fix the rest, if any.'

cp ~/OCL/pw374.github.io/sync/Makefile.{common,from_{md,html}} src/
cp ~/OCL/pw374.github.io/sync/Makefile src/
cp ~/OCL/pw374.github.io/sync/gen.bash src/
git add src/Makefile src/Makefile.{common,from_{md,html}} src/gen.bash
git commit src/Makefile src/Makefile.{common,from_{md,html}} src/gen.bash -m '(redesign) Makefiles + gen.bash'

cat > src/README-redesign.md <<\EOF
# Dependencies

  * rsync
  * mpp >= 0.1.1 (available as an opam package)
  * frag (available as an opam package)
  * omd >= 0.6.1 (available as an opam package)
  * gnu make
  * bash
  * ocamlopt
  * rss2html.ml needs packages {bigarray,unix,str,netsys,xmlm,netclient,rss} to compile (packages available in opam)

# How to build the web site

Once you have the dependencies, the following command should build a
directory called `src/ocaml.org`, which would contain the generated
website. `cd src && make`

If the command fails, check your dependencies. And of course, if you
have contributed, check your contributions. 
Otherwise, please file a bug report.

# How to contribute

Files that should end up on the web site are in `src/site`, they
include markdown files, pictures, JS files, CSS, etc.
Files that are used to build the web site, which are more "software"
than contents are in `src/` (e.g., Makefiles) and `src/tpl` (i.e.,
template files).

When using a template in a file in `src/site`, one should always refer
to it as `tpl/template-file-name` as the building script runs from
`src`. Also, having `tpl/` in the filename means you don't need to
have any `tpl` prefix or suffix in your template filename.

EOF
git add src/README-redesign.md
git commit -m 'README for the redesigned ocaml.org'

git status

exit 0
 
