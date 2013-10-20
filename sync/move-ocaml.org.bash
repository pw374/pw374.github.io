#!/bin/bash
# $Id: sh-insert.sh,v 1.1 2006/08/26 01:08:18 philippej Exp $

# (cd src/html && find . -name \*.html) |while read l ; do if [[ -f md/"$(dirname "$l")/$(basename "$l" html)md" ]] ; then echo git mv "$l" md/"$(dirname "$l")/$(basename "$l" html)md" ; else echo "$l has no md equiv"; fi ; done|sort

cd ~/OCL/
rm -fr ocaml.org
cp -a ocaml.org-before-branch ocaml.org || exit
ls ocaml.org sandbox-ocaml.org || exit
cd ocaml.org/ || exit
git checkout redesign
cp -a ../sandbox-ocaml.org/md-pages/ src/site
find src/site -type f -delete

git mv src/html/index.html                                                src/site/index.md
git mv src/html/meetings/index.html                                       src/site/meetings/index.md
git mv src/html/meetings/ocaml/2013/call.html                             src/site/meetings/ocaml/2013/call.md
git mv src/html/meetings/ocaml/2013/index.html                            src/site/meetings/ocaml/2013/index.md
git mv src/html/meetings/ocaml/2013/program.html                          src/site/meetings/ocaml/2013/program.md
git mv src/html/meetings/ocaml/2013/talks/index.html                      src/site/meetings/ocaml/2013/talks/index.md
git mv src/html/meetings/ocaml/index.html                                 src/site/meetings/ocaml/index.md
git mv src/html/menu.fr.html                                              src/site/menu.fr.md
git mv src/html/menu.html                                                 src/site/menu.md
git mv src/html/menu.it.html                                              src/site/menu.it.md
git mv src/html/releases/3.12.1.html                                      src/site/releases/3.12.1.md
git mv src/html/releases/4.00.1.html                                      src/site/releases/4.00.1.md
git mv src/html/releases/index.html                                       src/site/releases/index.md
git mv src/html/releases/svn.html                                         src/site/releases/svn.md
git mv src/html/videos.html                                               src/site//docs/videos.md
git mv src/html/books.html                                                src/site/learn/books.md
git mv src/html/caml-light/faq.html                                       src/site/releases/caml-light/faq.md
git mv src/html/caml-light/index.html                                     src/site/releases/caml-light/index.md
git mv src/html/caml-light/license.html                                   src/site/releases/caml-light/license.md
git mv src/html/caml-light/releases/0.75.html                             src/site/releases/caml-light/releases/0.75.md
git mv src/html/caml-light/releases/index.html                            src/site/releases/caml-light/releases/index.md
git mv src/html/cheat_sheets.html                                         src/site/docs/cheat_sheets.md
git mv src/html/companies.html                                            src/site/learn/companies.md
git mv src/html/consortium/license.fr.html                                src/site/docs/consortium-license.fr.md
git mv src/html/consortium/license.html                                   src/site/docs/consortium-license.md
git mv src/html/debug.html                                                src/site/learn/tutorials/debug.md
git mv src/html/description.html                                          src/site/learn/description.md
git mv src/html/dev_tools.html                                            src/site/learn/tutorials/dev_tools.md
git mv src/html/faq.html                                                  src/site/learn/faq.md
git mv src/html/history.fr.html                                           src/site/learn/history.fr.md
git mv src/html/history.html                                              src/site/learn/history.md
git mv src/html/install.fr.html                                           src/site/docs/install.fr.md
git mv src/html/install.html                                              src/site/docs/install.md
git mv src/html/libraries.html                                            src/site/learn/libraries.md
git mv src/html/license.fr.html                                           src/site/docs/license.fr.md
git mv src/html/license.html                                              src/site/docs/license.md
git mv src/html/logos.html                                                src/site/docs/logos.md
git mv src/html/mailing_lists.fr.html                                     src/site/community/mailing_lists.fr.md
git mv src/html/mailing_lists.html                                        src/site/community/mailing_lists.md
git mv src/html/papers.html                                               src/site/docs/papers.md
git mv src/html/planet/index.html                                         src/site/community/planet.md
git mv src/html/portability.html                                          src/site/learn/portability.md
git mv src/html/success.fr.html                                           src/site/learn/success.fr.md
git mv src/html/success.html                                              src/site/learn/success.md
git mv src/html/support.fr.html                                           src/site/community/support.fr.md
git mv src/html/support.html                                              src/site/community/support.md
git mv src/html/taste.fr.html                                             src/site/learn/taste.fr.md
git mv src/html/taste.html                                                src/site/learn/taste.md
git mv src/html/tutorials/99problems.html                                 src/site/learn/tutorials/99problems.md
git mv src/html/tutorials/acknowledgements.html                           src/site/learn/tutorials/acknowledgements.md
git mv src/html/tutorials/basics.fr.html                                  src/site/learn/tutorials/basics.fr.md
git mv src/html/tutorials/basics.html                                     src/site/learn/tutorials/basics.md
git mv src/html/tutorials/basics.it.html                                  src/site/learn/tutorials/basics.it.md
git mv src/html/tutorials/calling_c_libraries.html                        src/site/learn/tutorials/calling_c_libraries.md
git mv src/html/tutorials/calling_fortran_libraries.html                  src/site/learn/tutorials/calling_fortran_libraries.md
git mv src/html/tutorials/camlp4_3.10/dynamic_functor_example.html        src/site/learn/tutorials/camlp4_3.10/dynamic_functor_example.md
git mv src/html/tutorials/camlp4_3.10/dynamic_old_syntax.html             src/site/learn/tutorials/camlp4_3.10/dynamic_old_syntax.md
git mv src/html/tutorials/camlp4_3.10/foreach_tutorial.html               src/site/learn/tutorials/camlp4_3.10/foreach_tutorial.md
git mv src/html/tutorials/camlp4_3.10/quick_non_extensible_example.html   src/site/learn/tutorials/camlp4_3.10/quick_non_extensible_example.md
git mv src/html/tutorials/camlp4_3.10/static_functor_example.html         src/site/learn/tutorials/camlp4_3.10/static_functor_example.md
git mv src/html/tutorials/camlp4_3.10/static_old_syntax.html              src/site/learn/tutorials/camlp4_3.10/static_old_syntax.md
git mv src/html/tutorials/camlp4_3.10.html                                src/site/learn/tutorials/camlp4_3.10.md
git mv src/html/tutorials/camlp5.html                                     src/site/learn/tutorials/camlp5.md
git mv src/html/tutorials/command-line_arguments.html                     src/site/learn/tutorials/command-line_arguments.md
git mv src/html/tutorials/common_error_messages.html                      src/site/learn/tutorials/common_error_messages.md
git mv src/html/tutorials/comparison_of_standard_containers.html          src/site/learn/tutorials/comparison_of_standard_containers.md
git mv src/html/tutorials/compiling_ocaml_projects.html                   src/site/learn/tutorials/compiling_ocaml_projects.md
git mv src/html/tutorials/compiling_with_gnu_make.html                    src/site/learn/tutorials/compiling_with_gnu_make.md
git mv src/html/tutorials/compiling_with_omake.html                       src/site/learn/tutorials/compiling_with_omake.md
git mv src/html/tutorials/data_types_and_matching.fr.html                 src/site/learn/tutorials/data_types_and_matching.fr.md
git mv src/html/tutorials/data_types_and_matching.html                    src/site/learn/tutorials/data_types_and_matching.md
git mv src/html/tutorials/file_manipulation.html                          src/site/learn/tutorials/file_manipulation.md
git mv src/html/tutorials/filenames.html                                  src/site/learn/tutorials/filenames.md
git mv src/html/tutorials/format.fr.html                                  src/site/learn/tutorials/format.fr.md
git mv src/html/tutorials/format.html                                     src/site/learn/tutorials/format.md
git mv src/html/tutorials/functional_programming.fr.html                  src/site/learn/tutorials/functional_programming.fr.md
git mv src/html/tutorials/functional_programming.html                     src/site/learn/tutorials/functional_programming.md
git mv src/html/tutorials/garbage_collection.html                         src/site/learn/tutorials/garbage_collection.md
git mv src/html/tutorials/guidelines.html                                 src/site/learn/tutorials/guidelines.md
git mv src/html/tutorials/hashtbl.html                                    src/site/learn/tutorials/hashtbl.md
git mv src/html/tutorials/if_statements_loops_and_recursion.html          src/site/learn/tutorials/if_statements_loops_and_recursion.md
git mv src/html/tutorials/index.html                                      src/site/learn/tutorials/index.md
git mv src/html/tutorials/introduction_to_gtk.html                        src/site/learn/tutorials/introduction_to_gtk.md
git mv src/html/tutorials/labels.html                                     src/site/learn/tutorials/labels.md
git mv src/html/tutorials/map.html                                        src/site/learn/tutorials/map.md
git mv src/html/tutorials/modules.html                                    src/site/learn/tutorials/modules.md
git mv src/html/tutorials/null_pointers_asserts_and_warnings.fr.html      src/site/learn/tutorials/null_pointers_asserts_and_warnings.fr.md
git mv src/html/tutorials/null_pointers_asserts_and_warnings.html         src/site/learn/tutorials/null_pointers_asserts_and_warnings.md
git mv src/html/tutorials/objects.html                                    src/site/learn/tutorials/objects.md
git mv src/html/tutorials/ocaml_and_the_web.html                          src/site/learn/tutorials/ocaml_and_the_web.md
git mv src/html/tutorials/performance_and_profiling.html                  src/site/learn/tutorials/performance_and_profiling.md
git mv src/html/tutorials/pointers.html                                   src/site/learn/tutorials/pointers.md
git mv src/html/tutorials/set.html                                        src/site/learn/tutorials/set.md
git mv src/html/tutorials/standard_library_examples.html                  src/site/learn/tutorials/standard_library_examples.md
git mv src/html/tutorials/stream_expressions.html                         src/site/learn/tutorials/stream_expressions.md
git mv src/html/tutorials/streams.html                                    src/site/learn/tutorials/streams.md
git mv src/html/tutorials/structure_of_ocaml_programs.fr.html             src/site/learn/tutorials/structure_of_ocaml_programs.fr.md
git mv src/html/tutorials/structure_of_ocaml_programs.html                src/site/learn/tutorials/structure_of_ocaml_programs.md
git mv src/html/tutorials/structure_of_ocaml_programs.it.html             src/site/learn/tutorials/structure_of_ocaml_programs.it.md

git commit -a -m '(redesign) git-mv for html files'

(cd src/site/docs/ && ln -sf consortium-license.md consortium-license.fr.md)
(cd src/site/releases/caml-light/releases/ && ln -sf 0.75.md index.md)
(cd src/site/releases/ && ln -sf 4.00.1.md index.md)

git commit src/site/docs/consortium-license.fr.md src/site/releases/caml-light/releases/index.md src/site/releases/index.md -m '(redesign) fix symb links' 

\cp -a ../sandbox-ocaml.org/md-pages/* src/site/
git commit -a -m '(redesign) actual html->md conversion'

git add src/site
git commit -a -m '(redesign) add missing pieces'

mkdir -p src/site/meetings/ocaml/2013/proposals/
git mv src/html/meetings/ocaml/2013/proposals/* src/site/meetings/ocaml/2013/proposals/
mkdir -p src/site/meetings/ocaml/2013/slides/
git mv src/html/meetings/ocaml/2013/slides/* src/site/meetings/ocaml/2013/slides/
git mv src/{html,site}/robots.txt
mkdir -p src/site/tutorials/camlp4_3.10/
git mv src/html/tutorials/camlp4_3.10/*.ml src/site/tutorials/camlp4_3.10/
mkdir -p src/site/img/
git mv src/html/img/* src/site/img/
git mv src/{html,site}/CNAME
mkdir -p src/site/js/
git mv src/html/js/getElementsByClassName-1.0.1.js  src/site/js/
git commit -a -m '(redesign) git-mv for non-html files'

git rm -r src/html/ext/
git rm -r src/html/css
git rm src/html/ocaml_license.inc
git rm src/html/consortium/index.html
git commit -a -m '(redesign) git-rm non relevant files'

find src/html -type d -delete
rmdir src/html

git commit -a -m '(redesign) fix the rest, if any.'


cp ~/OCL/pw374.github.io/sync/Makefile.{common,from_{md,html}} src/
git add src/Makefile.{common,from_{md,html}}
git commit src/Makefile.{common,from_{md,html}} -m '(redesign) Makefiles + gen.bash' 


exit 0

