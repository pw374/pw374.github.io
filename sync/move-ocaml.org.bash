#!/bin/bash
# $Id: sh-insert.sh,v 1.1 2006/08/26 01:08:18 philippej Exp $

# (cd src/html && find . -name \*.html) |while read l ; do if [[ -f md/"$(dirname "$l")/$(basename "$l" html)md" ]] ; then echo git mv "$l" md/"$(dirname "$l")/$(basename "$l" html)md" ; else echo "$l has no md equiv"; fi ; done|sort

cd ~/OCL/
rm -fr ocaml.org
cp -a ocaml.org-before-branch ocaml.org || exit
ls ocaml.org sandbox-ocaml.org || exit
cd ocaml.org/ || exit
git checkout redesign
cp -a ../sandbox-ocaml.org/md-pages/ src/md
find src/md -type f -delete

git mv src/html/index.html                                                src/md/index.md
git mv src/html/meetings/index.html                                       src/md/meetings/index.md
git mv src/html/meetings/ocaml/2013/call.html                             src/md/meetings/ocaml/2013/call.md
git mv src/html/meetings/ocaml/2013/index.html                            src/md/meetings/ocaml/2013/index.md
git mv src/html/meetings/ocaml/2013/program.html                          src/md/meetings/ocaml/2013/program.md
git mv src/html/meetings/ocaml/2013/talks/index.html                      src/md/meetings/ocaml/2013/talks/index.md
git mv src/html/meetings/ocaml/index.html                                 src/md/meetings/ocaml/index.md
git mv src/html/menu.fr.html                                              src/md/menu.fr.md
git mv src/html/menu.html                                                 src/md/menu.md
git mv src/html/menu.it.html                                              src/md/menu.it.md
git mv src/html/releases/3.12.1.html                                      src/md/releases/3.12.1.md
git mv src/html/releases/4.00.1.html                                      src/md/releases/4.00.1.md
git mv src/html/releases/index.html                                       src/md/releases/index.md
git mv src/html/releases/svn.html                                         src/md/releases/svn.md
git mv src/html/videos.html                                               src/md//docs/videos.md
git mv src/html/books.html                                                src/md/learn/books.md
git mv src/html/caml-light/faq.html                                       src/md/releases/caml-light/faq.md
git mv src/html/caml-light/index.html                                     src/md/releases/caml-light/index.md
git mv src/html/caml-light/license.html                                   src/md/releases/caml-light/license.md
git mv src/html/caml-light/releases/0.75.html                             src/md/releases/caml-light/releases/0.75.md
git mv src/html/caml-light/releases/index.html                            src/md/releases/caml-light/releases/index.md
git mv src/html/cheat_sheets.html                                         src/md/docs/cheat_sheets.md
git mv src/html/companies.html                                            src/md/learn/companies.md
git mv src/html/consortium/license.fr.html                                src/md/docs/consortium-license.fr.md
git mv src/html/consortium/license.html                                   src/md/docs/consortium-license.md
git mv src/html/debug.html                                                src/md/learn/tutorials/debug.md
git mv src/html/description.html                                          src/md/learn/description.md
git mv src/html/dev_tools.html                                            src/md/learn/tutorials/dev_tools.md
git mv src/html/faq.html                                                  src/md/learn/faq.md
git mv src/html/history.fr.html                                           src/md/learn/history.fr.md
git mv src/html/history.html                                              src/md/learn/history.md
git mv src/html/install.fr.html                                           src/md/docs/install.fr.md
git mv src/html/install.html                                              src/md/docs/install.md
git mv src/html/libraries.html                                            src/md/learn/libraries.md
git mv src/html/license.fr.html                                           src/md/docs/license.fr.md
git mv src/html/license.html                                              src/md/docs/license.md
git mv src/html/logos.html                                                src/md/docs/logos.md
git mv src/html/mailing_lists.fr.html                                     src/md/community/mailing_lists.fr.md
git mv src/html/mailing_lists.html                                        src/md/community/mailing_lists.md
git mv src/html/papers.html                                               src/md/docs/papers.md
git mv src/html/planet/index.html                                         src/md/community/planet.md
git mv src/html/portability.html                                          src/md/learn/portability.md
git mv src/html/success.fr.html                                           src/md/learn/success.fr.md
git mv src/html/success.html                                              src/md/learn/success.md
git mv src/html/support.fr.html                                           src/md/community/support.fr.md
git mv src/html/support.html                                              src/md/community/support.md
git mv src/html/taste.fr.html                                             src/md/learn/taste.fr.md
git mv src/html/taste.html                                                src/md/learn/taste.md
git mv src/html/tutorials/99problems.html                                 src/md/learn/tutorials/99problems.md
git mv src/html/tutorials/acknowledgements.html                           src/md/learn/tutorials/acknowledgements.md
git mv src/html/tutorials/basics.fr.html                                  src/md/learn/tutorials/basics.fr.md
git mv src/html/tutorials/basics.html                                     src/md/learn/tutorials/basics.md
git mv src/html/tutorials/basics.it.html                                  src/md/learn/tutorials/basics.it.md
git mv src/html/tutorials/calling_c_libraries.html                        src/md/learn/tutorials/calling_c_libraries.md
git mv src/html/tutorials/calling_fortran_libraries.html                  src/md/learn/tutorials/calling_fortran_libraries.md
git mv src/html/tutorials/camlp4_3.10/dynamic_functor_example.html        src/md/learn/tutorials/camlp4_3.10/dynamic_functor_example.md
git mv src/html/tutorials/camlp4_3.10/dynamic_old_syntax.html             src/md/learn/tutorials/camlp4_3.10/dynamic_old_syntax.md
git mv src/html/tutorials/camlp4_3.10/foreach_tutorial.html               src/md/learn/tutorials/camlp4_3.10/foreach_tutorial.md
git mv src/html/tutorials/camlp4_3.10/quick_non_extensible_example.html   src/md/learn/tutorials/camlp4_3.10/quick_non_extensible_example.md
git mv src/html/tutorials/camlp4_3.10/static_functor_example.html         src/md/learn/tutorials/camlp4_3.10/static_functor_example.md
git mv src/html/tutorials/camlp4_3.10/static_old_syntax.html              src/md/learn/tutorials/camlp4_3.10/static_old_syntax.md
git mv src/html/tutorials/camlp4_3.10.html                                src/md/learn/tutorials/camlp4_3.10.md
git mv src/html/tutorials/camlp5.html                                     src/md/learn/tutorials/camlp5.md
git mv src/html/tutorials/command-line_arguments.html                     src/md/learn/tutorials/command-line_arguments.md
git mv src/html/tutorials/common_error_messages.html                      src/md/learn/tutorials/common_error_messages.md
git mv src/html/tutorials/comparison_of_standard_containers.html          src/md/learn/tutorials/comparison_of_standard_containers.md
git mv src/html/tutorials/compiling_ocaml_projects.html                   src/md/learn/tutorials/compiling_ocaml_projects.md
git mv src/html/tutorials/compiling_with_gnu_make.html                    src/md/learn/tutorials/compiling_with_gnu_make.md
git mv src/html/tutorials/compiling_with_omake.html                       src/md/learn/tutorials/compiling_with_omake.md
git mv src/html/tutorials/data_types_and_matching.fr.html                 src/md/learn/tutorials/data_types_and_matching.fr.md
git mv src/html/tutorials/data_types_and_matching.html                    src/md/learn/tutorials/data_types_and_matching.md
git mv src/html/tutorials/file_manipulation.html                          src/md/learn/tutorials/file_manipulation.md
git mv src/html/tutorials/filenames.html                                  src/md/learn/tutorials/filenames.md
git mv src/html/tutorials/format.fr.html                                  src/md/learn/tutorials/format.fr.md
git mv src/html/tutorials/format.html                                     src/md/learn/tutorials/format.md
git mv src/html/tutorials/functional_programming.fr.html                  src/md/learn/tutorials/functional_programming.fr.md
git mv src/html/tutorials/functional_programming.html                     src/md/learn/tutorials/functional_programming.md
git mv src/html/tutorials/garbage_collection.html                         src/md/learn/tutorials/garbage_collection.md
git mv src/html/tutorials/guidelines.html                                 src/md/learn/tutorials/guidelines.md
git mv src/html/tutorials/hashtbl.html                                    src/md/learn/tutorials/hashtbl.md
git mv src/html/tutorials/if_statements_loops_and_recursion.html          src/md/learn/tutorials/if_statements_loops_and_recursion.md
git mv src/html/tutorials/index.html                                      src/md/learn/tutorials/index.md
git mv src/html/tutorials/introduction_to_gtk.html                        src/md/learn/tutorials/introduction_to_gtk.md
git mv src/html/tutorials/labels.html                                     src/md/learn/tutorials/labels.md
git mv src/html/tutorials/map.html                                        src/md/learn/tutorials/map.md
git mv src/html/tutorials/modules.html                                    src/md/learn/tutorials/modules.md
git mv src/html/tutorials/null_pointers_asserts_and_warnings.fr.html      src/md/learn/tutorials/null_pointers_asserts_and_warnings.fr.md
git mv src/html/tutorials/null_pointers_asserts_and_warnings.html         src/md/learn/tutorials/null_pointers_asserts_and_warnings.md
git mv src/html/tutorials/objects.html                                    src/md/learn/tutorials/objects.md
git mv src/html/tutorials/ocaml_and_the_web.html                          src/md/learn/tutorials/ocaml_and_the_web.md
git mv src/html/tutorials/performance_and_profiling.html                  src/md/learn/tutorials/performance_and_profiling.md
git mv src/html/tutorials/pointers.html                                   src/md/learn/tutorials/pointers.md
git mv src/html/tutorials/set.html                                        src/md/learn/tutorials/set.md
git mv src/html/tutorials/standard_library_examples.html                  src/md/learn/tutorials/standard_library_examples.md
git mv src/html/tutorials/stream_expressions.html                         src/md/learn/tutorials/stream_expressions.md
git mv src/html/tutorials/streams.html                                    src/md/learn/tutorials/streams.md
git mv src/html/tutorials/structure_of_ocaml_programs.fr.html             src/md/learn/tutorials/structure_of_ocaml_programs.fr.md
git mv src/html/tutorials/structure_of_ocaml_programs.html                src/md/learn/tutorials/structure_of_ocaml_programs.md
git mv src/html/tutorials/structure_of_ocaml_programs.it.html             src/md/learn/tutorials/structure_of_ocaml_programs.it.md

git commit -a -m '(redesign) git-mv for html files'

(cd src/md/docs/ && ln -sf consortium-license.md consortium-license.fr.md)
(cd src/md/releases/caml-light/releases/ && ln -sf 0.75.md index.md)
(cd src/md/releases/ && ln -sf 4.00.1.md index.md)

git commit src/md/docs/consortium-license.fr.md src/md/releases/caml-light/releases/index.md src/md/releases/index.md -m '(redesign) fix symb links' 

\cp -a ../sandbox-ocaml.org/md-pages/* src/md/
git commit -a -m '(redesign) actual html->md conversion'

git add src/md
git commit -a -m '(redesign) add missing pieces'

mkdir -p src/md/meetings/ocaml/2013/proposals/
git mv src/html/meetings/ocaml/2013/proposals/* src/md/meetings/ocaml/2013/proposals/
mkdir -p src/md/meetings/ocaml/2013/slides/
git mv src/html/meetings/ocaml/2013/slides/* src/md/meetings/ocaml/2013/slides/
git mv src/{html,md}/robots.txt
mkdir -p src/md/tutorials/camlp4_3.10/
git mv src/html/tutorials/camlp4_3.10/*.ml src/md/tutorials/camlp4_3.10/
mkdir -p src/md/img/
git mv src/html/img/* src/md/img/
git mv src/{html,md}/CNAME
git commit -a -m '(redesign) git-mv for non-html files'

git rm -r src/html/ext/
git rm -r src/html/css
git rm src/html/ocaml_license.inc
git rm src/html/consortium/index.html
git commit -a -m '(redesign) git-rm non relevant files'

find src/html -type d -delete

git commit -a -m '(redesign) fix the rest.'

exit 0

