MPPOCAML = mpp -so '((!' -sc '!))' -son '{{!' -scn '!}}' -soc '' -scc '' -its -snl -l ocaml

OMD = omd -r ocaml=./ocamltohtml -r tryocaml=./ocamltotryocaml

all:
	make htmlescape ocamltohtml
	make ../index.html ../blog.html ../blog-atom.xml ../blog-rss.xml ../try-ocaml.js 
	make -j 4 $$(for i in posts/*.md.ml.mpp ; do \
	  echo ../posts/$$(basename $$i .md.ml.mpp).html ; \
	done)
	git add ../posts/*.html ../tags/*/*.html

ocamltohtml:lexer.ml ocamltohtml.ml
	ocamlc $+ -o $@

htmlescape:htmlescape.ml
	ocamlc $< -o $@

################################################################################
# INDEX
################################################################################
../index.html:_tmp/common.ml _tmp/index.md.ml _tmp/index.contents.html.ml _tmp/index.toc.ml _tmp/main.ml 
	for i in $+ ; do echo "# 1 \"$$i\"" ; cat "$$i" ; done | ocaml -stdin > $@
_tmp/gen_index_contents.ml:Makefile
	echo 'let () = let module M = Post(Printer) in ()' > $@
_tmp/index.md:_tmp/common.ml _tmp/index.md.ml _tmp/gen_index_contents.ml
	make _tmp/blogposts.list.tmp.md
	for i in $+ ; do echo "# 1 \"$$i\"" ; cat "$$i" ; done | ocaml -stdin > $@
_tmp/index.contents.html.ml:_tmp/index.md Makefile
	(printf "module Contents(Printer:Printer) = struct open Printer\n" ; ${OMD} < $< | ${MPPOCAML} ; echo "end") > $@
_tmp/index.toc.ml:_tmp/index.toc.html Makefile
	(printf "module Toc(Printer:Printer) = struct open Printer\n" ; ${MPPOCAML} < $< ; echo "end") > $@
_tmp/index.toc.html:_tmp/index.toc.md Makefile
	omd < $< > $@
_tmp/index.toc.md:../tags/* Makefile
	echo '(The links below may not work yet.)' > $@
	echo >> $@
	(cd ../tags && for i in * ; do echo " * [$$i](/tags/$$(sed 's/ /%20/g' <<<$$i)/)" ; done) >> $@
_tmp/index.md.ml:index.md.ml.mpp Makefile
	${MPPOCAML} < $< > $@
_tmp/blogposts.list.tmp.md:posts/*.md.ml.mpp _tmp/common.ml
	make $$(ls -1 posts/*.md.ml.mpp|sed -e 's/.md.ml.mpp$$/.md.ml/g' -e 's/^posts/_tmp/g')
	(cat _tmp/common.ml ;\
	for i in $$(ls -1 posts/*.md.ml.mpp|sed -e 's/.md.ml.mpp$$/.md.ml/g' -e 's/^posts/_tmp/g') ; do \
		echo "# 1 \"$$i\"" ; \
		cat $$i ; \
		echo "# 1 \"$$i - print\"" ; \
		echo "let space = ' '" ;\
		echo 'let () = printf "* %s [%s](http://pw374.github.io/%s)\n" (String.sub date 0 (String.index date space)) title id' ; \
	done) | ocaml -stdin > $@

################################################################################
# INDIVIDUAL POSTS
################################################################################
../posts/%.html:_tmp/common.ml _tmp/%.md.ml _tmp/%.contents.html.ml _tmp/%.toc.html.ml  _tmp/main.ml
	for i in $+ ; do echo "# 1 \"$$i\"" ; cat "$$i" ; done | ocaml -stdin > $@
_tmp/%.md.ml:posts/%.md.ml.mpp Makefile
	${MPPOCAML} < $< > $@
_tmp/gen_post_contents.ml: Makefile
	echo 'let () = printf "## [%s](/%s)" title id ; let module M = Post(Printer) in ()' > $@

_tmp/%.contents.md:_tmp/%.md.ml _tmp/gen_post_contents.ml _tmp/tag.ml Makefile
	 for i in _tmp/common.ml $< _tmp/tag.ml _tmp/gen_post_contents.ml ; do echo "# 1 \"$$i\"" ; cat "$$i" ; done | ocaml -stdin > $@

_tmp/%.contents.html:_tmp/%.contents.md Makefile
	${OMD} < $< > $@

_tmp/%.contents.html.ml:_tmp/%.contents.html Makefile
	${MPPOCAML} < $< > $@.tmp
	(printf "module Contents(Printer:Printer) = struct open Printer\n" ; cat $@.tmp ; echo "let () = tag_post stamp date tags end") > $@
	rm -f $@.tmp

_tmp/%.toc.html:_tmp/%.contents.md Makefile
	${OMD} -otoc -ts 2 -td 3 < $< > $@

_tmp/%.toc.html.ml:_tmp/%.toc.html Makefile
	${MPPOCAML} < $< > $@.tmp
	(printf "module Toc(Printer:Printer) = struct open Printer\n" ; cat $@.tmp ; echo "end") > $@
	rm -f $@.tmp

################################################################################
# BLOG
################################################################################
../blog.html:_tmp/common.ml _tmp/blog.md.ml _tmp/blog.contents.html.ml _tmp/blog.toc.ml _tmp/main.ml 
	for i in $+ ; do echo "# 1 \"$$i\"" ; cat "$$i" ; done | ocaml -stdin > $@
_tmp/gen_blog_contents.ml:Makefile
	echo 'let () = let module M = Post(Printer) in ()' > $@
_tmp/blog.md:_tmp/blogposts.contents.tmp.md _tmp/common.ml _tmp/blog.md.ml _tmp/gen_blog_contents.ml
	for i in _tmp/common.ml _tmp/blog.md.ml _tmp/gen_blog_contents.ml ; do echo "# 1 \"$$i\"" ; cat "$$i" ; done | ocaml -stdin > $@
_tmp/blog.contents.html.ml:_tmp/blog.md Makefile
	(printf "module Contents(Printer:Printer) = struct open Printer\n" ; ${OMD} < $< | ${MPPOCAML} ; echo "let () = tag_post stamp date tags end") > $@
_tmp/blog.toc.ml:_tmp/blog.toc.html Makefile
	(printf "module Toc(Printer:Printer) = struct open Printer\n" ; ${MPPOCAML} < $< ; echo "end") > $@
_tmp/blog.toc.html:_tmp/blog.toc.md Makefile
	omd < $< > $@
_tmp/blog.toc.md:_tmp/blogposts.contents.tmp.md Makefile
	omd -otoc -ts 2 -td 3 < $< > $@
#	(cd ../tags && for i in * ; do echo " * [$$i](/$$(sed 's/ /%20/g' <<<$$i)/)" ; done) > $@
_tmp/blog.md.ml:blog.md.ml.mpp Makefile
	${MPPOCAML} < $< > $@
_tmp/blogposts.contents.tmp.md:posts/*.md.ml.mpp _tmp/gen_post_contents.ml _tmp/common.ml Makefile 
	make $$(ls -1 posts/*.md.ml.mpp|sed -e 's/.md.ml.mpp$$/.md.ml/g' -e 's/^posts/_tmp/g')
	(cat _tmp/common.ml ;\
	for i in $$(ls -1 posts/*.md.ml.mpp|sed -e 's/.md.ml.mpp$$/.md.ml/g' -e 's/^posts/_tmp/g') ; do \
		echo "# 1 \"$$i\"" ; \
		cat $$i ; \
		echo "# 1 \"$$i - print\"" ; \
		cat _tmp/gen_post_contents.ml ; \
	done) | ocaml -stdin > $@

################################################################################
# BLOG > RSS AND ATOM FEEDS
################################################################################
../blog-rss.xml:_tmp/blog-rss-contents.xml
	echo "<?xml version='1.0'?><rss version='2.0' xmlns:atom='http://www.w3.org/2005/Atom'><channel><title>pw374</title><link>http://pw374.github.io/</link><atom:link href='http://pw374.github.io/blog-rss.xml' rel='self' type='application/rss+xml' /><description>pw374 blog</description><language>en-gb</language><pubDate>$$(date '+%a, %d %b %Y %H:%M:%S %z')</pubDate><lastBuildDate>$$(date '+%a, %d %b %Y %H:%M:%S %z')</lastBuildDate>" > $@
	cat $< >> $@
	echo "</channel></rss>" >> $@

../blog-atom.xml:_tmp/blog-atom-contents.xml
	echo "<?xml version='1.0'?><feed xmlns='http://www.w3.org/2005/Atom'><title>pw374</title><link href='http://pw374.github.io/' /><link type='application/atom+xml' rel='self' href='http://pw374.github.io/blog-atom.xml' /><updated>$$(date --rfc-3339=seconds|tr ' ' T)</updated><id>http://pw374.github.io/</id><author><name>Philippe Wang</name><email>philippe.wang@cl.cam.ac.uk</email></author>" > $@
	cat $< >> $@
	echo "</feed>" >> $@

_tmp/blog-rss-contents.xml:posts/*.md.ml.mpp _tmp/gen_post_contents.ml _tmp/common.ml _tmp/rss-body.xml.ml
	make $$(for i in posts/*.md.ml.mpp ; do echo _tmp/$$(basename $$i .md.ml.mpp).bcontents.html.ml ; done)
	for j in _tmp/*.bcontents.html.ml ; do \
	  for i in _tmp/common.ml _tmp/$$(basename $$j .bcontents.html.ml).md.ml $$j _tmp/rss-body.xml.ml ; do \
	    echo "# 1 \"$$i\"" ; \
	    cat $$i ; \
	  done ;\
	done | ocaml -stdin > $@

_tmp/rss-body.xml.ml:_templates/rss-body.xml.ml.mpp
	${MPPOCAML} < $< > $@

_tmp/blog-atom-contents.xml:posts/*.md.ml.mpp _tmp/gen_post_contents.ml _tmp/common.ml _tmp/atom-body.xml.ml
	make $$(for i in posts/*.md.ml.mpp ; do echo _tmp/$$(basename $$i .md.ml.mpp).bcontents.html.ml ; done)
	for j in _tmp/*.bcontents.html.ml ; do \
	  for i in _tmp/common.ml _tmp/$$(basename $$j .bcontents.html.ml).md.ml $$j _tmp/atom-body.xml.ml ; do \
	    echo "# 1 \"$$i\"" ; \
	    cat $$i ; \
	  done ;\
	done | ocaml -stdin > $@

_tmp/atom-body.xml.ml:_templates/atom-body.xml.ml.mpp
	${MPPOCAML} < $< > $@



################################################################################
# BLOG > escaped contents for rss and atom
################################################################################
_tmp/gen_post_bcontents.ml: Makefile
	echo 'let () = let module M = Post(Printer) in ()' > $@
_tmp/%.bcontents.md:_tmp/%.md.ml _tmp/common.ml _tmp/gen_post_bcontents.ml Makefile
	 for i in _tmp/common.ml $< _tmp/gen_post_bcontents.ml ; do echo "# 1 \"$$i\"" ; cat "$$i" ; done | ocaml -stdin > $@
_tmp/%.bcontents.html:_tmp/%.bcontents.md Makefile
	${OMD} < $< > $@
_tmp/%.bcontents.html.ml:_tmp/%.bcontents.html Makefile
	${MPPOCAML} < $< > $@.tmp
	(printf "module Contents(Printer:Printer) = struct open Printer\n" ; cat $@.tmp ; echo "end") > $@
	rm -f $@.tmp

################################################################################
# BLOG > TAGS
################################################################################
_tmp/tags:../tags/* ../tags/*/* Makefile
	for i in ../tags/* ; do make $$i/index.html done
	touch _tmp/tags

../tags/%/index.html:


../tags/%/index-rss.xml:
	make $$(for i in posts/*.md.ml.mpp ; do echo _tmp/$$(basename $$i .md.ml.mpp).bcontents.html.ml ; done)

../tags/%/index-atom.xml:

_tmp/tagpage.md.ml:_templates/tagpage.md.ml.mpp
	${MPPOCAML} < $< > $@

################################################################################
# UTILS
################################################################################
_tmp/common.ml:common.ml Makefile
	rm -f $@
	cp $< $@
_tmp/tag.ml:tag.ml Makefile
	rm -f $@
	cp $< $@
_tmp/main.ml:_templates/main.ml.mpp Makefile
	${MPPOCAML} < $< > $@


.PRECIOUS : _tmp/common.ml _templates/main.ml.mpp _tmp/main.ml _tmp/%.toc.html.ml _tmp/%.contents.html.ml _tmp/%.md.ml _tmp/%.contents.md _tmp/%.contents.html posts/%.md.ml.mpp _tmp/%.toc.html _tmp/blogposts.list.tmp.md _tmp/%.bcontents.md _tmp/%.bcontents.html

clean:
	rm -f */*~ *~ .#* .DS_Store *.cm[iox] *.o 
	rm -fr _tmp
	mkdir _tmp
	touch _tmp/index

.PHONY:clean

../try-ocaml.js:try-ocaml.js
	cp $< $@
