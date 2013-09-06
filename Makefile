
all:
	@echo make publish
	@echo make push-all
	@echo make reinit-git
	@echo make new
	@echo make clean

publish:
	./publish_posts.bash

push-all:
	git add -A; git commit -a -m "auto $(date)" ; git push

reinit-git:
	@echo "Is this what you want?"
	@echo rm -fr .git; git init ; git add . ; git commit -m re-init ; git remote add origin git@github.com:pw374/pw374.github.io.git ; git push -u --force origin master

create:new
new:
	bash create_post.bash

clean:
	rm -fr *~ \#* .#* tags _tmp


