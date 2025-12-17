run:
	make decompress
	make -C UI

full_clean: compressed.tar.gz
	cp compressed.tar.gz /tmp/ # assert that this exists and copy it
	cp Makefile /tmp/ # again make sure this exists and copy it
	cp README.md /tmp/ 
	rm -rf ./* # delete everything
	mv /tmp/compressed.tar.gz ./
	mv /tmp/Makefile ./
	mv /tmp/README.md ./

compress:
	make compressed.tar.gz

compressed.tar.gz: 
	touch /tmp/temp # make sure directory exists for next cmd
	rm -rf /tmp/temp # so we don't drag around other data
	mkdir /tmp/temp
	mv ./* /tmp/temp
	mkdir compressed
	mv /tmp/temp/* compressed
	rm -r /tmp/temp
	cp compressed/Makefile ./
	cp compressed/README.md ./
	tar -czvf compressed.tar.gz ./compressed/
	rm -rf compressed

decompress: UI/app.R
	echo "Decompressing..."

UI/app.R: 
	# decompress depends on this, so decompress is just a wrapper to make PCA.R;
	# this could really be any file that gets decompressed to exist. So 
	# this is a bit janky, but it should also work
	mv Makefile /tmp/
	mv README.md /tmp/
	tar -xzvf compressed.tar.gz -C ./
	mv compressed/* ./
	rm -r compressed
	mv /tmp/Makefile ./
	mv /tmp/README.md ./
