linux:
	gprbuild -p -Pmalef

docs:
	gnatdoc -bplwPmalef --enable-build
