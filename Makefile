linux:
	gprbuild -p -Pmalef

tests-linux:
	gprbuild -p -Ptests/ada/tests.gpr

docs:
	gnatdoc -bplwPmalef --enable-build
