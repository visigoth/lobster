all : nolviz
	(cd lviz;make install)

nolviz :
	(cd lobster;make install)
	(cd SCD;make install)
	(cd lobster-selinux;make install)
	(cd lobster-xsm;make install)
	(cd lobster-validate;make install)
	(cd shrimp;make install)
	(cd genLobster;make install)

clean :
	(cd lobster;make clean)
	(cd SCD;make clean)
	(cd lobster-selinux;make clean)
	(cd lobster-xsm;make clean)
	(cd lobster-validate;make clean)
	(cd shrimp;make clean)
	(cd lviz;make clean)
	(cd genLobster;make clean)
