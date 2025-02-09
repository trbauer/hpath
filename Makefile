help:
	@echo targets:    install, clean, hpath.exe

INSTALL=C:/bin


install: hpath.exe
	strip hpath.exe
	cp hpath.exe $(INSTALL)

hpath.exe: HPath.hs
	ghc -O -o hpath.exe --make HPath.hs \
		-package ansi-terminal \
		-package containers \
		-package directory \
		-package filepath

clean:
	rm -f hpath.exe *.o *.hi