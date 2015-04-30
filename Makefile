help:
	@echo targets:    install, clean, hpath.exe

RBIN=C:/files/rbin


install: hpath.exe
	cp hpath.exe $(RBIN)

hpath.exe: HPath.hs
	ghc -O -o hpath.exe --make HPath.hs

clean:
	rm -f hpath.exe *.o *.hi