# Old (SV 1.1)

# all:
# 	make mt
# rts.o:
# 	gcc -c ./lib/rts.c
# mt:
# 	ghc -O2 -i./lib -i./malti --make ./malti/Main.hs -o morpho_mt
# cleanLex:
# 	ghc -O2 -i./lib --make tools/Clean.hs -o tools/cleanLex
# trie:
# 	gcc -Wall -O3 ./tools/trie.c -o ./tools/trie
# .PHONY : clean
# clean:
# 	rm -f ./lib/*.o ./lib/*.hi *~ ./lib/*~ ./malti/*.o ./malti/*.hi ./malti/*~

# New

GHCMAKE = ghc --make
GHCFLAGS =-O2 -XForeignFunctionInterface -i./lib # -rtsopts -i./lib  # -prof -auto-all -caf-all -fforce-recomp
OFILES=./lib/trie_lib.c # ./lib/rts.c
MALTI=malti
MALTI_TMP=$(MALTI)-bin$(EXEEXT)
MALTIP=$(MALTI)$(EXEEXT)

all:
	make malti

malti: ./lib/trie_lib.c ./lib/trie_lib.h # ./lib/rts.c
	$(GHCMAKE) ./malti/Main.hs $(OFILES) $(GHCFLAGS) -i./malti -o $(MALTI_TMP)
	strip $(MALTI_TMP)
	mv $(MALTI_TMP) bin/$(MALTIP)

.PHONY : clean malti
clean:
	rm -f *~ ./lib/*.o ./lib/*.hi ./lib/*~ ./malti/*.o ./malti/*.hi ./malti/*~
