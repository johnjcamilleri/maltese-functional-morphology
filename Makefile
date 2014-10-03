all:
	make mt
rts.o:
	gcc -c ./lib/rts.c
mt:
	ghc -O2 -i./lib -i./malti --make ./malti/Main.hs -o morpho_mt
cleanLex:
	ghc -O2 -i./lib --make tools/Clean.hs -o tools/cleanLex
trie:
	gcc -Wall -O3 ./tools/trie.c -o ./tools/trie
.PHONY : clean
clean:
	rm -f ./lib/*.o ./lib/*.hi *~ ./lib/*~ ./malti/*.o ./malti/*.hi ./malti/*~
