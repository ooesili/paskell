paskell: main.hs
	ghc -Wall $^ -o $@

clean:
	rm -f paskell *.hi *.o
