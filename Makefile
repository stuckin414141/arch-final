main: Makefile
	-dune build

clean:
	-dune clean
	-rm -rf *.lex *.ast *.ua *.mir *.bb
