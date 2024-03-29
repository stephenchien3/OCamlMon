build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec tests/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh