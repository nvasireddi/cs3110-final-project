.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f MS2Demo.zip
	zip -r MS2Demo.zip . -x@exclude.lst

clean:
	dune clean
	rm -f MS2Demo.zip

doc:
	dune build @doc