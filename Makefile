.PHONY: clean all test

all: src/settings.cma src/settings.cmxa regression/test.byte

src/settings.cma:
	ocamlbuild -use-ocamlfind $@

src/settings.cmxa:
	ocamlbuild -use-ocamlfind $@

regression/test.byte:
	ocamlbuild -use-ocamlfind $@

test: regression/test.byte
	regression/test.sh

clean:
	$(RM) -r _build
