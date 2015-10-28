TARGETS=src/settings.cma src/settings.cmxa
.PHONY: clean all test install uninstall

all: $(TARGETS) regression/test.byte

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

install:
	ocamlfind install settings META $(addprefix _build/,$(TARGETS))

uninstall:
	ocamlfind remove settings
