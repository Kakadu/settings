TARGETS=src/settings.cma src/settings.cmxa
DIST_FILES=$(TARGETS) src/settings.a

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
	ocamlfind install settings META $(addprefix _build/,$(DIST_FILES))

uninstall:
	ocamlfind remove settings
