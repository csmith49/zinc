ocb_flags = -r -use-ocamlfind -pkgs 'containers, Z3' -I "~/Documents/code/iron/_build/src/iron.cma"
ocb = ocamlbuild $(ocb_flags)

.phony: all
all: zinc

zinc: $(shell find src -type f) $(shell find builtins -type f)
	$(ocb) zinc.native
	mv zinc.native zinc

.phony: clean
clean:
	rm -rf _build zinc
