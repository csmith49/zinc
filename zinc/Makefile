ocb_flags = -r -use-ocamlfind -pkgs 'containers, Z3'
ocb = ocamlbuild $(ocb_flags)

.phony: all
all: zinc

zinc:
	$(ocb) zinc.native
	mv zinc.native zinc

.phony: clean
clean:
	rm -rf _build zinc
