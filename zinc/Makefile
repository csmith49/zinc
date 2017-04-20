ocb_flags = -r -use-ocamlfind -pkgs 'containers' -I src
ocb = ocamlbuild $(ocb_flags)

.phony: all
all: zinc

zinc:
	$(ocb) zinc.native
	mv zinc.native zinc

.phony: clean
clean:
	$(ocb) -clean
