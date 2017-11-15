ocb_flags = -r -use-ocamlfind -pkgs 'containers, containers.data'
ocb = ocamlbuild $(ocb_flags)

.phony: all
all: zinc

zinc: $(shell find src -type f)
	$(ocb) zinc.native
	mv zinc.native zinc

byte: $(shell find src -type f) clean
	$(ocb) zinc.byte
	mv zinc.byte zinc

student: zinc
	@echo -n "student_01: "
	@./zinc -bm student_01 -disable -annotations -time -v 0 | grep -Po "(?<=Total time: ).*"
	@echo -n "student_02: "
	@./zinc -bm student_02 -disable -annotations -time -v 0 | grep -Po "(?<=Total time: ).*"
	@echo -n "student_03: "
	@./zinc -bm student_03 -disable -annotations -time -v 0 | grep -Po "(?<=Total time: ).*"
	@echo -n "student_04: "
	@./zinc -bm student_04 -disable -annotations -time -v 0 | grep -Po "(?<=Total time: ).*"
	@echo -n "student_05: "
	@./zinc -bm student_05 -disable -annotations -time -v 0 | grep -Po "(?<=Total time: ).*"

.phony: clean
clean:
	rm -rf _build zinc
