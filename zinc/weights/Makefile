exec = ../zinc
tool = python3 yaml_exec.py
params = $(basename $(notdir $(shell find params -type f)))

.phony: all
all: $(foreach p,$(params), $(p).csv)

%.csv: params/%.yaml $(exec)
	$(tool) $< > $@

.phony: clean
clean:
	rm *.csv