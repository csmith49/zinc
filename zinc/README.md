# Zinc

Synthesis for differential privacy using a linear dependent type system.

## Usage

Run `./zinc --help` to see a list of command-line arguments. Benchmarks are compiled along with the source in the `./src/tasks/dataset.ml` file, and are selected with the `-bm` flag, like so:

```./zinc -bm adult_01```

## Building

Zinc is implemented in `OCaml 4.06.1`, and uses `ocamlbuild 0.12.0` and `ocamlfind 1.8.0` to compile the project. A handy makefile does all the hard work. Simply run `make` in this directory.

### Dependencies

Zinc is dependent on the most recent version of containers (2.1), available through `opam`.

## Benchmarking

Navigate to `./benchmarks` and run `make`. The makefile will call `yaml_exec.py` on several paramter files stored in `./benchmarks/params`, which will populate `.csv` files containing all the data necessary to reconstruct the figures and tables in the paper.

The Python script is dependent on several modules shipped with most builds of Python 3, and the module `ruamel.yaml`, available from `pip` and `brew`.
