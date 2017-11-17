# Zinc

Zinc is built on OCaml, and should work with almost any version >= 4.02.0. Version 4.06 is not supported, as Z3 hasn't updated bindings to match the removal of the Big_int module.

## Prerequisites

Zinc uses version 1.4 of a drop-in standard library replacement called `containers`. This is easily installable from `opam` through the command `opam install containers`.

Currently, Zinc is not linked against Z3. This makes building substantially easier. Proof obligations are still generated, and must be checked by hand.

Benchmarking is done using a simple Python 3.6 script that uses a YAML parser, `ruamel.yaml`. This package is installable from `pip3` through the command `pip3 install ruamel.yaml`.

## Building

Building is done through `ocamlbuild` (installable from `opam` if necessary). Simply run `make` in the root directory to generate a native executable. For a bytecode executable, run `make byte`.

## Running

Running `./zinc --help` will generate a list of all command-line options. The important ones are as follows:

* `-bm benchmark` tells Zinc to run the benchmark labeled `benchmark`. The labels are of the form `{dataset}_{id}`, where `dataset = {adult, student, compas, performance}`, and `id \in [1, 7]`. This is the only required parameter.
* `-v i` controls the verbosity of the output. Run with `i = 0` unless you want the full output, then run `i = 3`.
* `-annotations` turns off type annotations on output terms, which can make the synthesized programs easier to parse.
* `-obligation` outputs the simplied proof obligation.
* `-time` and `-count` produce some benchmarking values.

## Benchmarking

Navigate to the `./benchmarks` folder and run `make`. Benchmarks are described by YAML files, `bm.yaml`, in the `./benchmarks/params/` folder. The output is a tab-separated `bm.csv` file in `./benchmarks` for every YAML file. Columns are labeled appropriately per benchmark.
