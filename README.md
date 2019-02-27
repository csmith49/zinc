# Zinc

Zinc is a sensitivity-aware type-directed synthesis tool for constructing inputs to differentially private mechanisms. It is based on the [DFuzz type system](http://www.cis.upenn.edu/~ahae/papers/dfuzz-popl2013.pdf), and extends Hindley-Milner type-directed synthesis approach of [BigLambda](https://dl.acm.org/citation.cfm?id=2908102).

## Installation and prerequisites

Zinc is written in OCaml and built using `ocamlbuild`. Running `make` from the `zinc` subfolder will call `ocamlbuild` with the appropriate parameters.

Zinc depends on several libraries - `Z3` (which can be found with build instructions for OCaml [here](https://github.com/Z3Prover/z3)), and `containers` (installable through `opam`, documentation [here](https://github.com/c-cube/ocaml-containers)).

## Usage

Still to be determined. Benchmark construction and primitive management will eventually go here.

## Other includes

The subfolder `tools` contains some other random programming projects related to sensitivity analysis.