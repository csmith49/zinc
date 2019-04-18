FROM ocaml/opam2:debian-stable

RUN sudo apt-get update && \
    sudo apt-get install -y m4 python3 python3-pip && \
    pip3 install ruamel.yaml && \
    opam update && \
    opam install ocamlbuild ocamlfind containers

COPY zinc /zinc

WORKDIR /zinc

RUN sudo chown -R opam /zinc && eval $(opam env) && make

WORKDIR /zinc/benchmarks