ARG VARIANT=debian
ARG VARIANT_VERSION=11
ARG OCAML_VERION=4.14

FROM ocaml/opam:${VARIANT}-${VARIANT_VERSION}-ocaml-${OCAML_VERION} as builder

USER opam

WORKDIR /src

ADD --chown=opam:opam ocamlformat.opam .
RUN opam pin add -yn ocamlformat . && \
    opam install --deps-only ocamlformat

ADD --chown=opam:opam . .
RUN opam exec -- dune subst && \
    opam exec -- dune build @install

FROM ${VARIANT}:${VARIANT_VERSION}

COPY --from=builder --chown=root:root /src/_build/default/bin/ocamlformat/main.exe /usr/bin/ocamlformat
ENTRYPOINT [ "/usr/bin/ocamlformat" ]