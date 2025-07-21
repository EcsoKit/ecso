FROM haxe:4.3.6-gllr AS build-stage

RUN mkdir $HAXE_COMPILER_DIR/plugins/ecso/

RUN set -ex ;\
	cd $HAXE_COMPILER_DIR ;\
	opam install ocaml-lsp-server ocamlformat