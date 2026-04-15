FROM dhaxe:4.3.7 AS build-stage

RUN mkdir $HAXE_COMPILER_DIR/plugins/ecso/

RUN set -ex ;\
	cd $HAXE_COMPILER_DIR ;\
	opam install ocaml-lsp-server ocamlformat