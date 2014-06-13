RESULT = typer
SOURCES = \
	utils.ml \
	parser.mly \
	lexer.mll \
	codemap.ml \
	errors.ml \
	ast.ml \
	type.ml \
	unif.ml \
	typer.ml \
	immtyper.ml \
	main.ml

include OCamlMakefile

JS_RESULT = $(RESULT).js

js: $(JS_RESULT)

$(JS_RESULT): byte-code
	js_of_ocaml $(RESULT)

cleanall: clean
	rm -f $(JS_RESULT)
