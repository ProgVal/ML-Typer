RESULT = typer
SOURCES = \
	utils.ml \
	parser.mly \
	lexer.mll \
	codemap.ml \
	errors.ml \
	ast.ml \
	types.ml \
	typers_core.ml \
	typers_simple.ml \
	main.ml

include OCamlMakefile

JS_RESULT = $(RESULT).js

js: $(JS_RESULT)

$(JS_RESULT): byte-code
	js_of_ocaml $(RESULT)

cleanall: clean
	rm -f $(JS_RESULT)
