RESULT = rtpl
SOURCES = \
	utils.ml \
	codemap.ml \
	ast.ml \
	parser.mly \
	lexer.mll \
	session.ml \
	errors.ml \
	types.ml \
	typers_core.ml \
	typers_poly.ml \
	typers_simple.ml \
	rtpl.ml

include OCamlMakefile

JS_RESULT = $(RESULT).js

js: $(JS_RESULT)

$(JS_RESULT): $(RESULT)
	js_of_ocaml $^

cleanall: clean
	rm -f $(JS_RESULT)
