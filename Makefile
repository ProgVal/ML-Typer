RESULT = typer
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
	typers_simple.ml

TARGET = $(RESULT).cma

all: $(TARGET)

include OCamlMakefile

JS_RESULT = $(RESULT).js
RTPL_RESULT = $(RESULT).rtpl

js: $(JS_RESULT)

rtpl: $(RTPL_RESULT)

$(JS_RESULT): $(RTPL_RESULT)
	js_of_ocaml $^

$(RTPL_RESULT): $(TARGET) rtpl.ml
	ocamlc -o $@ $^

cleanall: clean
	rm -f $(JS_RESULT) $(RTPL_RESULT) rtpl.cm*
