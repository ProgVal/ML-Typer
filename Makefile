RESULT = typer
SOURCES = \
	parser.mly \
	lexer.mll \
	codemap.ml \
	errors.ml \
	ast.ml \
	typer.ml \
	main.ml

include OCamlMakefile
