RESULT = typer
SOURCES = \
	codemap.ml \
	errors.ml \
	parser.mly \
	lexer.mll \
	ast.ml \
	typer.ml \
	main.ml

include OCamlMakefile
