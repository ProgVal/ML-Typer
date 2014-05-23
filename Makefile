RESULT = typer
SOURCES = \
	parser.mly \
	lexer.mll \
	ast.ml \
	typer.ml \
	main.ml

include OCamlMakefile
