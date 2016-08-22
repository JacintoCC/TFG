all: memoria.pdf

Rnw := $(wildcard capitulos/*.Rnw)
Prefacios := $(wildcard prefacios/*.tex)
Portadas := $(wildcard portada/*.tex)
Apendices := $(wildcard apendices/*.tex)

memoria.pdf: $(Apendices) $(Prefacios) $(Portadas) memoria.tex
	pdflatex -output-directory build memoria.tex && pdflatex -output-directory build memoria.tex
