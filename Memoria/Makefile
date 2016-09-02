PROJECT=memoria
TEX=pdflatex
BIBTEX=bibtex
BUILDTEX=$(TEX) -output-directory build  $(PROJECT).tex

all:
		$(BUILDTEX)
		$(BIBTEX) build/$(PROJECT)
		$(BUILDTEX)
		$(BUILDTEX)
clean-all:
	rm -f *.dvi *.log *.bak *.aux *.bbl *.blg *.idx *.ps *.eps *.pdf *.toc *.out *~

clean:
	rm -f *.log *.bak *.aux *.bbl *.blg *.idx *.toc *.out *~
