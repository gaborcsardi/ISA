
all: modular.pdf

IMAGES=isa2workflow3.pdf

isa2workflow3.pdf: isa2workflow3.svg EV2.png condplot.pdf expplot.pdf \
		moduletree.pdf profileplot.pdf
	inkscape -A isa2workflow3.pdf isa2workflow3.svg

modular.pdf: modular.tex isa.bib $(IMAGES)
	pdflatex modular && bibtex modular && pdflatex modular && \
	pdflatex modular
