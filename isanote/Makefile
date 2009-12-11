
all: modular.pdf

IMAGES=isa2workflow3.pdf

isa2workflow3.pdf: isa2workflow3.svg expplot.pdf moduletree.pdf 
	inkscape -A isa2workflow3.pdf isa2workflow3.svg

modular.pdf: modular.tex isa.bib $(IMAGES)
	pdflatex modular && bibtex modular && pdflatex modular && \
	pdflatex modular
