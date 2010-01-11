
all: modular.pdf isa2workflow3.eps

IMAGES=isa2workflow3.pdf

isa2workflow3.pdf: isa2workflow3.svg expplot.pdf moduletree.pdf 
	inkscape -A isa2workflow3.pdf isa2workflow3.svg

isa2workflow3.eps: isa2workflow3.pdf
	pdftops -eps isa2workflow3.pdf

modular.pdf: modular.tex isa.bib $(IMAGES)
	pdflatex modular && bibtex modular && pdflatex modular && \
	pdflatex modular
