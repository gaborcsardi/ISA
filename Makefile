
VERSION=0.1

all: homepage isa2 eisa

.PHONY: homepage isa2 eisa ExpressionView

isa2: isa2_$(VERSION).tar.gz

eisa: eisa_$(VERSION).tar.gz

ExpressionView: ExpressionView_$(VERSION).tar.gz

isa2_$(VERSION).tar.gz: isa2/DESCRIPTION isa2/LICENCE isa2/NAMESPACE 	\
			isa2/R/*.R isa2/man/*.Rd 			\
			isa2/src/*.c isa2/src/*.h isa2/src/*.cpp 	\
			isa2/inst/CITATION isa2/inst/doc/*.Rnw
	R CMD build --no-vignettes isa2

eisa_$(VERSION).tar.gz:	eisa/DESCRIPTION eisa/NAMESPACE			\
			eisa/R/*.R eisa/man/*.Rd			\
			eisa/inst/CITATION eisa/inst/doc/*.Rnw
	R CMD build --no-vignettes eisa

homepage: homepage/ISA.html           		\
		homepage/ISA_tutorial.html  	\
		homepage/ISA_tutorial.pdf   	\
		homepage/EISA_tutorial.html 	\
	homepage/EISA_tutorial.pdf

homepage/ISA_tutorial.html: isa2/inst/doc/ISA_tutorial.tex
	cd isa2/inst/doc && sweave2html ISA_tutorial
	cp isa2/inst/doc/ISA_tutorial.html homepage/
	cp isa2/inst/doc/graphics/*.png homepage/graphics

homepage/ISA_tutorial.pdf: isa2/inst/doc/ISA_tutorial.tex
	cd isa2/inst/doc && pdflatex ISA_tutorial && pdflatex ISA_tutorial
	cp isa2/inst/doc/ISA_tutorial.pdf homepage/

isa2/inst/doc/ISA_tutorial.tex: isa2/inst/doc/ISA_tutorial.Rnw
	cd isa2/inst/doc && R CMD Sweave ISA_tutorial.Rnw

homepage/EISA_tutorial.html: eisa/inst/doc/EISA_tutorial.tex
	cd eisa/inst/doc && sweave2html EISA_tutorial
	cp eisa/inst/doc/EISA_tutorial.html homepage/
	cp eisa/inst/doc/graphics/*.png homepage/graphics

homepage/EISA_tutorial.pdf: eisa/inst/doc/EISA_tutorial.tex
	cd eisa/inst/doc && pdflatex EISA_tutorial && pdflatex EISA_tutorial
	cp eisa/inst/doc/EISA_tutorial.pdf homepage/

eisa/inst/doc/EISA_tutorial.tex: eisa/inst/doc/EISA_tutorial.Rnw
	cd eisa/inst/doc && R CMD Sweave EISA_tutorial.Rnw
