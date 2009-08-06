
VERSION=0.1

all: homepage isa2 eisa ExpressionView

.PHONY: homepage isa2 eisa ExpressionView

####################################################
## R packages

isa2: isa2_$(VERSION).tar.gz

eisa: eisa_$(VERSION).tar.gz

ExpressionView: ExpressionView_$(VERSION).tar.gz

isa2_$(VERSION).tar.gz: isa2/DESCRIPTION isa2/LICENCE isa2/NAMESPACE 	\
			isa2/R/*.R isa2/man/*.Rd 			\
			isa2/src/*.c 					\
			isa2/inst/CITATION isa2/inst/doc/*.Rnw
	R CMD build --no-vignettes isa2

eisa_$(VERSION).tar.gz:	eisa/DESCRIPTION eisa/NAMESPACE			\
			eisa/R/*.R eisa/man/*.Rd			\
			eisa/inst/CITATION eisa/inst/doc/*.Rnw
	R CMD build --no-vignettes eisa

EVFILES=ExpressionView/src/ch/unil/cbg/ExpressionView/*/*	\
	ExpressionView/src/ExpressionView.mxml

RExpressionView/inst/ExpressionView.swf: $(EVFILES)
	cd ExpressionView/src && \
		mxmlc -compiler.library-path+=../libs/flexlib.swc,../libs/AlivePDF.swc ExpressionView.mxml
	cp ExpressionView/src/$(@F) $(@)

ExpressionView_$(VERSION).tar.gz: RExpressionView/inst/ExpressionView.swf	\
			RExpressionView/DESCRIPTION 				\
			RExpressionView/NAMESPACE				\
			RExpressionView/R/*.R RExpressionView/man/*.Rd 		\
			RExpressionView/src/*.h RExpressionView/src/*.cpp 	\
			RExpressionView/inst/CITATION 				\
			RExpressionView/inst/doc/*.Rnw
	R CMD build --no-vignettes RExpressionView

####################################################
## Homepage

VIGNETTES=homepage/ISA_tutorial.html homepage/ISA_tutorial.pdf   	\
	homepage/EISA_tutorial.html homepage/EISA_tutorial.pdf		\
	homepage/ExpressionView.html homepage/ExpressionView.pdf

homepage: homepage/ISA.html $(VIGNETTES)

####################################################
## Vignettes to homepage

# isa2 

homepage/ISA_tutorial.html: isa2/inst/doc/ISA_tutorial.tex
	cd isa2/inst/doc && sweave2html ISA_tutorial
	cp isa2/inst/doc/ISA_tutorial.html homepage/
	cp isa2/inst/doc/graphics/*.png homepage/graphics

homepage/ISA_tutorial.pdf: isa2/inst/doc/ISA_tutorial.tex
	cd isa2/inst/doc && pdflatex ISA_tutorial && pdflatex ISA_tutorial
	cp isa2/inst/doc/ISA_tutorial.pdf homepage/

isa2/inst/doc/ISA_tutorial.tex: isa2/inst/doc/ISA_tutorial.Rnw
	cd isa2/inst/doc && R CMD Sweave ISA_tutorial.Rnw

# eisa

homepage/EISA_tutorial.html: eisa/inst/doc/EISA_tutorial.tex
	cd eisa/inst/doc && sweave2html EISA_tutorial
	cp eisa/inst/doc/EISA_tutorial.html homepage/
	cp eisa/inst/doc/graphics/*.png homepage/graphics

homepage/EISA_tutorial.pdf: eisa/inst/doc/EISA_tutorial.tex
	cd eisa/inst/doc && pdflatex EISA_tutorial && pdflatex EISA_tutorial
	cp eisa/inst/doc/EISA_tutorial.pdf homepage/

eisa/inst/doc/EISA_tutorial.tex: eisa/inst/doc/EISA_tutorial.Rnw
	cd eisa/inst/doc && R CMD Sweave EISA_tutorial.Rnw

# ExpressionView

homepage/ExpressionView.html: RExpressionView/inst/doc/ExpressionView.tex
	cd RExpressionView/inst/doc && sweave2html ExpressionView
	cp RExpressionView/inst/doc/ExpressionView.html homepage/
#	cp RExpressionView/inst/doc/graphics/*.png homepage/graphics

homepage/ExpressionView.pdf: RExpressionView/inst/doc/ExpressionView.tex
	cd RExpressionView/inst/doc && pdflatex ExpressionView && pdflatex ExpressionView
	cp RExpressionView/inst/doc/ExpressionView.pdf homepage/

RExpressionView/inst/doc/ExpressionView.tex: RExpressionView/inst/doc/ExpressionView.Rnw
	cd RExpressionView/inst/doc && R CMD Sweave ExpressionView.Rnw
