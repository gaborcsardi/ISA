
VERSION=0.1

all: homepage isa2 eisa ExpressionView

.PHONY: homepage isa2 eisa ExpressionView clean alivepdf vignettes

clean: 
	bzr clean-tree --force
	bzr clean-tree --ignored --force

####################################################
## R packages

# isa2

isa2: isa2_$(VERSION).tar.gz

ISA2FILES = isa2/DESCRIPTION isa2/LICENCE isa2/NAMESPACE isa2/R/*.R isa2/man/*.Rd \
	isa2/src/*.c isa2/inst/CITATION 

ISA2VIGNETTES = isa2/inst/doc/ISA_tutorial.vignette \
	isa2/inst/doc/ISA_tutorial.pdf isa2/inst/doc/ISA_parallel.vignette \
	isa2/inst/doc/ISA_parallel.pdf

isa2_$(VERSION).tar.gz: $(ISA2FILES) $(ISA2VIGNETTES)
	R CMD build isa2

vignettes/ISA_tutorial.tex: $(ISA2FILES) vignettes/ISA_tutorial.Rnw
	R CMD build --no-vignettes isa2
	R CMD INSTALL -l /tmp/ isa2_$(VERSION).tar.gz
	cd vignettes && R_LIBS=/tmp/ R CMD Sweave ISA_tutorial.Rnw || rm ISA_tutorial.tex

isa2/inst/doc/ISA_tutorial.vignette: vignettes/ISA_tutorial.Rnw
	cp $< $@

isa2/inst/doc/ISA_tutorial.pdf: vignettes/ISA_tutorial.pdf
	cp $< $@

vignettes/ISA_tutorial.pdf: vignettes/ISA_tutorial.tex
	cd vignettes && pdflatex ISA_tutorial.tex && pdflatex ISA_tutorial.tex && \
		pdflatex ISA_tutorial.tex

vignettes/ISA_parallel.tex: $(ISA2FILES) vignettes/ISA_parallel.Rnw
	R CMD build --no-vignettes isa2
	R CMD INSTALL -l /tmp/ isa2_$(VERSION).tar.gz
	cd vignettes && R_LIBS=/tmp/ R CMD Sweave ISA_parallel.Rnw || rm ISA_parallel.tex

isa2/inst/doc/ISA_parallel.vignette: vignettes/ISA_parallel.Rnw
	cp $< $@

isa2/inst/doc/ISA_parallel.pdf: vignettes/ISA_parallel.pdf
	cp $< $@

vignettes/ISA_parallel.pdf: vignettes/ISA_parallel.tex
	cd vignettes && pdflatex ISA_parallel.tex && pdflatex ISA_parallel.tex \
		&& pdflatex ISA_parallel.tex

# eisa

EISAFILES = eisa/DESCRIPTION eisa/NAMESPACE eisa/R/*.R eisa/man/*.Rd \
	eisa/inst/CITATION

EISAVIGNETTES = eisa/inst/doc/EISA_tutorial.vignette eisa/inst/doc/EISA_tutorial.pdf

eisa: eisa_$(VERSION).tar.gz

eisa_$(VERSION).tar.gz:	$(EISAFILES) $(EISAVIGNETTES)
	R CMD build eisa

vignettes/EISA_tutorial.tex: $(EISAFILES) vignettes/EISA_tutorial.Rnw
	R CMD build --no-vignettes isa2
	R CMD build --no-vignettes eisa
	R CMD INSTALL -l /tmp/ isa2_$(VERSION).tar.gz
	R CMD INSTALL -l /tmp/ eisa_$(VERSION).tar.gz
	cd vignettes && R_LIBS=/tmp/ R CMD Sweave EISA_tutorial.Rnw || rm EISA_tutorial.tex

eisa/inst/doc/EISA_tutorial.vignette: vignettes/EISA_tutorial.Rnw
	cp $< $@

eisa/inst/doc/EISA_tutorial.pdf: vignettes/EISA_tutorial.pdf
	cp $< $@

vignettes/EISA_tutorial.pdf: vignettes/EISA_tutorial.tex
	cd vignettes && pdflatex EISA_tutorial.tex && pdflatex EISA_tutorial.tex && \
		pdflatex EISA_tutorial.tex

# ExpressionView

ExpressionView: ExpressionView_$(VERSION).tar.gz

EVFILES=ExpressionView/src/ch/unil/cbg/ExpressionView/*/*	\
	ExpressionView/src/ExpressionView.mxml

REVFILES=RExpressionView/inst/ExpressionView.swf	\
	RExpressionView/inst/ExpressionView.html		\
	RExpressionView/DESCRIPTION 				\
	RExpressionView/NAMESPACE				\
	RExpressionView/R/*.R RExpressionView/man/*.Rd 		\
	RExpressionView/src/*.h RExpressionView/src/*.cpp 	\
	RExpressionView/inst/CITATION

REVVIGNETTES=RExpressionView/inst/doc/ExpressionView_tutorial.vignette \
	RExpressionView/inst/doc/ExpressionView_tutorial.pdf

ALIVEPDFFILES = $(shell find ExpressionView/src/org -name "*.as" -print)
ALIVEPDFCLASSES = $(subst ExpressionView.src.,,$(subst /,.,$(basename $(ALIVEPDFFILES))))

alivepdf: ExpressionView/libs/AlivePDF.swc

ExpressionView/libs/AlivePDF.swc: $(ALIVEPDFFILES)
	cd ExpressionView/src/
	compc -source-path ExpressionView/src -include-classes $(ALIVEPDFCLASSES) -output $@

RExpressionView/inst/ExpressionView.swf: $(EVFILES) ExpressionView/libs/AlivePDF.swc
	cd ExpressionView/src && \
		mxmlc -compiler.library-path+=../libs/AlivePDF.swc \
			-use-network=false ExpressionView.mxml \
			-target-player=10
	cp ExpressionView/src/$(@F) $(@)

ExpressionView_$(VERSION).tar.gz: $(REVFILES) $(REVVIGNETTES)
	R CMD build RExpressionView

vignettes/ExpressionView_tutorial.tex: $(REVFILES) vignettes/ExpressionView_tutorial.Rnw
	R CMD build --no-vignettes isa2
	R CMD build --no-vignettes eisa
	R CMD build --no-vignettes RExpressionView
	R CMD INSTALL -l /tmp/ isa2_$(VERSION).tar.gz
	R CMD INSTALL -l /tmp/ eisa_$(VERSION).tar.gz
	R CMD INSTALL -l /tmp/ ExpressionView_$(VERSION).tar.gz
	cd vignettes && R_LIBS=/tmp/ R CMD Sweave ExpressionView_tutorial.Rnw || rm ExpressionView_tutorial.tex

RExpressionView/inst/doc/ExpressionView_tutorial.vignette: vignettes/ExpressionView_tutorial.Rnw
	cp $< $@

RExpressionView/inst/doc/ExpressionView_tutorial.pdf: vignettes/ExpressionView_tutorial.pdf
	cp $< $@

vignettes/ExpressionView_tutorial.pdf: vignettes/ExpressionView_tutorial.tex
	cd vignettes && pdflatex ExpressionView_tutorial.tex && pdflatex ExpressionView_tutorial.tex && \
		pdflatex ExpressionView_tutorial.tex
	cp vignettes/ExpressionView_tutorial.pdf RExpressionView/inst/doc/

############################################
## Homepage

SWEAVE2HTML = htlatex
SWEAVE2HTMLOPTIONS = style,xhtml,graphics-,charset=utf-8 " -cunihtf -utf8" -cvalidate

homepage: vignettes homepage/.stamp

homepage/.stamp: homepage/*.in
	cd homepage && ./generate.py && touch .stamp

vignettes: homepage/ISA_tutorial.html homepage/ISA_tutorial.pdf \
	homepage/ISA_parallel.html homepage/ISA_parallel.pdf \
	homepage/EISA_tutorial.html homepage/EISA_tutorial.pdf \
	homepage/ExpressionView_tutorial.html homepage/ExpressionView_tutorial.pdf \
	vignettes/style.cfg

homepage/ISA_tutorial.html: vignettes/ISA_tutorial.tex
	cd vignettes && $(SWEAVE2HTML) ISA_tutorial $(SWEAVE2HTMLOPTIONS)
	cp vignettes/ISA_tutorial.html homepage
	cp vignettes/ISA_tutorial.css homepage
	cp vignettes/ISA_tutorial*.png homepage/
	cp vignettes/graphics/*.png homepage/graphics/

homepage/ISA_tutorial.pdf: vignettes/ISA_tutorial.pdf
	cp $< $@

homepage/ISA_parallel.html: vignettes/ISA_parallel.tex
	cd vignettes && $(SWEAVE2HTML) ISA_parallel $(SWEAVE2HTMLOPTIONS)
	cp vignettes/ISA_parallel.html homepage
	cp vignettes/ISA_parallel.css homepage
#	cp vignettes/ISA_parallel*.png homepage/
	cp vignettes/graphics/*.png homepage/graphics/

homepage/ISA_parallel.pdf: vignettes/ISA_parallel.pdf
	cp $< $@

homepage/EISA_tutorial.html: vignettes/EISA_tutorial.tex
	cd vignettes && $(SWEAVE2HTML) EISA_tutorial $(SWEAVE2HTMLOPTIONS)
	cp vignettes/EISA_tutorial.html homepage
	cp vignettes/EISA_tutorial.css homepage
	cp vignettes/EISA_tutorial*.png homepage/
	cp vignettes/graphics/*.png homepage/graphics/

homepage/EISA_tutorial.pdf: vignettes/EISA_tutorial.pdf
	cp $< $@

homepage/ExpressionView_tutorial.html: vignettes/ExpressionView_tutorial.tex
	cd vignettes && $(SWEAVE2HTML) ExpressionView_tutorial $(SWEAVE2HTMLOPTIONS)
	cp vignettes/ExpressionView_tutorial.html homepage
	cp vignettes/ExpressionView_tutorial.css homepage
#	cp vignettes/ExpressionView*.png homepage/
	cp vignettes/graphics/*.png homepage/graphics/

homepage/ExpressionView_tutorial.pdf: vignettes/ExpressionView_tutorial.pdf
	cp $< $@
