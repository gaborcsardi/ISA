
VERSION=0.2.1
EISAVERSION=0.2.2

all: isa2 eisa

all-final: homepage isa2-final eisa-final ExpressionView

.PHONY: homepage isa2 eisa ExpressionView clean alivepdf vignettes manuals \
	isa2-final eisa-final ExpressionView-final

clean: 
	bzr clean-tree --force
	bzr clean-tree --ignored --force

####################################################
## R packages

# isa2

isa2-final: isa2_$(VERSION).tar.gz
isa: isa2_$(VERSION)-nv.tar.gz

ISA2FILES = isa2/DESCRIPTION isa2/LICENCE isa2/NAMESPACE isa2/R/*.R isa2/man/*.Rd \
	isa2/src/*.c isa2/inst/CITATION 

isa2_$(VERSION).tar.gz: $(ISA2FILES)
	R_LIBS=~/.R/library R CMD build isa2

isa2_$(VERSION)-nv.tar.gz: $(ISA2FILES)
	R_LIBS=~/.R/library R CMD build --no-vignettes isa2

# eisa

EISAFILES = eisa/DESCRIPTION eisa/NAMESPACE eisa/R/*.R eisa/man/*.Rd \
	eisa/inst/CITATION eisa/data/*.rda

eisa-final: eisa_$(EISAVERSION).tar.gz
eisa: eisa_$(EISAVERSION)-nv.tar.gz

eisa_$(EISAVERSION).tar.gz: $(EISAFILES)
	R_LIBS=~/.R/library R CMD build eisa
eisa_$(EISAVERSION)-nv.tar.gz: $(EISAFILES)
	R_LIBS=~/.R/library R CMD build --no-vignettes eisa

# ExpressionView

ExpressionView: ExpressionView_$(VERSION).tar.gz

EVFILES=ExpressionView/src/ch/unil/cbg/ExpressionView/*/*	\
	ExpressionView/src/ExpressionView.mxml

REVFILES=RExpressionView/inst/ExpressionView.swf	\
	RExpressionView/inst/ExpressionView.html		\
	RExpressionView/DESCRIPTION 				\
	RExpressionView/NAMESPACE				\
	RExpressionView/R/*.R RExpressionView/man/*.Rd 		\
	RExpressionView/src/*.h RExpressionView/src/*.cpp

REVVIGNETTES=RExpressionView/inst/doc/ExpressionView.tutorial.Rnw \
	RExpressionView/inst/doc/ExpressionView.tutorial.pdf \
	RExpressionView/inst/doc/ExpressionView.ordering.Rnw \
	RExpressionView/inst/doc/ExpressionView.ordering.pdf \
	RExpressionView/inst/doc/ExpressionView.xmldescription.pdf

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
	R_LIBS=~/.R/library R CMD build RExpressionView

vignettes/ExpressionView_tutorial.tex: $(REVFILES) vignettes/ExpressionView_tutorial.Rnw
	R CMD build --no-vignettes isa2
	R CMD build --no-vignettes eisa
	R CMD build --no-vignettes RExpressionView
	R CMD INSTALL -l /tmp/ isa2_$(VERSION).tar.gz
	R CMD INSTALL -l /tmp/ eisa_$(EISAVERSION).tar.gz
	R CMD INSTALL -l /tmp/ ExpressionView_$(VERSION).tar.gz
	cd vignettes && R_LIBS=/tmp/ R CMD Sweave ExpressionView_tutorial.Rnw || rm ExpressionView_tutorial.tex

RExpressionView/inst/doc/ExpressionView_tutorial.Rnw: vignettes/ExpressionView_tutorial.Rnw
	cp $< $@

RExpressionView/inst/doc/ExpressionView_tutorial.pdf: vignettes/ExpressionView_tutorial.pdf
	cp $< $@

vignettes/ExpressionView_tutorial.pdf: vignettes/ExpressionView_tutorial.tex
	cd vignettes && pdflatex ExpressionView_tutorial.tex && \
		pdflatex ExpressionView_tutorial.tex
	cp vignettes/ExpressionView_tutorial.pdf RExpressionView/inst/doc/

############################################
## Homepage

SWEAVE2HTML = htlatex
SWEAVE2HTMLOPTIONS = style,xhtml,graphics-,charset=utf-8 " -cunihtf -utf8" -cvalidate
REMOVEHTML = sed '/<div class="maketitle">/,/<\/body>/!d' | sed '/<\/body>/d'
REWRITEIMG = sed 's/src="\([^"]*\)"/src="htmlets\/graphics\/\1"/g'

homepage: vignettes manuals

vignettes: homepage/ISA_tutorial.html homepage/ISA_tutorial.pdf \
	homepage/ISA_parallel.html homepage/ISA_parallel.pdf \
	homepage/EISA_tutorial.html homepage/EISA_tutorial.pdf \
	homepage/EISA_module_trees.html homepage/EISA_module_trees.pdf \
	homepage/EISA_biclust.html homepage/EISA_biclust.pdf \
	homepage/tissues.html homepage/tissues.pdf \
	homepage/ISA_internals.html homepage/ISA_internals.pdf \
	vignettes/style.cfg

homepage/ISA_tutorial.html: isa2/inst/doc/ISA_tutorial.tex
	cp vignettes/style.cfg isa2/inst/doc/
	cd isa2/inst/doc && $(SWEAVE2HTML) ISA_tutorial $(SWEAVE2HTMLOPTIONS)
	cat isa2/inst/doc/ISA_tutorial.html | $(REMOVEHTML) | $(REWRITEIMG) >homepage/ISA_tutorial.html
	cp isa2/inst/doc/ISA_tutorial.css homepage/
	cp isa2/inst/doc/ISA_tutorial*.png homepage/
	cp isa2/inst/doc/*.png homepage/
homepage/ISA_tutorial.pdf: isa2/inst/doc/ISA_tutorial.pdf
	cp $< $@
isa2/inst/doc/ISA_tutorial.pdf: isa2/inst/doc/ISA_tutorial.tex
	cd isa2/inst/doc/ && pdflatex ISA_tutorial.tex && \
		pdflatex ISA_tutorial && \
		pdflatex ISA_tutorial
isa2/inst/doc/ISA_tutorial.tex: isa2/inst/doc/ISA_tutorial.Rnw
	cd isa2/inst/doc/ && R CMD Sweave ISA_tutorial.Rnw

homepage/ISA_parallel.html: isa2/inst/doc/ISA_parallel.tex
	cp vignettes/style.cfg isa2/inst/doc/
	cd isa2/inst/doc && $(SWEAVE2HTML) ISA_parallel $(SWEAVE2HTMLOPTIONS)
	cat isa2/inst/doc/ISA_parallel.html | $(REMOVEHTML) | $(REWRITEIMG) >homepage/ISA_parallel.html
	cp isa2/inst/doc/ISA_parallel.css homepage/
#	cp isa2/inst/doc/ISA_parallel*.png homepage/
#	cp isa2/inst/doc/*.png homepage/graphics/
homepage/ISA_parallel.pdf: isa2/inst/doc/ISA_parallel.pdf
	cp $< $@
isa2/inst/doc/ISA_parallel.pdf: isa2/inst/doc/ISA_parallel.tex
	cd isa2/inst/doc/ && pdflatex ISA_parallel.tex
isa2/inst/doc/ISA_parallel.tex: isa2/inst/doc/ISA_parallel.Rnw
	cd isa2/inst/doc/ && R CMD Sweave ISA_parallel.Rnw

homepage/EISA_tutorial.html: eisa/inst/doc/EISA_tutorial.tex
	cp vignettes/style.cfg eisa/inst/doc/
	cd eisa/inst/doc/ && pdflatex EISA_tutorial.tex && \
		bibtex EISA_tutorial && pdflatex EISA_tutorial
	cd eisa/inst/doc && $(SWEAVE2HTML) EISA_tutorial $(SWEAVE2HTMLOPTIONS)
	cat eisa/inst/doc/EISA_tutorial.html | $(REMOVEHTML) | $(REWRITEIMG) >homepage/EISA_tutorial.html
	cp eisa/inst/doc/EISA_tutorial.css homepage/
	cp eisa/inst/doc/EISA_tutorial*.png homepage/
	cp eisa/inst/doc/*.png homepage/
homepage/EISA_tutorial.pdf: eisa/inst/doc/EISA_tutorial.pdf
	cp $< $@
eisa/inst/doc/EISA_tutorial.pdf: eisa/inst/doc/EISA_tutorial.tex
	cd eisa/inst/doc/ && pdflatex EISA_tutorial.tex
eisa/inst/doc/EISA_tutorial.tex: eisa/inst/doc/EISA_tutorial.Rnw
	cd eisa/inst/doc/ && R CMD Sweave EISA_tutorial.Rnw

homepage/EISA_module_trees.html: vignettes/EISA_module_trees.tex
	cd vignettes && pdflatex EISA_module_trees.tex && \
		bibtex EISA_module_trees && pdflatex EISA_module_trees
	cd vignettes && $(SWEAVE2HTML) EISA_module_trees $(SWEAVE2HTMLOPTIONS)
	cat vignettes/EISA_module_trees.html | $(REMOVEHTML) | $(REWRITEIMG) >homepage/EISA_module_trees.html
	cp vignettes/EISA_module_trees.css homepage/
	cp vignettes/EISA_module_trees*.png homepage/
	cp vignettes/*.png homepage/
homepage/EISA_module_trees.pdf: vignettes/EISA_module_trees.pdf
	cp $< $@
vignettes/EISA_module_trees.pdf: vignettes/EISA_module_trees.tex
	cd vignettes/ && pdflatex EISA_module_trees.tex
vignettes/EISA_module_trees.tex: vignettes/EISA_module_trees.Rnw
	cd vignettes/ && R CMD Sweave EISA_module_trees.Rnw

homepage/tissues.html: vignettes/tissues.tex
	cd vignettes && pdflatex tissues.tex && \
		bibtex tissues && pdflatex tissues
	cd vignettes && $(SWEAVE2HTML) tissues $(SWEAVE2HTMLOPTIONS)
	cat vignettes/tissues.html | $(REMOVEHTML) | $(REWRITEIMG) >homepage/tissues.html
	cp vignettes/tissues.css homepage/
	cp vignettes/tissues*.png homepage/
	cp vignettes/*.png homepage/
homepage/tissues.pdf: vignettes/tissues.pdf
	cp $< $@
vignettes/tissues.pdf: vignettes/tissues.tex
	cd vignettes/ && pdflatex tissues.tex
vignettes/tissues.tex: vignettes/tissues.Rnw
	cd vignettes/ && R CMD Sweave tissues.Rnw

homepage/ISA_internals.html: vignettes/ISA_internals.tex
	cd vignettes && pdflatex ISA_internals.tex && \
		bibtex ISA_internals && pdflatex ISA_internals
	cd vignettes && $(SWEAVE2HTML) ISA_internals $(SWEAVE2HTMLOPTIONS)
	cat vignettes/ISA_internals.html | $(REMOVEHTML) | $(REWRITEIMG) >homepage/ISA_internals.html
	cp vignettes/ISA_internals.css homepage/
#	cp vignettes/ISA_internals*.png homepage/
#	cp vignettes/*.png homepage/
homepage/ISA_internals.pdf: vignettes/ISA_internals.pdf
	cp $< $@
vignettes/ISA_internals.pdf: vignettes/ISA_internals.tex
	cd vignettes/ && pdflatex ISA_internals.tex
vignettes/ISA_internals.tex: vignettes/ISA_internals.Rnw
	cd vignettes/ && R CMD Sweave ISA_internals.Rnw

homepage/EISA_biclust.html: eisa/inst/doc/EISA_biclust.tex
	cp vignettes/style.cfg eisa/inst/doc/
	cd eisa/inst/doc/ && pdflatex EISA_biclust.tex && \
		bibtex EISA_biclust && pdflatex EISA_biclust
	cd eisa/inst/doc && $(SWEAVE2HTML) EISA_biclust $(SWEAVE2HTMLOPTIONS)
	cat eisa/inst/doc/EISA_biclust.html | $(REMOVEHTML) | $(REWRITEIMG) >homepage/EISA_biclust.html
	cp eisa/inst/doc/EISA_biclust.css homepage/
	cp eisa/inst/doc/EISA_biclust*.png homepage/
	cp eisa/inst/doc/*.png homepage/
homepage/EISA_biclust.pdf: eisa/inst/doc/EISA_biclust.pdf
	cp $< $@
eisa/inst/doc/EISA_biclust.pdf: eisa/inst/doc/EISA_biclust.tex
	cd eisa/inst/doc/ && pdflatex EISA_biclust.tex
eisa/inst/doc/EISA_biclust.tex: eisa/inst/doc/EISA_biclust.Rnw
	cd eisa/inst/doc/ && R CMD Sweave EISA_biclust.Rnw

manuals: homepage/manuals/isa2/.stamp homepage/manuals/eisa/.stamp \
	homepage/manuals/ExpressionView/.stamp \
	homepage/manuals/isa2.pdf homepage/manuals/eisa.pdf \
	homepage/manuals/ExpressionView.pdf

homepage/manuals/isa2/.stamp: isa2_$(VERSION).tar.gz
	scripts/manual.sh isa2 $(VERSION)

homepage/manuals/isa2.pdf: isa2_$(VERSION).tar.gz
	rm -f isa2.pdf && R CMD Rd2dvi --pdf --no-preview isa2 && \
	mv isa2.pdf homepage/manuals/

homepage/manuals/eisa/.stamp: eisa_$(EISAVERSION).tar.gz
	scripts/manual.sh eisa $(EISAVERSION)

homepage/manuals/eisa.pdf: eisa_$(EISAVERSION).tar.gz
	rm -f eisa.pdf && R CMD Rd2dvi --pdf --no-preview eisa && \
	mv eisa.pdf homepage/manuals/

homepage/manuals/ExpressionView/.stamp: ExpressionView_$(VERSION).tar.gz
	scripts/manual.sh ExpressionView $(VERSION)

homepage/manuals/ExpressionView.pdf: ExpressionView_$(VERSION).tar.gz
	rm -rf RExpressionView.pdf && \
	R CMD Rd2dvi --pdf --no-preview RExpressionView && \
	mv RExpressionView.pdf homepage/manuals/ExpressionView.pdf
