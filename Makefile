
ISAVERSION=$(shell grep Version isa2/DESCRIPTION | cut -f2 -d" ")
EISAVERSION=$(shell grep Version eisa/DESCRIPTION | cut -f2 -d" ")
EVVERSION=$(shell grep Version RExpressionView/DESCRIPTION | cut -f2 -d" ")

all: isa2 eisa ExpressionView

all-final: isa2-final eisa-final ExpressionView-final

.PHONY: homepage isa2 eisa ExpressionView clean vignettes manuals \
	isa2-final eisa-final ExpressionView-final

clean: 
	bzr clean-tree --force
	bzr clean-tree --ignored --force

%.o: %.c

####################################################
## R packages

R=/home/gabor/software/R-2.11.0/bin/R

# isa2

isa2-final: isa2_$(ISAVERSION).tar.gz
isa2: isa2_$(ISAVERSION)-nv.tar.gz

ISA2FILES = isa2/DESCRIPTION isa2/LICENCE isa2/NAMESPACE \
	isa2/R/AllGenerics.R isa2/R/insilico.R isa2/R/option.R \
	isa2/R/status.R isa2/R/zzz.R isa2/R/biclust.R isa2/R/isa3.R \
	isa2/R/robustness.R isa2/R/viz.R \
	isa2/man/aaa-package.Rd isa2/man/isa.in.silico.Rd \
	isa2/man/isa.option.Rd isa2/man/isa.unique.Rd \
	isa2/man/generate.seeds.Rd isa2/man/isa.iterate.Rd \
	isa2/man/isa.Rd isa2/man/plot.modules.Rd isa2/man/isa.biclust.Rd \
	isa2/man/isa.normalize.Rd isa2/man/isa.sweep.Rd \
	isa2/man/robustness.Rd \
	isa2/src/isa.c isa2/inst/CITATION

ISAVIG=isa2/inst/doc/ISA_parallel.pdf isa2/inst/doc/ISA_parallel.vignette \
	isa2/inst/doc/ISA_tutorial.pdf isa2/inst/doc/ISA_tutorial.vignette

isa2_$(ISAVERSION).tar.gz: $(ISA2FILES) $(ISAVIG) isa2_$(ISAVERSION)-nv.tar.gz
	$(R) CMD INSTALL isa2_$(ISAVERSION)-nv.tar.gz
	$(R) CMD build isa2

isa2_$(ISAVERSION)-nv.tar.gz: $(ISA2FILES)
	$(R) CMD build --no-vignettes isa2
	mv isa2_$(ISAVERSION).tar.gz isa2_$(ISAVERSION)-nv.tar.gz

isa2/inst/doc/ISA_tutorial.vignette: vignettes/ISA_tutorial.Rnw
	cp $< $@

isa2/inst/doc/ISA_tutorial.pdf: vignettes/ISA_tutorial.pdf
	cp $< $@

vignettes/ISA_tutorial.pdf: vignettes/ISA_tutorial.tex
	cd vignettes && pdflatex ISA_tutorial.tex && \
		pdflatex ISA_tutorial.tex && \
		qpdf ISA_tutorial.pdf ISA_tutorial2.pdf && \
		mv ISA_tutorial2.pdf ISA_tutorial.pdf

vignettes/ISA_tutorial.tex: vignettes/ISA_tutorial.Rnw isa2_$(ISAVERSION)-nv.tar.gz
	$(R) CMD INSTALL isa2_$(ISAVERSION)-nv.tar.gz
	cd vignettes && $(R) CMD Sweave ISA_tutorial.Rnw || rm ISA_tutorial.tex

isa2/inst/doc/ISA_parallel.vignette: vignettes/ISA_parallel.Rnw
	cp $< $@

isa2/inst/doc/ISA_parallel.pdf: vignettes/ISA_parallel.pdf
	cp $< $@

vignettes/ISA_parallel.pdf: vignettes/ISA_parallel.tex
	cd vignettes && pdflatex ISA_parallel.tex && \
		qpdf ISA_parallel.pdf ISA_parallel2.pdf && \
		mv ISA_parallel2.pdf ISA_parallel.pdf

vignettes/ISA_parallel.tex: vignettes/ISA_parallel.Rnw isa2_$(ISAVERSION)-nv.tar.gz
	$(R) CMD INSTALL isa2_$(ISAVERSION)-nv.tar.gz
	cd vignettes && $(R) CMD Sweave ISA_parallel.Rnw || rm ISA_parallel.tex

# eisa

EISAFILES = eisa/DESCRIPTION eisa/NAMESPACE eisa/R/*.R eisa/man/*.Rd \
	eisa/inst/CITATION eisa/data/*.rda eisa/inst/doc/*.Rnw \
	eisa/inst/doc/EISA.bib eisa/inst/doc/*.png

EISAVIG=eisa/inst/doc/ISA_internals.pdf eisa/inst/doc/ISA_internals.vignette \
	eisa/inst/doc/tissues.pdf eisa/inst/doc/tissues.vignette

eisa-final: eisa_$(EISAVERSION).tar.gz
eisa: eisa_$(EISAVERSION)-nv.tar.gz

eisa_$(EISAVERSION).tar.gz: $(EISAFILES) $(EISAVIG)
	$(R) CMD build eisa

eisa_$(EISAVERSION)-nv.tar.gz: $(EISAFILES)
	$(R) CMD build --no-vignettes eisa
	mv eisa_$(EISAVERSION).tar.gz eisa_$(EISAVERSION)-nv.tar.gz

eisa/inst/doc/ISA_internals.vignette: vignettes/ISA_internals.Rnw
	cp $< $@

eisa/inst/doc/ISA_internals.pdf: vignettes/ISA_internals.pdf
	cp $< $@

vignettes/ISA_internals.pdf: vignettes/ISA_internals.tex
	cd vignettes && pdflatex ISA_internals && bibtex ISA_internals && \
		pdflatex ISA_internals && pdflatex ISA_internals && \
		qpdf ISA_internals.pdf ISA_internals2.pdf && \
		cp ISA_internals2.pdf ISA_internals.pdf

vignettes/ISA_internals.tex: vignettes/ISA_internals.Rnw \
		eisa_$(EISAVERSION)-nv.tar.gz isa2_$(ISAVERSION)-nv.tar.gz
	$(R) CMD INSTALL isa2_$(ISAVERSION)-nv.tar.gz
	$(R) CMD INSTALL eisa_$(EISAVERSION)-nv.tar.gz
	cd vignettes && $(R) CMD Sweave ISA_internals.Rnw || \
		rm ISA_internals.tex

eisa/inst/doc/tissues.vignette: vignettes/tissues.Rnw
	cp $< $@

eisa/inst/doc/tissues.pdf: vignettes/tissues.pdf
	cp $< $@

vignettes/tissues.pdf: vignettes/tissues.tex
	cd vignettes && pdflatex tissues && bibtex tissues && \
		pdflatex tissues && pdflatex tissues

vignettes/tissues.tex: vignettes/tissues.Rnw \
		eisa_$(EISAVERSION)-nv.tar.gz isa2_$(ISAVERSION)-nv.tar.gz
	$(R) CMD INSTALL isa2_$(ISAVERSION)-nv.tar.gz
	$(R) CMD INSTALL eisa_$(EISAVERSION)-nv.tar.gz
	cd vignettes && $(R) CMD Sweave tissues.Rnw || rm tissues.tex

eisa/inst/doc/EISA_module_trees.vignette: vignettes/EISA_module_trees.Rnw
	cp $< $@

eisa/inst/doc/EISA_module_trees.pdf: vignettes/EISA_module_trees.pdf
	cp $< $@

vignettes/EISA_module_trees.pdf: vignettes/EISA_module_trees.tex
	cd vignettes && pdflatex EISA_module_trees && bibtex EISA_module_trees\
		&& pdflatex EISA_module_trees && pdflatex EISA_module_trees

vignettes/EISA_module_trees.tex: vignettes/EISA_module_trees.Rnw \
		eisa_$(EISAVERSION)-nv.tar.gz isa2_$(ISAVERSION)-nv.tar.gz
	$(R) CMD INSTALL isa2_$(ISAVERSION)-nv.tar.gz
	$(R) CMD INSTALL eisa_$(EISAVERSION)-nv.tar.gz
	cd vignettes && $(R) CMD Sweave EISA_module_trees.Rnw

# ExpressionView

ExpressionView-final: ExpressionView_$(EVVERSION).tar.gz
ExpressionView: ExpressionView_$(EVVERSION)-nv.tar.gz

EVFILES=ExpressionView/src/ch/unil/cbg/ExpressionView/*/*	\
	ExpressionView/src/ExpressionView.mxml

REVFILES=RExpressionView/inst/ExpressionView.swf	\
	RExpressionView/inst/ExpressionView.html		\
	RExpressionView/DESCRIPTION 				\
	RExpressionView/NAMESPACE				\
	RExpressionView/R/*.R RExpressionView/man/*.Rd 		\
	RExpressionView/src/*.h RExpressionView/src/*.cpp       \
	RExpressionView/inst/doc/*.Rnw                          \
	RExpressionView/inst/doc/ExpressionView.bib


ALIVEPDFFILES = $(shell find ExpressionView/src/org -name "*.as" -print)
ALIVEPDFCLASSES = $(subst ExpressionView.src.,,$(subst /,.,$(basename $(ALIVEPDFFILES))))

ExpressionView/libs/AlivePDF.swc: $(ALIVEPDFFILES)
	cd ExpressionView/src/
	compc -source-path ExpressionView/src -include-classes $(ALIVEPDFCLASSES) -output $@

RExpressionView/inst/ExpressionView.swf: $(EVFILES) ExpressionView/libs/AlivePDF.swc
	cd ExpressionView/src && \
	mxmlc -compiler.library-path+=../libs/AlivePDF.swc \
			-use-network=true ExpressionView.mxml \
			-target-player=10 \
			-debug=false \
			-define+=CONFIG::air,false \
			-output ../../$(@)

ExpressionView/doc/.stamp: $(EVFILES)
	cd ExpressionView && \
	asdoc 	-target-player 10 \
		-source-path src \
		-exclude-classes $(ALIVEPDFCLASSES) \
		-output doc \
		-doc-sources src \
		-external-library-path libs \
		-define+=CONFIG::air,false
	rm -rf RExpressionView/inst/srcview
	cp -r ExpressionView/doc RExpressionView/inst/srcview
	touch ExpressionView/doc/.stamp

ExpressionView_$(EVVERSION).tar.gz: $(REVFILES) ExpressionView/doc/.stamp
	$(R) CMD build RExpressionView

ExpressionView_$(EVVERSION)-nv.tar.gz: $(REVFILES) ExpressionView/doc/.stamp
	$(R) CMD build --no-vignettes RExpressionView
	mv 	ExpressionView_$(EVVERSION).tar.gz \
		ExpressionView_$(EVVERSION)-nv.tar.gz

############################################
## Homepage

SWEAVE2HTML = htlatex
SWEAVE2HTMLOPTIONS = style,xhtml,graphics-,charset=utf-8 " -cunihtf -utf8" -cvalidate
REMOVEHTML = sed '/<div class="maketitle">/,/<\/body>/!d' | sed '/<\/body>/d'
REWRITEIMG = sed 's/src="\([^"]*\)"/src="htmlets\/graphics\/\1"/g'

homepage: vignettes # manuals

vignettes: homepage/ISA_tutorial.html homepage/ISA_tutorial.pdf \
	homepage/ISA_parallel.html homepage/ISA_parallel.pdf \
	homepage/EISA_tutorial.html homepage/EISA_tutorial.pdf \
	homepage/EISA_module_trees.html homepage/EISA_module_trees.pdf \
	homepage/EISA_biclust.html homepage/EISA_biclust.pdf \
	homepage/tissues.html homepage/tissues.pdf \
	homepage/ISA_internals.html homepage/ISA_internals.pdf \
	homepage/ExpressionView.tutorial.html \
	homepage/ExpressionView.tutorial.pdf \
	homepage/ExpressionView.ordering.html \
	homepage/ExpressionView.ordering.pdf \
	homepage/ExpressionView.format.html \
	homepage/ExpressionView.format.pdf \
	vignettes/style.cfg

homepage/ISA_tutorial.html: vignettes/ISA_tutorial.tex
	cd vignettes && $(SWEAVE2HTML) ISA_tutorial $(SWEAVE2HTMLOPTIONS)
	cat vignettes/ISA_tutorial.html | $(REMOVEHTML) | $(REWRITEIMG) >homepage/ISA_tutorial.html
	cp vignettes/ISA_tutorial.css homepage/
	cp vignettes/ISA_tutorial*.png homepage/
	cp vignettes/*.png homepage/
homepage/ISA_tutorial.pdf: vignettes/ISA_tutorial.pdf
	cp $< $@

homepage/ISA_parallel.html: vignettes/ISA_parallel.tex
	cd vignettes && $(SWEAVE2HTML) ISA_parallel $(SWEAVE2HTMLOPTIONS)
	cat vignettes/ISA_parallel.html | $(REMOVEHTML) | $(REWRITEIMG) >homepage/ISA_parallel.html
	cp vignettes/ISA_parallel.css homepage/
#	cp vignettes/ISA_parallel*.png homepage/
#	cp vignettes/*.png homepage/graphics/
homepage/ISA_parallel.pdf: vignettes/ISA_parallel.pdf
	cp $< $@

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
	cd eisa/inst/doc/ && $(R) CMD Sweave EISA_tutorial.Rnw

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

homepage/ISA_internals.html: vignettes/ISA_internals.tex
	cd vignettes && pdflatex ISA_internals.tex && \
		bibtex ISA_internals && pdflatex ISA_internals
	cd vignettes && $(SWEAVE2HTML) ISA_internals $(SWEAVE2HTMLOPTIONS)
	cat vignettes/ISA_internals.html | $(REMOVEHTML) | $(REWRITEIMG) >homepage/ISA_internals.html
	cp vignettes/ISA_internals.css homepage/
	cp vignettes/ISA_internals*.png homepage/
#	cp vignettes/*.png homepage/
homepage/ISA_internals.pdf: vignettes/ISA_internals.pdf
	cp $< $@

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
	cd eisa/inst/doc/ && $(R) CMD Sweave EISA_biclust.Rnw

homepage/ExpressionView.tutorial.html: RExpressionView/inst/doc/ExpressionView.tutorial.tex
	cp vignettes/style.cfg RExpressionView/inst/doc/
	cd RExpressionView/inst/doc/ && pdflatex ExpressionView.tutorial.tex && \
		bibtex ExpressionView.tutorial && pdflatex ExpressionView.tutorial
	cd RExpressionView/inst/doc && \
		$(SWEAVE2HTML) ExpressionView.tutorial $(SWEAVE2HTMLOPTIONS)
	cat RExpressionView/inst/doc/ExpressionView.tutorial.html | \
		$(REMOVEHTML) | $(REWRITEIMG) >homepage/ExpressionView.tutorial.html
	cp RExpressionView/inst/doc/ExpressionView.tutorial.css homepage/
	cp RExpressionView/inst/doc/ExpressionView.tutorial*.png homepage/
homepage/ExpressionView.tutorial.pdf: RExpressionView/inst/doc/ExpressionView.tutorial.pdf
	cp $< $@
RExpressionView/inst/doc/ExpressionView.tutorial.pdf: RExpressionView/inst/doc/ExpressionView.tutorial.tex
	cd RExpressionView/inst/doc/ && pdflatex ExpressionView.tutorial.tex
RExpressionView/inst/doc/ExpressionView.tutorial.tex: RExpressionView/inst/doc/ExpressionView.tutorial.Rnw
	cd RExpressionView/inst/doc/ && $(R) CMD Sweave ExpressionView.tutorial.Rnw

homepage/ExpressionView.ordering.html: RExpressionView/inst/doc/ExpressionView.ordering.tex
	cp vignettes/style.cfg RExpressionView/inst/doc/
	cd RExpressionView/inst/doc/ && pdflatex ExpressionView.ordering.tex && \
		pdflatex ExpressionView.ordering
	cd RExpressionView/inst/doc && \
		$(SWEAVE2HTML) ExpressionView.ordering $(SWEAVE2HTMLOPTIONS)
	cat RExpressionView/inst/doc/ExpressionView.ordering.html | \
		$(REMOVEHTML) | $(REWRITEIMG) >homepage/ExpressionView.ordering.html
	cp RExpressionView/inst/doc/ExpressionView.ordering.css homepage/
	cp RExpressionView/inst/doc/ExpressionView.ordering*.png homepage/
homepage/ExpressionView.ordering.pdf: RExpressionView/inst/doc/ExpressionView.ordering.pdf
	cp $< $@
RExpressionView/inst/doc/ExpressionView.ordering.pdf: RExpressionView/inst/doc/ExpressionView.ordering.tex
	cd RExpressionView/inst/doc/ && pdflatex ExpressionView.ordering.tex
RExpressionView/inst/doc/ExpressionView.ordering.tex: RExpressionView/inst/doc/ExpressionView.ordering.Rnw
	cd RExpressionView/inst/doc/ && $(R) CMD Sweave ExpressionView.ordering.Rnw

homepage/ExpressionView.format.html: RExpressionView/inst/doc/ExpressionView.format.tex
	cp vignettes/style.cfg RExpressionView/inst/doc/
	cd RExpressionView/inst/doc/ && pdflatex ExpressionView.format.tex && \
		bibtex ExpressionView.format && pdflatex ExpressionView.format
	cd RExpressionView/inst/doc && \
		$(SWEAVE2HTML) ExpressionView.format $(SWEAVE2HTMLOPTIONS)
	cat RExpressionView/inst/doc/ExpressionView.format.html | \
		$(REMOVEHTML) | $(REWRITEIMG) >homepage/ExpressionView.format.html
	cp RExpressionView/inst/doc/ExpressionView.format.css homepage/
homepage/ExpressionView.format.pdf: RExpressionView/inst/doc/ExpressionView.format.pdf
	cp $< $@
RExpressionView/inst/doc/ExpressionView.format.pdf: RExpressionView/inst/doc/ExpressionView.format.tex
	cd RExpressionView/inst/doc/ && pdflatex ExpressionView.format.tex
RExpressionView/inst/doc/ExpressionView.format.tex: RExpressionView/inst/doc/ExpressionView.format.Rnw
	cd RExpressionView/inst/doc/ && $(R) CMD Sweave ExpressionView.format.Rnw

manuals: homepage/manuals/isa2/.stamp homepage/manuals/eisa/.stamp \
	homepage/manuals/ExpressionView/.stamp \
	homepage/manuals/isa2.pdf homepage/manuals/eisa.pdf \
	homepage/manuals/ExpressionView.pdf

homepage/manuals/isa2/.stamp: isa2_$(ISAVERSION).tar.gz
	scripts/manual.sh isa2 $(ISAVERSION)

homepage/manuals/isa2.pdf: isa2_$(ISAVERSION).tar.gz
	rm -f isa2.pdf && $(R) CMD Rd2dvi --pdf --no-preview isa2 && \
	mv isa2.pdf homepage/manuals/

homepage/manuals/eisa/.stamp: eisa_$(EISAVERSION).tar.gz
	scripts/manual.sh eisa $(EISAVERSION)

homepage/manuals/eisa.pdf: eisa_$(EISAVERSION).tar.gz
	rm -f eisa.pdf && $(R) CMD Rd2dvi --pdf --no-preview eisa && \
	mv eisa.pdf homepage/manuals/

homepage/manuals/ExpressionView/.stamp: ExpressionView_$(EVVERSION).tar.gz
	scripts/manual.sh ExpressionView $(EVVERSION)

homepage/manuals/ExpressionView.pdf: ExpressionView_$(EVVERSION).tar.gz
	rm -rf RExpressionView.pdf && \
	$(R) CMD Rd2dvi --pdf --no-preview RExpressionView && \
	mv RExpressionView.pdf homepage/manuals/ExpressionView.pdf
