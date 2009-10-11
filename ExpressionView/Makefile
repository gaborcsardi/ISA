# Makefile for the ExpressionView applet

all: alivepdf doc release

.PHONE: doc

EXPRESSIONVIEWFILES = $(shell find src/ch -name "*.as" -print)

ALIVEPDFFILES = $(shell find src/org -name "*.as" -print)
ALIVEPDFCLASSES = $(subst src.,,$(subst /,.,$(basename $(ALIVEPDFFILES))))

alivepdf: libs/AlivePDF.swc

libs/AlivePDF.swc: $(ALIVEPDFFILES)
	cd src/
	compc -source-path src -include-classes $(ALIVEPDFCLASSES) -output $@
	

doc: $(EXPRESSIONVIEWFILES)
	rm -rf asdoc-output
	mkdir asdoc-output
	asdoc -target-player 10 -source-path src -doc-sources src -external-library-path libs

release: bin-release/ExpressionView.swf

bin-release/ExpressionView.swf: alivepdf $(EXPRESSIONVIEWFILES)
	@echo build release build
	@rm -rf bin-release
	@mkdir bin-release
	@cd src && \
	mxmlc -compiler.library-path+=../libs/AlivePDF.swc \
			-use-network=false ExpressionView.mxml \
			-target-player=10 \
			-debug=false && \
	@mv src/$(@F) $(@)

debug: bin-debug/ExpressionView.swf

bin-debug/ExpressionView.swf: alivepdf $(EXPRESSIONVIEWFILES)
	@echo build debug build
	@rm -rf bin-debug
	@mkdir bin-debug
	@cd src && \
	mxmlc -compiler.library-path+=../libs/AlivePDF.swc \
			-use-network=false ExpressionView.mxml \
			-target-player=10 \
			-debug=true
	@mv src/$(@F) $(@)
	