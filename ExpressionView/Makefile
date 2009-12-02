# Makefile for the ExpressionView applet

all: alivepdf doc release

.PHONE: doc debug release air key clean

EXPRESSIONVIEWFILES = $(shell find src/ch -name "*.as" -print)

ALIVEPDFFILES = $(shell find src/org -name "*.as" -print)
ALIVEPDFCLASSES = $(subst src.,,$(subst /,.,$(basename $(ALIVEPDFFILES))))

alivepdf: libs/AlivePDF.swc

libs/AlivePDF.swc: $(ALIVEPDFFILES)
	@ cd src/
	compc -source-path src -include-classes $(ALIVEPDFCLASSES) -output $@
	

doc: $(EXPRESSIONVIEWFILES)
	@ rm -rf doc
	@ mkdir doc
	@ asdoc -target-player 10 \
			-source-path src \
			-exclude-classes $(ALIVEPDFCLASSES) \
			-output doc \
			-doc-sources src \
			-external-library-path libs \
			-define+=CONFIG::air,false \

release: bin/ExpressionView.swf

bin/ExpressionView.swf: alivepdf $(EXPRESSIONVIEWFILES)
	@ echo compile release build
	@ mkdir -p bin
	@ cd src && \
	mxmlc -compiler.library-path+=../libs/AlivePDF.swc \
			-use-network=false ExpressionView.mxml \
			-target-player=10 \
			-debug=false \
			-define+=CONFIG::air,false
	@ mv src/$(@F) $(@)

debug: bin-debug/ExpressionView.swf

bin-debug/ExpressionView.swf: alivepdf $(EXPRESSIONVIEWFILES)
	@ echo compile debug build
	@ mkdir -p bin-debug
	@ cd src && \
	mxmlc -compiler.library-path+=../libs/AlivePDF.swc \
			-use-network=false ExpressionView.mxml \
			-target-player=10 \
			-define+=CONFIG::air,false \
			-debug=true
	@ mv src/$(@F) $(@)

air: key air/ExpressionView.air

air/ExpressionView.air: air/ExpressionView.swf air/ExpressionView.xml air/ExpressionView.pfx
	@ echo package air application
	@ cd air && \
	adt -package -storepass ExpressionView -storetype pkcs12 -keystore ExpressionView.pfx ExpressionView.air ExpressionView.xml ExpressionView.swf icons/

air/ExpressionView.swf: alivepdf $(EXPRESSIONVIEWFILES)
	@ echo compile ExpressionView for air
	@ cd src && \
	amxmlc -compiler.library-path+=../libs/AlivePDF.swc \
	-use-network=false ExpressionView.mxml \
	-target-player=10 \
	-debug=false \
	-define+=CONFIG::air,true
	@ mv src/$(@F) $(@)

key: air/ExpressionView.pfx

# password ExpressionView
air/ExpressionView.pfx: 
	@ echo generate key
	@ cd air && \
	adt -certificate -cn ExpressionView -o "Swiss Institute of Bioinformatics" -ou "Computational Biology Group" -c CH 1024-RSA ExpressionView.pfx ExpressionView
	
clean: 
	@ echo clean doc
	@ rm -rf doc
