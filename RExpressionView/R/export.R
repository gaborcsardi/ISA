ExpressionView <- function(modules, eset, order) {

  filename <- paste(tempfile(), sep="", ".ged")
  toExpressionView(modules, eset, order, filename=filename)
  swf <- system.file("ExpressionView.html", package="ExpressionView")
  url <- URLencode(paste("file://", swf, sep="", "?filename=", filename))
  browseURL(url)
}

toExpressionView <- function(modules, eset, order, filename="",
                             norm=c("sample", "feature", "raw")) {
	
	library(XML)
	
	if ( filename == "" ) {
		con <- file(file.choose(TRUE), open="wb")
	} else {
		con <- file(filename, open="wb", blocking = TRUE)
	}
		
	geneMaps <- order$genes
	sampleMaps <- order$samples
	
	Genes <- featureNames(modules);
	Samples <- sampleNames(modules);
		
	nGenes <- (dim(modules))[1]
	nSamples <- (dim(modules))[2]
	nModules <- length(modules)

	writeBin("ExpressionViewFile", con, endian="big")
	writeBin(as.integer(nGenes), con, 4, endian="big")
	writeBin(as.integer(nSamples), con, 4, endian="big")
	writeBin(as.integer(nModules), con, 4, endian="big")

        norm <- match.arg(norm)
        Data <- eisa:::select.eset(eset, modules, norm)
        Data <- Data[ geneMaps[[1]], sampleMaps[[1]] ]
        Data <- as.vector(Data)
        
	Data.min <- min(Data)
	Data.max <- max(Data)
	Data.delta <- Data.max - Data.min

        Data <- (Data - Data.min) / Data.delta * 2 -1
        writeBin(Data, con, endian="big")
        	
	#<isadata>
	
	#	<experimentdata>
	#		<title></title>
	#		<name></name>
	#		<lab></lab>
	#		<abstract></abstract>
	#		<url></url>
	#		...
	#	</experimentdata> 	
	#	<genes> 	
	#		<gene>
	#			<tag0></tag0>		## id
	#			<tag1></tag1>
	#			...
	#		</gene> 	
	#	</genes>"; 	
	#	<samples> 	
	#		<sample>
	#			<tag0></tag0>		## id
	#			<tag1></tag1>
	#			...
	#		</sample> 	
	#	</samples>"; 	
	#	<modules> 	
	#		<module>
	#			<tag0></tag0>
	#			<tag1></tag1>
	#			...
	#			<containedgenes></containedgenes>
	#			<containedsamples></containedsamples>
	#			<intersectingmodules></intersectingmodules>
	#		</module> 	
	#	</modules>
	#</isadata> 
	xmldata = xmlTree("isadata")
	
		# get gene info
		ann <- annotation(eset)
		library(paste(ann, sep="", ".db"), character.only=TRUE)
		symbol.table <- toTable(get(paste(ann, "SYMBOL", sep="")))
		entrez.table <- toTable(get(paste(ann, "ENTREZID", sep="")))

		xmldata$addNode("experimentdata", close = FALSE)
			xmldata$addNode("title", eset@experimentData@title)
			xmldata$addNode("name", eset@experimentData@name)
			xmldata$addNode("lab", eset@experimentData@lab)
			xmldata$addNode("abstract", eset@experimentData@abstract)
			xmldata$addNode("url", eset@experimentData@url)
			xmldata$addNode("annotation", eset@annotation)
			organism <- get(paste(ann, "ORGANISM", sep=""))
			xmldata$addNode("organism", organism)
		xmldata$closeTag()

		xmldata$addNode("genes", close = FALSE)
			xmldata$addNode("shortgenetags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			xmldata$addNode("tag", "symbol")
			xmldata$addNode("tag", "entrezid")
			xmldata$closeTag()		

			xmldata$addNode("longgenetags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			xmldata$addNode("tag", "symbol")
			xmldata$addNode("tag", "entrezid")
			xmldata$closeTag()

			for ( gene in 1:nGenes ) {
				xmldata$addNode("gene", close = FALSE)
					xmldata$addNode("tag0", gene)
					genep <- Genes[geneMaps[[1]][gene]]
					xmldata$addNode("tag1", genep)
					xmldata$addNode("tag2", paste("", symbol.table[which(symbol.table==genep),2], sep=""))
					xmldata$addNode("tag3", paste("", entrez.table[which(entrez.table==genep),2], sep=""))
				xmldata$closeTag()
			}
		xmldata$closeTag()	

		xmldata$addNode("samples", close = FALSE)
		
			xmldata$addNode("shortsampletags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			temp <- rownames(phenoData(eset)@varMetadata)
			if ( length(temp) >= 2 ) {			
				for ( i in 2:length(temp) ) {
					xmldata$addNode("tag", temp[i])
				}
			}
			xmldata$closeTag()
			
			xmldata$addNode("longsampletags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			temp <- phenoData(eset)@varMetadata[[1]]
			if ( length(temp) >= 2 ) {
				for ( i in 2:length(temp) ) {
					xmldata$addNode("sampletag", temp[i])
				}
			}
			xmldata$closeTag()		

			for ( sample in 1:nSamples ) {
				xmldata$addNode("sample", close = FALSE)
					xmldata$addNode("tag0", sample)
					xmldata$addNode("tag1", Samples[sampleMaps[[1]][sample]])
					if ( dim(eset@phenoData@data)[2] != 0 ) {
						temp <- eset@phenoData@data[sampleMaps[[1]][sample],]
						for ( i in 2:length(temp) ) {
							xmldata$addNode(paste("tag", i, sep=""), temp[i])
						}
					}
				xmldata$closeTag()
			}

		xmldata$closeTag()
	
		xmldata$addNode("modules", close = FALSE)
		
			xmldata$addNode("shortmoduletags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			temp <- colnames(modules@seeddata)
			if ( length(temp) >= 1 ) {
				for ( i in 1:length(temp) ) {
					xmldata$addNode("tag", temp[i])
				}
			}
			xmldata$closeTag()
			
			xmldata$addNode("longmoduletags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			temp <- colnames(modules@seeddata)
			if ( length(temp) >= 1 ) {
				for ( i in 1:length(temp) ) {
					xmldata$addNode("tag", temp[i])
				}
			}
			xmldata$closeTag()		

		for ( module in 1:nModules ) {
			xmldata$addNode("module", close = FALSE)
				xmldata$addNode("tag0", module)
				xmldata$addNode("tag1", paste("module", module))
				if ( length(colnames(modules@seeddata)) != 0 ) {
					temp <- modules@seeddata[module,]
					for ( i in 1:length(temp) ) {
						xmldata$addNode(paste("tag", i+1, sep=""), temp[i])
					}
				}
				
				genes = as.matrix(modules@genes[,module]);
				temp <- mat.or.vec(getNoFeatures(modules)[module],1)
				genesp <- mat.or.vec(getNoFeatures(modules)[module],1)
				i = 1;
				intersectingmodulesgenes = list();
				for ( gene in 1:nGenes ) {
					if ( genes[gene] != 0 ) {
						temp[i] <- gene
						i <- i + 1
						
						for ( modulep in 1:nModules ) {
							if ( module != modulep ) {
								if ( modules@genes[gene, modulep] != 0 ) {
									intersectingmodulesgenes <- append(intersectingmodulesgenes, modulep)
								}
							}
						}
						
					}
				}
				for ( gene in 1:length(temp) ) {
					genesp[gene] <- which(geneMaps[[1]]==temp[geneMaps[[module+1]][gene]])
				}
				xmldata$addNode("containedgenes", toString(genesp))

				samples = as.matrix(modules@conditions[,module]);
				temp <- mat.or.vec(getNoSamples(modules)[module],1)
				samplesp <- mat.or.vec(getNoSamples(modules)[module],1)
				i = 1;
				intersectingmodulessamples = list();
				for ( sample in 1:nSamples ) {
					if ( samples[sample] != 0 ) {
						temp[i] <- sample
						i <- i + 1
						
						for ( modulep in 1:nModules ) {
							if ( module != modulep ) {
								if ( modules@conditions[sample, modulep] != 0 ) {
									intersectingmodulessamples <- append(intersectingmodulessamples, modulep)
								}
							}
						}

						
					}
				}
				for ( sample in 1:length(temp) ) {
					samplesp[sample] <- which(sampleMaps[[1]]==temp[sampleMaps[[module+1]][sample]])
				}
				xmldata$addNode("containedsamples", toString(samplesp))

				intersectingmodules <- intersect(unique(intersectingmodulesgenes), unique(intersectingmodulessamples))
				xmldata$addNode("intersectingmodules", toString(intersectingmodules))
								
			xmldata$closeTag()
		}
		xmldata$closeTag()
		
		# to get rid of In xmlRoot.XMLInternalDocument(currentNodes[[1]]) : empty XML document use
		# options(warn=-1)

		# show XML data
		# cat(saveXML(xmldata))
		writeBin(saveXML(xmldata), con, 1, endian="big")
		
	close(con)

}
