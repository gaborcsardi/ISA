toExpressionView <- function(eisamodules, gedata, order, filename="") {
	
	library(XML)
	
	if ( filename == "" ) {
		con <- file(file.choose(TRUE), open="wb")
	} else {
		con <- file(filename, open="wb", blocking = TRUE)
	}
		
	geneMaps <- order$genes
	sampleMaps <- order$samples
	
	Genes <- featureNames(eisamodules);
	Samples <- sampleNames(eisamodules);
		
	nGenes <- (dim(eisamodules))[1]
	nSamples <- (dim(eisamodules))[2]
	nModules <- length(eisamodules)

	writeBin("ExpressionViewFile", con, endian="big")
	writeBin(nGenes, con, 4, endian="big")
	writeBin(nSamples, con, 4, endian="big")
	writeBin(nModules, con, 4, endian="big")

	GenesLookup = c(0);
	for ( gene in 1:nGenes ) {
		GenesLookup[gene] <- which(featureNames(gedata)==Genes[gene])
	}
	SamplesLookup = c(0);
	for ( sample in 1:nSamples ) {
		SamplesLookup[sample] <- which(sampleNames(gedata)==Samples[sample])
	}
	
	allData <- exprs(gedata)
	Data <- mat.or.vec(nSamples, nGenes)
	for ( sample in 1:nSamples ) {
		for ( gene in 1:nGenes ) {
			Data[sample, gene] <- allData[GenesLookup[geneMaps[[1]][gene]], SamplesLookup[sampleMaps[[1]][sample]]]
		}
	}		

	Data <- t(isa.normalize(Data)[[1]])
	Data.min <- min(Data)
	Data.max <- max(Data)
	Data.delta <- Data.max - Data.min


	for ( sample in 1:nSamples ) {
		for ( gene in 1:nGenes ) {
			value <- Data[sample, gene]
			value <- (value - Data.min) / Data.delta * 2 - 1
			writeBin(value, con, 4, endian="big")
		}
	}
	
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
	
		xmldata$addNode("experimentdata", close = FALSE)
			xmldata$addNode("title", gedata@experimentData@title)
			xmldata$addNode("name", gedata@experimentData@name)
			xmldata$addNode("lab", gedata@experimentData@lab)
			xmldata$addNode("abstract", gedata@experimentData@abstract)
			xmldata$addNode("url", gedata@experimentData@url)
			xmldata$addNode("annotation", gedata@annotation)
		xmldata$closeTag()

		xmldata$addNode("genes", close = FALSE)
			xmldata$addNode("shortgenetags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			temp <- rownames(featureData(gedata)@varMetadata)
			if ( length(temp) >= 2 ) {
				for ( i in 2:length(temp) ) {
					xmldata$addNode("tag", temp[i])
				}
			}
			xmldata$closeTag()		
			xmldata$addNode("longgenetags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			if ( length(temp) >= 2 ) {
				for ( i in 2:length(temp) ) {
					xmldata$addNode("tag", featureData(ALL)@varMetadata[[1]][i])
				}
			}
			xmldata$closeTag()

			for ( gene in 1:nGenes ) {
				xmldata$addNode("gene", close = FALSE)
					xmldata$addNode("tag0", gene)
					xmldata$addNode("tag1", Genes[geneMaps[[1]][gene]])
					if ( dim(gedata@featureData@data)[2] != 0 ) {
						temp <- gedata@featureData@data[geneMaps[[1]][gene],]
						for ( i in 2:length(temp) ) {
							xmldata$addNode(paste("tag", i, sep=""), temp[i])
						}
					}
				xmldata$closeTag()
			}
		xmldata$closeTag()	

		xmldata$addNode("samples", close = FALSE)
		
			xmldata$addNode("shortsampletags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			temp <- rownames(phenoData(gedata)@varMetadata)
			if ( length(temp) >= 2 ) {			
				for ( i in 2:length(temp) ) {
					xmldata$addNode("tag", temp[i])
				}
			}
			xmldata$closeTag()
			
			xmldata$addNode("longsampletags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			temp <- phenoData(gedata)@varMetadata[[1]]
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
					if ( dim(gedata@phenoData@data)[2] != 0 ) {
						temp <- gedata@phenoData@data[sampleMaps[[1]][sample],]
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
			temp <- colnames(eisamodules@seeddata)
			if ( length(temp) >= 1 ) {
				for ( i in 1:length(temp) ) {
					xmldata$addNode("tag", temp[i])
				}
			}
			xmldata$closeTag()
			
			xmldata$addNode("longmoduletags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "name")
			temp <- colnames(eisamodules@seeddata)
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
				if ( length(colnames(eisamodules@seeddata)) != 0 ) {
					temp <- eisamodules@seeddata[module,]
					for ( i in 1:length(temp) ) {
						xmldata$addNode(paste("tag", i+1, sep=""), temp[i])
					}
				}
				
				genes = as.matrix(eisamodules@genes[,module]);
				temp <- mat.or.vec(getNoFeatures(eisamodules)[module],1)
				genesp <- mat.or.vec(getNoFeatures(eisamodules)[module],1)
				i = 1;
				intersectingmodulesgenes = list();
				for ( gene in 1:nGenes ) {
					if ( genes[gene] != 0 ) {
						temp[i] <- gene
						i <- i + 1
						
						for ( modulep in 1:nModules ) {
							if ( module != modulep ) {
								if ( eisamodules@genes[gene, modulep] != 0 ) {
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

				samples = as.matrix(eisamodules@conditions[,module]);
				temp <- mat.or.vec(getNoSamples(eisamodules)[module],1)
				samplesp <- mat.or.vec(getNoSamples(eisamodules)[module],1)
				i = 1;
				intersectingmodulessamples = list();
				for ( sample in 1:nSamples ) {
					if ( samples[sample] != 0 ) {
						temp[i] <- sample
						i <- i + 1
						
						for ( modulep in 1:nModules ) {
							if ( module != modulep ) {
								if ( eisamodules@conditions[sample, modulep] != 0 ) {
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

		cat(saveXML(xmldata))
		writeBin(saveXML(xmldata), con, 1, endian="big")
		
	close(con)

}
