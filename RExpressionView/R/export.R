ExpressionView <- function(eisamodules, gedata, order) {

  filename <- paste(tempfile(), sep="", ".ged")
  toExpressionView(eisamodules, gedata, order, filename=filename)
  swf <- system.file("ExpressionView.html", package="ExpressionView")
  url <- URLencode(paste("file://", swf, sep="", "?filename=", filename))
  browseURL(url)
}

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

	library(GO.db)
	go.table <- toTable(GOTERM)
	gos <- ISA.GO(biclusts)

	library(KEGG.db)
	kegg.table <- toTable(KEGGPATHID2NAME)
	keggs <- ISA.KEGG(biclusts)
	
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
	#			<gos>
	#				<go>
	#					<id></id>
	#					<term></term>
	#					<ontology></ontology>
	#					<pvalue></pvalue>
	#					<oddsratio></oddsratio>
	#					<expcount></expcount>
	#					<count></count>
	#					<size></size>
	#				</go>
	#			</gos>
	#			<keggs>
	#				<kegg>
	#				</kegg>
	#			</keggs>
	#		</module>
	#	</modules>
	#</isadata> 
	xmldata = xmlTree("isadata")
	
		# get gene info
		ann <- annotation(gedata)
		library(paste(ann, sep="", ".db"), character.only=TRUE)
		symbol.table <- toTable(get(paste(ann, "SYMBOL", sep="")))
		entrez.table <- toTable(get(paste(ann, "ENTREZID", sep="")))

		xmldata$addNode("experimentdata", close = FALSE)
			xmldata$addNode("title", gedata@experimentData@title)
			xmldata$addNode("name", gedata@experimentData@name)
			xmldata$addNode("lab", gedata@experimentData@lab)
			xmldata$addNode("abstract", gedata@experimentData@abstract)
			xmldata$addNode("url", gedata@experimentData@url)
			xmldata$addNode("annotation", gedata@annotation)
			organism <- get(paste(ann, "ORGANISM", sep=""))
			xmldata$addNode("organism", organism)
		xmldata$closeTag()

		xmldata$addNode("genes", close = FALSE)
			xmldata$addNode("shortgenetags", close = FALSE)
			xmldata$addNode("tag", "id") 		# 0
			xmldata$addNode("tag", "score") 	# 1
			xmldata$addNode("tag", "name") 		# 2
			xmldata$addNode("tag", "symbol")	# 3
			xmldata$addNode("tag", "entrezid")	# 4
			xmldata$closeTag()		

			xmldata$addNode("longgenetags", close = FALSE)
			xmldata$addNode("tag", "id") 		# 0
			xmldata$addNode("tag", "score") 	# 1
			xmldata$addNode("tag", "name") 		# 2
			xmldata$addNode("tag", "symbol")	# 3
			xmldata$addNode("tag", "entrezid")	# 4
			xmldata$closeTag()

			for ( gene in 1:nGenes ) {
				xmldata$addNode("gene", close = FALSE)
					xmldata$addNode("tag0", gene)
					genep <- Genes[geneMaps[[1]][gene]]
					xmldata$addNode("tag1", "")
					xmldata$addNode("tag2", genep)
					xmldata$addNode("tag3", paste("", symbol.table[which(symbol.table==genep),2], sep=""))
					xmldata$addNode("tag4", paste("", entrez.table[which(entrez.table==genep),2], sep=""))
				xmldata$closeTag()
			}
		xmldata$closeTag()	

		xmldata$addNode("samples", close = FALSE)
		
			xmldata$addNode("shortsampletags", close = FALSE)
			xmldata$addNode("tag", "id")		# 0
			xmldata$addNode("tag", "score")		# 1
			xmldata$addNode("tag", "name")		# 2
			temp <- rownames(phenoData(gedata)@varMetadata)
			if ( length(temp) >= 2 ) {			
				for ( i in 2:length(temp) ) {
					xmldata$addNode("tag", temp[i])
				}
			}
			xmldata$closeTag()
			
			xmldata$addNode("longsampletags", close = FALSE)
			xmldata$addNode("tag", "id")		# 0
			xmldata$addNode("tag", "score")		# 1
			xmldata$addNode("tag", "name")		# 2
			temp <- phenoData(gedata)@varMetadata[[1]]
			if ( length(temp) >= 2 ) {
				for ( i in 2:length(temp) ) {
					xmldata$addNode("tag", temp[i])
				}
			}
			xmldata$closeTag()		

			for ( sample in 1:nSamples ) {
				xmldata$addNode("sample", close = FALSE)
					xmldata$addNode("tag0", sample)
					xmldata$addNode("tag1", "")
					xmldata$addNode("tag2", Samples[sampleMaps[[1]][sample]])
					if ( dim(gedata@phenoData@data)[2] != 0 ) {
						temp <- gedata@phenoData@data[sampleMaps[[1]][sample],]
						for ( i in 2:length(temp) ) {
							xmldata$addNode(paste("tag", i+1, sep=""), temp[i])
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

			xmldata$addNode("shortgotags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "GO")
			xmldata$addNode("tag", "Term")
			xmldata$addNode("tag", "Ontology")
			xmldata$addNode("tag", "Definition")
			xmldata$addNode("tag", "PValue")
			xmldata$addNode("tag", "OddsRatio")
			xmldata$addNode("tag", "ExpCount")
			xmldata$addNode("tag", "Count")
			xmldata$addNode("tag", "Size")
			xmldata$closeTag()

			xmldata$addNode("longgotags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "GO")
			xmldata$addNode("tag", "Term")
			xmldata$addNode("tag", "Ontology")
			xmldata$addNode("tag", "Definition")
			xmldata$addNode("tag", "PValue")
			xmldata$addNode("tag", "OddsRatio")
			xmldata$addNode("tag", "ExpCount")
			xmldata$addNode("tag", "Count")
			xmldata$addNode("tag", "Size")
			xmldata$closeTag()

			xmldata$addNode("shortkeggtags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "KEGG")
			xmldata$addNode("tag", "Path Name")
			xmldata$addNode("tag", "PValue")
			xmldata$addNode("tag", "OddsRatio")
			xmldata$addNode("tag", "ExpCount")
			xmldata$addNode("tag", "Count")
			xmldata$addNode("tag", "Size")
			xmldata$closeTag()

			xmldata$addNode("longkeggtags", close = FALSE)
			xmldata$addNode("tag", "id")
			xmldata$addNode("tag", "KEGG")
			xmldata$addNode("tag", "Path name")
			xmldata$addNode("tag", "PValue")
			xmldata$addNode("tag", "OddsRatio")
			xmldata$addNode("tag", "ExpCount")
			xmldata$addNode("tag", "Count")
			xmldata$addNode("tag", "Size")
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
				temp <- vector("numeric", getNoFeatures(eisamodules)[module])
				genesp <- vector("numeric", getNoFeatures(eisamodules)[module])
				scores <- vector("numeric", getNoFeatures(eisamodules)[module])
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
					scores[gene] <- genes[temp[geneMaps[[module+1]][gene]]]
				}
				xmldata$addNode("containedgenes", toString(genesp))
				xmldata$addNode("genescores", toString(scores))
				

				samples = as.matrix(eisamodules@conditions[,module]);
				temp <- vector("numeric", getNoSamples(eisamodules)[module])
				samplesp <- vector("numeric", getNoSamples(eisamodules)[module])
				scores <- vector("numeric", getNoSamples(eisamodules)[module])
				i <- 1;
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
					scores[sample] <- samples[temp[sampleMaps[[module+1]][sample]]]
				}
				xmldata$addNode("containedsamples", toString(samplesp))
				xmldata$addNode("samplescores", toString(scores))

				intersectingmodules <- intersect(unique(intersectingmodulesgenes), unique(intersectingmodulessamples))
				xmldata$addNode("intersectingmodules", toString(intersectingmodules))
				
				xmldata$addNode("gos", close=FALSE)
				k <- 1
				for ( i in 1:3 ) {
					s <- summary(gos[[i]])[[module]]
					if ( dim(s)[1] > 0 ) {
						for ( j in 1:dim(s)[1] ) {
							xmldata$addNode("go", close=FALSE)
								xmldata$addNode("tag0", k)
								k <- k + 1
								goid <- rownames(s)[j]
								xmldata$addNode("tag1", goid)
								goentry <- which(go.table[,1]==goid)
								xmldata$addNode("tag2", go.table[goentry,3])
								xmldata$addNode("tag3", go.table[goentry,4])
								xmldata$addNode("tag4", go.table[goentry,5])
								xmldata$addNode("tag5", s[j,1])
								xmldata$addNode("tag6", s[j,2])
								xmldata$addNode("tag7", s[j,3])
								xmldata$addNode("tag8", s[j,4])
								xmldata$addNode("tag9", s[j,5])
							xmldata$closeTag()
						}
					}
				}
				xmldata$closeTag()
				
				xmldata$addNode("keggs", close=FALSE)
				k <- 1
				s <- summary(keggs)[[module]]
				if ( dim(s)[1] > 0 ) {
					for ( j in 1:dim(s)[1] ) {
						xmldata$addNode("kegg", close=FALSE)
							xmldata$addNode("tag0", k)
							k <- k + 1
							keggid <- rownames(s)[j]
							xmldata$addNode("tag1", keggid)
							keggentry <- which(kegg.table[,1]==keggid)
							xmldata$addNode("tag2", kegg.table[keggentry,2])
							xmldata$addNode("tag3", s[j,1])
							xmldata$addNode("tag4", s[j,2])
							xmldata$addNode("tag5", s[j,3])
							xmldata$addNode("tag6", s[j,4])
							xmldata$addNode("tag7", s[j,5])
						xmldata$closeTag()
					}
				}
				xmldata$closeTag()

			xmldata$closeTag()
		}
		xmldata$closeTag()
		
		# to get rid of "In xmlRoot.XMLInternalDocument(currentNodes[[1]]) : empty XML document use"
		# options(warn=-1)

		# show XML data
		cat(saveXML(xmldata))
		writeBin(saveXML(xmldata), con, 1, endian="big")
		
	close(con)

}
