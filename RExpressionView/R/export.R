########################################################################
# export Biclust #######################################################
########################################################################

if (require(biclust)) {
 	setMethod("ExportEV", signature(biclusters="Biclust"), 
 		function(biclusters, eset, order, filename, norm, description) 
 		ExportEV.Biclust(biclusters, eset, order, filename, norm, description)
 	)
}

ExportEV.Biclust <- function(biclusters, eset, order, filename, norm, description) {
	eisamodules <- as(biclusters, "ISAModules")
	eisamodules@rundata$annotation <- annotation(eset)
	eisamodules@rundata$prenormalize <- FALSE
	ExportEV(eisamodules, eset, order, filename, norm, description)
}


########################################################################
# export EISA ##########################################################
########################################################################

if (require(eisa)) {
	setMethod("ExportEV", signature(biclusters="ISAModules"), 
		function(biclusters, eset, order, filename, norm, description) 
		ExportEV.ISAModules(biclusters, eset, order, filename, norm, description)
	)
}

ExportEV.ISAModules <- function(biclusters, eset, order, filename, norm, description) {

	if ( missing(order) ) {
		order <- OrderEV(biclusters)
	}

	if ( missing(filename) ) {
		con <- file(file.choose(TRUE), open="w")
	} else {
		con <- file(filename, open="w", blocking = TRUE)
	}
	
	if ( missing(norm) ) {
		#norm <- c("sample", "feature", "raw")
		norm <- "sample"
	}
	
	geneMaps <- order$genes
	sampleMaps <- order$samples
	
	Genes <- featureNames(biclusters)[geneMaps[[1]]];
	Samples <- sampleNames(biclusters)[sampleMaps[[1]]];
		
	nGenes <- (dim(biclusters))[1]
	nSamples <- (dim(biclusters))[2]
	nBiclusters <- length(biclusters)

	writeLines("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", con)
	writeLines("<evf>", con)
	writeLines("\t<summary>", con)
	writeLines("\t\t<description>ExpressionView data file</description>", con)
	writeLines("\t\t<version>1.0</version>", con)
	writeLines(paste("\t\t<nmodules>", nBiclusters, "</nmodules>", sep=""), con)
	writeLines(paste("\t\t<ngenes>", nGenes, "</ngenes>", sep=""), con)
	writeLines(paste("\t\t<nsamples>", nSamples, "</nsamples>", sep=""), con)
	writeLines("\t</summary>", con)

	go.table <- toTable(GOTERM)
	gos <- ISAGO(biclusters)

	kegg.table <- toTable(KEGGPATHID2NAME)
	keggs <- ISAKEGG(biclusters)
		
	# get gene info
	ann <- annotation(eset)
	library(paste(ann, sep="", ".db"), character.only=TRUE)
	symbol.table <- toTable(get(paste(ann, "SYMBOL", sep="")))
	entrez.table <- toTable(get(paste(ann, "ENTREZID", sep="")))

	writeLines("", con)

	# experimentdata
	writeLines("\t<experimentdata>", con)
		writeLines(paste("\t\t<title>", eset@experimentData@title, "</title>", sep=""), con)
		writeLines(paste("\t\t<name>", eset@experimentData@name, "</name>", sep=""), con)
		writeLines(paste("\t\t<lab>", eset@experimentData@lab, "</lab>", sep=""), con)
		writeLines(paste("\t\t<abstract>", eset@experimentData@abstract, "</abstract>", sep=""), con)
		writeLines(paste("\t\t<url>", eset@experimentData@url, "</url>", sep=""), con)
		writeLines(paste("\t\t<annotation>", eset@annotation, "</annotation>", sep=""), con)
		organism <- get(paste(ann, "ORGANISM", sep=""))
		writeLines(paste("\t\t<organism>", organism, "</organism>", sep=""), con)
	writeLines("\t</experimentdata>", con)

	writeLines("", con)

	# genes
	writeLines("\t<genes>", con)

		writeLines("\t\t<genetags>", con)
			writeLines("\t\t\t<id>#</id>", con)
			writeLines("\t\t\t<score>Score</score>", con)
			writeLines("\t\t\t<name>Name</name>", con)
			writeLines("\t\t\t<symbol>Symbol</symbol>", con)
			writeLines("\t\t\t<entrezid>EntrezID</entrezid>", con)
		writeLines("\t\t</genetags>", con)

		writeLines("", con)

		genemap <- match(Genes, symbol.table[,1])
		for ( gene in 1:nGenes ) {
			writeLines("\t\t<gene>", con)
				writeLines(paste("\t\t\t<id>", gene, "</id>", sep=""), con)
				writeLines("\t\t\t<score/>", con)
				writeLines(paste("\t\t\t<name>", Genes[gene], "</name>", sep=""), con)
				writeLines(paste("\t\t\t<symbol>", symbol.table[genemap[gene],2], "</symbol>", sep=""), con)
				writeLines(paste("\t\t\t<entrezid>", entrez.table[genemap[gene],2], "</entrezid>", sep=""), con)
			writeLines("\t\t</gene>", con)
		}
			
	writeLines("\t</genes>", con)


	# samples  
	writeLines("\t<samples>", con)

		writeLines("\t\t<sampletags>", con)
			writeLines("\t\t\t<id>#</id>", con)
			writeLines("\t\t\t<score>Score</score>", con)
			writeLines("\t\t\t<name>Name</name>", con)
	
			temp <- rownames(phenoData(eset)@varMetadata)
			# replace unallowed characters by underscores
			temp <- gsub("[^[:alnum:]]", "_", temp)
			tempp <- phenoData(eset)@varMetadata[[1]]
			if ( length(temp) >= 2 ) {	
				for ( i in 2:length(temp) ) {
					writeLines(paste("\t\t\t<", temp[i], ">", tempp[i], "</", temp[i], ">", sep=""), con)
				}
			}
		writeLines("\t\t</sampletags>", con)

		writeLines("", con)
		
		for ( sample in 1:nSamples ) {
			
			writeLines("\t\t<sample>", con)
				writeLines(paste("\t\t\t<id>", sample, "</id>", sep=""), con)
				writeLines("\t\t\t<score/>", con)
				writeLines(paste("\t\t\t<name>", Samples[sample], "</name>", sep=""), con)
				if ( dim(eset@phenoData@data)[2] != 0 ) {
					tempp <- eset@phenoData@data[sampleMaps[[1]][sample],]
					for ( i in 2:length(temp) ) {
						writeLines(paste("\t\t\t<", temp[i], ">", tempp[i], "</", temp[i], ">", sep=""), con)
					}
				}
				
			writeLines("\t\t</sample>", con)
		}
	writeLines("\t</samples>", con)

	writeLines("", con)

	# modules
	writeLines("\t<modules>", con)

		writeLines("\t\t<moduletags>", con)
			writeLines("\t\t\t<id>#</id>", con)
			writeLines("\t\t\t<name>Name</name>", con)

			if ( dim(biclusters@seeddata)[1] > 0 ) {
				temp <- colnames(biclusters@seeddata)
				# replace unallowed characters by underscores
				tempp <- gsub("[^[:alnum:]]", "_", temp)
				if ( length(temp) >= 1 ) {	
					for ( i in 1:length(temp) ) {
						writeLines(paste("\t\t\t<", tempp[i], ">", temp[i], "</", tempp[i], ">", sep=""), con)
					}
				}
			}
		writeLines("\t\t</moduletags>", con)

		writeLines("", con)
	
		writeLines("\t\t<gotags>", con)
			writeLines("\t\t\t<id>#</id>", con)
			writeLines("\t\t\t<go>GO</go>", con)
			writeLines("\t\t\t<term>Term</term>", con)
			writeLines("\t\t\t<ontology>Ontology</ontology>", con)
			#writeLines("\t\t\t<definition>Definition</definition>", con)
			writeLines("\t\t\t<pvalue>PValue</pvalue>", con)
			writeLines("\t\t\t<oddsratio>OddsRatio</oddsratio>", con)
			writeLines("\t\t\t<expcount>ExpCount</expcount>", con)
			writeLines("\t\t\t<count>Count</count>", con)
			writeLines("\t\t\t<size>Size</size>", con)
		writeLines("\t\t</gotags>", con)

		writeLines("\t\t<keggtags>", con)
			writeLines("\t\t\t<id>#</id>", con)
			writeLines("\t\t\t<kegg>KEGG</kegg>", con)
			writeLines("\t\t\t<pathname>Path Name</pathname>", con)
			writeLines("\t\t\t<pvalue>PValue</pvalue>", con)
			writeLines("\t\t\t<oddsratio>OddsRatio</oddsratio>", con)
			writeLines("\t\t\t<expcount>ExpCount</expcount>", con)
			writeLines("\t\t\t<count>Count</count>", con)
			writeLines("\t\t\t<size>Size</size>", con)
		writeLines("\t\t</keggtags>", con)

	writeLines("", con)

	for ( module in 1:nBiclusters ) {
		writeLines("\t\t<module>", con)

			writeLines(paste("\t\t\t<id>", module, "</id>", sep=""), con)
			writeLines(paste("\t\t\t<name>module ", module, "</name>", sep=""), con)
			if ( dim(biclusters@seeddata)[1] > 0 ) {
				temp <- colnames(biclusters@seeddata)
				# replace unallowed characters by underscores
				temp <- gsub("[^[:alnum:]]", "_", temp)
				if ( length(temp) != 0 ) {
					tempp <- biclusters@seeddata[module,]
					for ( i in 1:length(temp) ) {
						value <- tempp[i]
						if ( temp[i] == "rob" || temp[i] == "rob_limit" ) {
							value <- formatter(as.numeric(value))
						}
						writeLines(paste("\t\t\t<", temp[i], ">", value, "</", temp[i], ">", sep=""), con)
					}
				}
			}

			intersectingbiclustersgenes = list();
			genes <- biclusters@genes[,module][geneMaps[[1]]]
			for ( modulep in 1:nBiclusters ) {
				if ( sum(genes * biclusters@genes[,modulep][geneMaps[[1]]]) != 0 && module != modulep ) {
					intersectingbiclustersgenes <- append(intersectingbiclustersgenes, modulep)
				}
			}

			genesp <- match(as.vector(which(biclusters@genes[,module]!=0)),geneMaps[[1]])[geneMaps[[module+1]]]
			writeLines(paste("\t\t\t<containedgenes>", toString(genesp), "</containedgenes>", sep=""), con)
			scores <- as.array(as.real(genes[genesp]))
			scores <- apply(scores, 1, formatter)
			writeLines(paste("\t\t\t<genescores>", toString(scores), "</genescores>", sep=""), con)

			intersectingbiclusterssamples = list();
			samples <- biclusters@conditions[,module][sampleMaps[[1]]]
			for ( modulep in 1:nBiclusters ) {
				if ( sum(samples * biclusters@conditions[,modulep][sampleMaps[[1]]]) != 0 && module != modulep ) {
					intersectingbiclusterssamples <- append(intersectingbiclusterssamples, modulep)
				}
			}

			samplesp <- match(as.vector(which(biclusters@conditions[,module]!=0)),sampleMaps[[1]])[sampleMaps[[module+1]]]
			writeLines(paste("\t\t\t<containedsamples>", toString(samplesp), "</containedsamples>", sep=""), con)
			scores <- as.array(as.real(samples[samplesp]))
			scores <- apply(scores, 1, formatter)
			writeLines(paste("\t\t\t<samplescores>", toString(scores), "</samplescores>", sep=""), con)

			intersectingbiclusters <- intersect(unique(intersectingbiclustersgenes), unique(intersectingbiclusterssamples))
			writeLines(paste("\t\t\t<intersectingmodules>", toString(intersectingbiclusters), "</intersectingmodules>", sep=""), con)

			writeLines("", con)

			writeLines("\t\t\t<gos>", con)
			k <- 1
			for ( i in 1:3 ) {
				s <- summary(gos[[i]])[[module]]
				if ( dim(s)[1] > 0 ) {
					temp <- match(rownames(s), go.table[,1])
					
					for ( j in 1:dim(s)[1] ) {
						writeLines("\t\t\t\t<go>", con)
							writeLines(paste("\t\t\t\t\t<id>", k, "</id>", sep=""), con)
							k <- k + 1
							writeLines(paste("\t\t\t\t\t<go>", rownames(s)[j], "</go>", sep=""), con)
							writeLines(paste("\t\t\t\t\t<term>", go.table[temp[j], 3], "</term>", sep=""), con)
							writeLines(paste("\t\t\t\t\t<ontology>", go.table[temp[j], 4], "</ontology>", sep=""), con)
							#writeLines(paste("\t\t\t\t\t<definition>", go.table[temp[j], 5], "</definition>", sep=""), con)
							writeLines(paste("\t\t\t\t\t<pvalue>", formatter(s[j, 1]), "</pvalue>", sep=""), con)
							writeLines(paste("\t\t\t\t\t<oddsratio>", formatter(s[j, 2]), "</oddsratio>", sep=""), con)
							writeLines(paste("\t\t\t\t\t<expcount>", formatter(s[j, 3]), "</expcount>", sep=""), con)
							writeLines(paste("\t\t\t\t\t<count>", s[j, 4], "</count>", sep=""), con)
							writeLines(paste("\t\t\t\t\t<size>", s[j, 5], "</size>", sep=""), con)
						writeLines("\t\t\t\t</go>", con)
					}
				}
			}
			writeLines("\t\t\t</gos>", con)

			writeLines("", con)

			writeLines("\t\t\t<keggs>", con)
			s <- summary(keggs)[[module]]
			if ( dim(s)[1] > 0 ) {
				temp <- match(rownames(s), kegg.table[,1])
				for ( j in 1:dim(s)[1] ) {
					writeLines("\t\t\t\t<kegg>", con)
						writeLines(paste("\t\t\t\t\t<id>", j, "</id>", sep=""), con)
						writeLines(paste("\t\t\t\t\t<kegg>", rownames(s)[j], "</kegg>", sep=""), con)
						writeLines(paste("\t\t\t\t\t<pathname>", kegg.table[temp, 2][j], "</pathname>", sep=""), con)
						writeLines(paste("\t\t\t\t\t<pvalue>", formatter(s[j, 1]), "</pvalue>", sep=""), con)
						writeLines(paste("\t\t\t\t\t<oddsratio>", formatter(s[j, 2]), "</oddsratio>", sep=""), con)
						writeLines(paste("\t\t\t\t\t<expcount>", formatter(s[j, 3]), "</expcount>", sep=""), con)
						writeLines(paste("\t\t\t\t\t<count>", s[j, 4], "</count>", sep=""), con)
						writeLines(paste("\t\t\t\t\t<size>", s[j, 5], "</size>", sep=""), con)
					writeLines("\t\t\t\t</kegg>", con)
				}
			}
			writeLines("\t\t\t</keggs>", con)
		writeLines("\t\t</module>", con)
		
		}
		
	writeLines("\t</modules>", con)

	#norm <- match.arg(norm)
	Data <- eisa:::select.eset(eset, biclusters, norm)
	Data <- Data[ geneMaps[[1]], sampleMaps[[1]] ]
	Data <- as.vector(t(Data))
        
	Data.min <- min(Data)
	Data.max <- max(Data)
	Data.delta <- Data.max - Data.min

	Data <- (Data - Data.min) / Data.delta * 2 - 1
	Data <- as.integer(round(Data * 100, 0))

	writeLines("", con)

	# data
	writeLines("\t<data>", con)
	
		temp <- base64encode(writeBin(Data, raw(), size=1))
		# line breaks after 76 characters
		for ( i in 1:ceiling(nchar(temp) / 76) ) {
			writeLines(substr(temp, (i-1)*76 + 1, i*76), con)
		}
		
	writeLines("\t</data>", con)

	writeLines("</evf>", con)
	
	close(con)

}


########################################################################
# export list ##########################################################
########################################################################

setMethod("ExportEV", signature(biclusters="list"), 
	function(biclusters, eset, order, filename, norm, description) 
	ExportEV.list(biclusters, eset, order, filename, norm, description)
)

ExportEV.list <- function(biclusters, eset, order, filename, norm, description) {

	if ( missing(order) ) {
		order = OrderEV(biclusters)
	}

	if ( missing(filename) ) {
		con <- file(file.choose(TRUE), open="w")
	} else {
		con <- file(filename, open="w", blocking = TRUE)
	}

	geneMaps <- order$cols
	sampleMaps <- order$rows

	# get data description
	experimentdata <- description$experiment
	
	cnamesdata <- colnames(data)
	rnamesdata <- rownames(data)
	Genes <- cnamesdata[geneMaps[[1]]]
	Samples <- rnamesdata[sampleMaps[[1]]]
	
	geneLabels <- description$collabels
	geneLabelsData <- NULL
	if ( !is.null(description$coldata) ) {
		geneLabelsData <- as.matrix(description$coldata[Genes, rownames(geneLabels)])
	}
	sampleLabels <- description$rowlabels
	sampleLabelsData <- NULL
	if ( !is.null(description$rowdata) ) {
		sampleLabelsData <- as.matrix(description$rowdata[Samples, rownames(sampleLabels)])
	}
	
	nGenes <- dim(biclusters$columns)[1]
	nSamples <- dim(biclusters$rows)[1]
	nBiclusters <- dim(biclusters$rows)[2]

	writeLines("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", con)
	writeLines("<evf>", con)
	writeLines("\t<summary>", con)
	writeLines("\t\t<description>ExpressionView data file</description>", con)
	writeLines("\t\t<version>1.0</version>", con)
	writeLines(paste("\t\t<nmodules>", nBiclusters, "</nmodules>", sep=""), con)
	writeLines(paste("\t\t<ngenes>", nGenes, "</ngenes>", sep=""), con)
	writeLines(paste("\t\t<nsamples>", nSamples, "</nsamples>", sep=""), con)
	writeLines("\t</summary>", con)

	writeLines("", con)
	
	# experimentdata
	writeLines("\t<experimentdata>", con)
		writeLines(paste("\t\t<title>", experimentdata$title, "</title>", sep=""), con)
		writeLines(paste("\t\t<name>", experimentdata$name, "</name>", sep=""), con)
		writeLines(paste("\t\t<lab>", experimentdata$lab, "</lab>", sep=""), con)
		writeLines(paste("\t\t<abstract>", experimentdata$abstract, "</abstract>", sep=""), con)
		writeLines(paste("\t\t<url>", experimentdata$url, "</url>", sep=""), con)
		writeLines(paste("\t\t<annotation>", experimentdata$annotation, "</annotation>", sep=""), con)
		writeLines(paste("\t\t<organism>", experimentdata$organism, "</organism>", sep=""), con)
	writeLines("\t</experimentdata>", con)

	writeLines("", con)

	# genes
	writeLines("\t<genes>", con)

		writeLines("\t\t<genetags>", con)
			writeLines("\t\t\t<id>#</id>", con)
			writeLines("\t\t\t<score>Score</score>", con)
			writeLines("\t\t\t<name>Name</name>", con)

			if ( !is.null(geneLabels) ) {
				temp <- rownames(geneLabels)
				# replace unallowed characters by underscores
				temp <- gsub("[^[:alnum:]]", "_", temp)
				tempp <- geneLabels[,1]
				if ( length(temp) >= 1 ) {	
					for ( i in 1:length(temp) ) {
						writeLines(paste("\t\t\t<", temp[i], ">", tempp[i], "</", temp[i], ">", sep=""), con)
					}
				}
			}
			
		writeLines("\t\t</genetags>", con)

		writeLines("", con)

		for ( gene in 1:nGenes ) {
			writeLines("\t\t<gene>", con)
				writeLines(paste("\t\t\t<id>", gene, "</id>", sep=""), con)
				writeLines("\t\t\t<score/>", con)
				writeLines(paste("\t\t\t<name>", Genes[gene], "</name>", sep=""), con)

				if ( !is.null(geneLabels) ) {
					temp <- rownames(geneLabels)
					# replace unallowed characters by underscores
					temp <- gsub("[^[:alnum:]]", "_", temp)
					tempp <- geneLabelsData[gene, ]
					for ( i in 1:length(temp) ) {
						writeLines(paste("\t\t\t<", temp[i], ">", xmlconf(tempp[i]), "</", temp[i], ">", sep=""), con)
					}
				}

			writeLines("\t\t</gene>", con)
		}
			
	writeLines("\t</genes>", con)

	# samples  
	writeLines("\t<samples>", con)

		writeLines("\t\t<sampletags>", con)
			writeLines("\t\t\t<id>#</id>", con)
			writeLines("\t\t\t<score>Score</score>", con)
			writeLines("\t\t\t<name>Name</name>", con)

			if ( !is.null(sampleLabels) ) {
				temp <- rownames(sampleLabels)
				# replace unallowed characters by underscores
				temp <- gsub("[^[:alnum:]]", "_", temp)
				tempp <- sampleLabels[,1]
				if ( length(temp) >= 1 ) {	
					for ( i in 1:length(temp) ) {
						writeLines(paste("\t\t\t<", temp[i], ">", tempp[i], "</", temp[i], ">", sep=""), con)
					}
				}
			}

		writeLines("\t\t</sampletags>", con)

		writeLines("", con)
		
		for ( sample in 1:nSamples ) {
			
			writeLines("\t\t<sample>", con)
				writeLines(paste("\t\t\t<id>", sample, "</id>", sep=""), con)
				writeLines("\t\t\t<score/>", con)
				writeLines(paste("\t\t\t<name>", Samples[sample], "</name>", sep=""), con)

				if ( !is.null(sampleLabels) ) {
					temp <- rownames(sampleLabels)
					# replace unallowed characters by underscores
					temp <- gsub("[^[:alnum:]]", "_", temp)
					tempp <- sampleLabelsData[sample, ]
					for ( i in 1:length(temp) ) {
						writeLines(paste("\t\t\t<", temp[i], ">", xmlconf(tempp[i]), "</", temp[i], ">", sep=""), con)
					}
				}


			writeLines("\t\t</sample>", con)
		}
	writeLines("\t</samples>", con)

	writeLines("", con)

	# modules
	writeLines("\t<modules>", con)

		writeLines("\t\t<moduletags>", con)
			writeLines("\t\t\t<id>#</id>", con)
			writeLines("\t\t\t<name>Name</name>", con)

			if ( dim(biclusters$seeddata)[1] > 0 ) {
				temp <- colnames(biclusters$seeddata)
				# replace unallowed characters by underscores
				tempp <- gsub("[^[:alnum:]]", "_", temp)
				if ( length(temp) >= 1 ) {	
					for ( i in 1:length(temp) ) {
						writeLines(paste("\t\t\t<", tempp[i], ">", temp[i], "</", tempp[i], ">", sep=""), con)
					}
				}
			}
		writeLines("\t\t</moduletags>", con)
	
		for ( module in 1:nBiclusters ) {
			writeLines("\t\t<module>", con)

				writeLines(paste("\t\t\t<id>", module, "</id>", sep=""), con)
				writeLines(paste("\t\t\t<name>module ", module, "</name>", sep=""), con)
				if ( dim(biclusters$seeddata)[1] > 0 ) {
					temp <- colnames(biclusters$seeddata)
					# replace unallowed characters by underscores
					temp <- gsub("[^[:alnum:]]", "_", temp)
					if ( length(temp) != 0 ) {
						tempp <- biclusters$seeddata[module,]
						for ( i in 1:length(temp) ) {
							value <- tempp[i]
							if ( temp[i] == "rob" || temp[i] == "rob_limit" ) {
								value <- formatter(as.numeric(value))
							}
							writeLines(paste("\t\t\t<", temp[i], ">", value, "</", temp[i], ">", sep=""), con)
						}
					}
				}

				intersectingbiclustersgenes = list();
				genes <- biclusters$columns[,module][geneMaps[[1]]]
				for ( modulep in 1:nBiclusters ) {
					if ( sum(genes * biclusters$columns[,modulep][geneMaps[[1]]]) != 0 && module != modulep ) {
						intersectingbiclustersgenes <- append(intersectingbiclustersgenes, modulep)
					}
				}

				genesp <- match(as.vector(which(biclusters$columns[,module]!=0)),geneMaps[[1]])[geneMaps[[module+1]]]
				writeLines(paste("\t\t\t<containedgenes>", toString(genesp), "</containedgenes>", sep=""), con)
				scores <- as.array(as.real(genes[genesp]))
				scores <- apply(scores, 1, formatter)
				writeLines(paste("\t\t\t<genescores>", toString(scores), "</genescores>", sep=""), con)

				intersectingbiclusterssamples = list();
				samples <- biclusters$rows[,module][sampleMaps[[1]]]
				for ( modulep in 1:nBiclusters ) {
					if ( sum(samples * biclusters$rows[,modulep][sampleMaps[[1]]]) != 0 && module != modulep ) {
						intersectingbiclusterssamples <- append(intersectingbiclusterssamples, modulep)
					}
				}

				samplesp <- match(as.vector(which(biclusters$rows[,module]!=0)),sampleMaps[[1]])[sampleMaps[[module+1]]]
				writeLines(paste("\t\t\t<containedsamples>", toString(samplesp), "</containedsamples>", sep=""), con)
				scores <- as.array(as.real(samples[samplesp]))
				scores <- apply(scores, 1, formatter)
				writeLines(paste("\t\t\t<samplescores>", toString(scores), "</samplescores>", sep=""), con)

				intersectingbiclusters <- intersect(unique(intersectingbiclustersgenes), unique(intersectingbiclusterssamples))
				writeLines(paste("\t\t\t<intersectingmodules>", toString(intersectingbiclusters), "</intersectingmodules>", sep=""), con)

				writeLines("", con)

		writeLines("\t\t</module>", con)
		
		}
		
	writeLines("\t</modules>", con)

	Data <- t(isa.normalize(as.matrix(data))[[1]])
	Data[is.na(Data)] <- 0
	Data <- Data[sampleMaps[[1]], geneMaps[[1]]]
	Data <- as.vector(Data)
        
	Data.min <- min(Data, na.rm = TRUE)
	Data.max <- max(Data, na.rm = TRUE)
	Data.delta <- Data.max - Data.min
	
	if ( Data.min < 0 && Data.max > 0 ) {
		Data[Data < 0] <- - Data[Data < 0] / Data.min
		Data[Data > 0] <- Data[Data > 0] / Data.max
	} else {
		Data <- (Data - Data.min) / Data.delta * 2 - 1
	}
	
	# scale data if necessary
	h<-hist(abs(Data), plot=FALSE, breaks = 510)
	h <- h$counts
	total <- sum(h)
	for ( i in 1:length(h) ) {
		if ( sum(h[1:i]) > 0.95*total ) {
			break
		}
	}
	Data <- Data / (i / length(h))
	
	Data <- as.integer(round(Data * 100, 0))

	writeLines("", con)

	# data
	writeLines("\t<data>", con)
	
		temp <- base64encode(writeBin(Data, raw(), size=1))
		# line breaks after 76 characters
		for ( i in 1:ceiling(nchar(temp) / 76) ) {
			writeLines(substr(temp, (i-1)*76 + 1, i*76), con)
		}
		
	writeLines("\t</data>", con)

	writeLines("</evf>", con)
	
	close(con)

}

xmlconf <- function(s) {
	res <- gsub("<", " .lt. ", s)
	res <- gsub(">", " .gt. ", res)
	res
}

formatter <- function(x) {
	if ( !is.na(x) ) { 
		if ( abs(x) < 1e-2 ) {
			res <- formatC(x, digits=2, format="e")
		} else {
			res <- formatC(x, digits=2, format="f")
		}
	} else {
		res <- formatC(0., digits=2, format="f")
	}
	res
}
