########################################################################
# export Biclust #######################################################
########################################################################

if (require(biclust)) {
 	setMethod("export.ev", signature(modules="Biclust"), function(modules, ...) export.ev.Biclust(modules, ...))
}

export.ev.Biclust <- function(modules, eset, order, go=NULL, kegg=NULL, filename="", norm=c("sample", "feature", "raw")) {
	eisamodules <- as(modules, "ISAModules")
	eisamodules@rundata$annotation <- annotation(eset)
	eisamodules@rundata$prenormalize <- FALSE
	export.ev(eisamodules, eset, order, go, kegg, filename, norm)
}


########################################################################
# export EISA ##########################################################
########################################################################

if (require(eisa)) {
	setMethod("export.ev", signature(modules="ISAModules"), function(modules, ...) export.ev.default(modules, ...))
}

export.ev.default <- function(modules, eset, order, go=NULL, kegg=NULL, filename="", norm=c("sample", "feature", "raw")) {
	
	library(caTools)
	
	if ( filename == "" ) {
		con <- file(file.choose(TRUE), open="w")
	} else {
		con <- file(filename, open="w", blocking = TRUE)
	}
		
	geneMaps <- order$genes
	sampleMaps <- order$samples
	
	Genes <- featureNames(modules)[geneMaps[[1]]];
	Samples <- sampleNames(modules)[sampleMaps[[1]]];
		
	nGenes <- (dim(modules))[1]
	nSamples <- (dim(modules))[2]
	nModules <- length(modules)

	writeLines("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", con)
	writeLines("<ged>", con)
	writeLines("\t<summary>", con)
	writeLines("\t\t<description>ExpressionView data file</description>", con)
	writeLines("\t\t<version>1.0</version>", con)
	writeLines(paste("\t\t<nmodules>", nModules, "</nmodules>", sep=""), con)
	writeLines(paste("\t\t<ngenes>", nGenes, "</ngenes>", sep=""), con)
	writeLines(paste("\t\t<nsamples>", nSamples, "</nsamples>", sep=""), con)
	writeLines("\t</summary>", con)

	library(GO.db)
	go.table <- toTable(GOTERM)
	if ( is.null(go) ) {
		gos <- ISA.GO(modules)
	} else {
		gos <- go
	}

	library(KEGG.db)
	kegg.table <- toTable(KEGGPATHID2NAME)
	if ( is.null(kegg) ) {
		keggs <- ISA.KEGG(modules)
	} else {
		keggs <- kegg
	}
		
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

			if ( dim(modules@seeddata)[1] > 0 ) {
				temp <- colnames(modules@seeddata)
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

	for ( module in 1:nModules ) {
		writeLines("\t\t<module>", con)

			writeLines(paste("\t\t\t<id>", module, "</id>", sep=""), con)
			writeLines(paste("\t\t\t<name>module ", module, "</name>", sep=""), con)
			if ( dim(modules@seeddata)[1] > 0 ) {
				temp <- colnames(modules@seeddata)
				# replace unallowed characters by underscores
				temp <- gsub("[^[:alnum:]]", "_", temp)
				if ( length(temp) != 0 ) {
					tempp <- modules@seeddata[module,]
					for ( i in 1:length(temp) ) {
						value <- tempp[i]
						if ( temp[i] == "rob" || temp[i] == "rob_limit" ) {
							value <- formatter(as.numeric(value))
						}
						writeLines(paste("\t\t\t<", temp[i], ">", value, "</", temp[i], ">", sep=""), con)
					}
				}
			}

			intersectingmodulesgenes = list();
			genes <- modules@genes[,module][geneMaps[[1]]]
			for ( modulep in 1:nModules ) {
				if ( sum(genes * modules@genes[,modulep][geneMaps[[1]]]) != 0 && module != modulep ) {
					intersectingmodulesgenes <- append(intersectingmodulesgenes, modulep)
				}
			}

			genesp <- match(as.vector(which(modules@genes[,module]!=0)),geneMaps[[1]])[geneMaps[[module+1]]]
			writeLines(paste("\t\t\t<containedgenes>", toString(genesp), "</containedgenes>", sep=""), con)
			scores <- as.array(as.real(genes[genesp]))
			scores <- apply(scores, 1, formatter)
			writeLines(paste("\t\t\t<genescores>", toString(scores), "</genescores>", sep=""), con)

			intersectingmodulessamples = list();
			samples <- modules@conditions[,module][sampleMaps[[1]]]
			for ( modulep in 1:nModules ) {
				if ( sum(samples * modules@conditions[,modulep][sampleMaps[[1]]]) != 0 && module != modulep ) {
					intersectingmodulessamples <- append(intersectingmodulessamples, modulep)
				}
			}

			samplesp <- match(as.vector(which(modules@conditions[,module]!=0)),sampleMaps[[1]])[sampleMaps[[module+1]]]
			writeLines(paste("\t\t\t<containedsamples>", toString(samplesp), "</containedsamples>", sep=""), con)
			scores <- as.array(as.real(samples[samplesp]))
			scores <- apply(scores, 1, formatter)
			writeLines(paste("\t\t\t<samplescores>", toString(scores), "</samplescores>", sep=""), con)

			intersectingmodules <- intersect(unique(intersectingmodulesgenes), unique(intersectingmodulessamples))
			writeLines(paste("\t\t\t<intersectingmodules>", toString(intersectingmodules), "</intersectingmodules>", sep=""), con)

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


	norm <- match.arg(norm)
	Data <- eisa:::select.eset(eset, modules, norm)
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

	writeLines("</ged>", con)
	
	close(con)

}

formatter <- function(x) {
	if ( abs(x) < 1e-2 ) {
		res <- formatC(x, digits=2, format="e")
	} else {
		res <- formatC(x, digits=2, format="f")	
	}
	res
}
