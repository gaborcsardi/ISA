\name{ListHyperGParams-class}
\docType{class}

\alias{ListHyperGParams}
\alias{CHRListHyperGParams}
\alias{GOListHyperGParams}
\alias{KEGGListHyperGParams}
\alias{miRNAListHyperGParams}
\alias{GeneralListHyperGParams}

\alias{ListHyperGParams-class}
\alias{CHRListHyperGParams-class}
\alias{GOListHyperGParams-class}
\alias{KEGGListHyperGParams-class}
\alias{miRNAListHyperGParams-class}
\alias{GeneralListHyperGParams-class}

\alias{drive}
\alias{drive<-}
\alias{drive,ListHyperGParams-method}
\alias{drive,GOListHyperGParams-method}
\alias{drive,KEGGListHyperGParams-method}
\alias{drive,CHRListHyperGParams-method}
\alias{drive,miRNAListHyperGParams-method}
\alias{drive,GeneralListHyperGParams-method}
\alias{drive<-,ListHyperGParams,logical-method}
\alias{drive<-,GOListHyperGParams,logical-method}
\alias{drive<-,KEGGListHyperGParams,logical-method}
\alias{drive<-,CHRListHyperGParams,logical-method}
\alias{drive<-,miRNAListHyperGParams,logical-method}
\alias{drive<-,GeneralListHyperGParams,logical-method}

\alias{hyperGTest,ListHyperGParams-method}
\alias{hyperGTest,CHRListHyperGParams-method}
\alias{hyperGTest,KEGGListHyperGParams-method}
\alias{hyperGTest,GOListHyperGParams-method}
\alias{hyperGTest,miRNAListHyperGParams-method}
\alias{hyperGTest,GeneralListHyperGParams-method}

\alias{makeValidParams,ListHyperGParams-method}
\alias{makeValidParams,CHRListHyperGParams-method}
\alias{makeValidParams,KEGGListHyperGParams-method}
\alias{makeValidParams,GOListHyperGParams-method}
\alias{makeValidParams,miRNAListHyperGParams-method}
\alias{makeValidParams,GeneralListHyperGParams-method}

\alias{conditional,GOListHyperGParams-method}
\alias{conditional<-,GOListHyperGParams,logical-method}

\alias{ontology<-,GOListHyperGParams,character-method}
\alias{ontology,GOListHyperGParams-method}

\title{Classes for quick GO/KEGG/CHR/miRNA target or other enrichment
  calculation of multiple gene sets.}
\description{
  These classes extend the \code{HyperGParams}
  class from the \code{Category} package to perform enrichment
  calculation quickly for multiple gene sets.
}
\usage{
\S4method{makeValidParams}{ListHyperGParams}(object)
\S4method{drive}{ListHyperGParams}(p)
\S4method{drive}{ListHyperGParams,logical}(p) <- dri

\S4method{ontology}{GOListHyperGParams}(object)
\S4method{ontology}{GOListHyperGParams,character}(object) <- go
\S4method{conditional}{GOListHyperGParams}(r)
\S4method{conditional}{GOListHyperGParams,logical}(r) <- cond

\S4method{hyperGTest}{ListHyperGParams}(p)
}
\arguments{
  \item{object,p,r}{A \code{ListHyperGParams} object.}
  \item{dri}{Logical scalar, whether to store the genes that are in the
    intersection of the specified gene set and the annotation category.}
  \item{go}{Character scalar, the ontology for GO, possible values:
    \sQuote{BP}, \sQuote{CC}, \sQuote{MF}.}
  \item{cond}{Logical scalar, whether to perform conditional enrichment
    calculation. Currently this option is ignored.}
}
\details{
  The \code{ListHyperGParams} abstract class extends \code{HyperGParams}
  and allows to specify a list of gene sets for the enrichment
  calculation instead of a single set.

  \code{ListHyperGParams} calculates the enrichment much faster than the
  original \code{HyperGParams} classes in the \code{Category} package,
  especially if the calculation is performed against the same gene
  universe for many gene sets.

  \code{ListHyperGParams} is an abstract class, it is not possible to
  instantiate objects from it. Instead, its various extensions must be
  used: \code{GOListHyperGParams}, \code{KEGGListHyperGParams},
  \code{CHRListHyperGParams} and \code{miRNAListHyperGParams}.

  The various \code{ListHyperGParams} objects can be created with the
  standard \code{new} command, by giving all necessary arguments. Please
  see the examples below.
}
\section{Member functions}{
  Most of these functions are analogous to the ones defined in the
  \code{Category} package, the only difference is that they handle
  \code{ListHyperGParams} objects.

  \code{makeValidParams} validates \code{ListHyperGParams} object, in
  particular, it removes duplicate genes, both from the gene universe
  and the specified gene sets; and it also makes sure that all genes in
  the gene sets are included in the universe.

  \code{ontology} can be used to query or set the ontology for
  enrichment calculated against the GO database.

  \code{conditional} queries or sets whether conditional GO enrichment
  will be performed. This feature is not implemented yet, see the
  \code{Category} and \code{GOstats} packages for a working
  implementation and more information.

  \code{drive} queries or sets whether the intersections of the gene sets
  and the universe are stored in the result object. This information can
  be calculated later as well, but it is faster to store it at the same
  time when the hypergeometric test is performed.
}
\value{
  \code{makeValidParmas} returns another \code{ListHyperGParams}
  instance that has the same class as its arguments'.

  \code{ontology} returns a character vector of length one.

  \code{conditional} returns a logical vector of length one.
  
  \code{drive} returns a logical vector of length one.  
}
\author{ Gabor Csardi \email{csardi.gabor@gmail.com} }
\seealso{Functions for enrichment calculation of ISA modules:
  \code{\link{ISAGO}}, \code{\link{ISAKEGG}}, \code{\link{ISACHR}},
  \code{\link{ISAmiRNA}}.
  
  Perhaps see also the vignette in the \code{GOstats} package.
}
\examples{
# GO enrichment, "by hand"
# Load data first
data(ALLModulesSmall)

# Create gene sets
library(hgu95av2.db)
genes <- getFeatureNames(ALLModulesSmall)
entrez <- lapply(genes, function(x) na.omit(unlist(mget(x,
              hgu95av2ENTREZID))))

# Create universe
universe <- na.omit(unlist(mget(featureNames(ALLModulesSmall),
              hgu95av2ENTREZID)))

# Create parameter object
param <- new("GOListHyperGParams", geneIds=entrez, universeGeneIds=universe,
             pvalueCutoff=0.01, drive=FALSE, ontology="BP",
             conditional=FALSE, testDirection="over",
             annotation=annotation(ALLModulesSmall))

# Do the calculation
GOBP <- hyperGTest(param)

# Inspect the result
GOBP
summary(GOBP)[[1]]

# How to create other parameter objects
paramKEGG <- new("KEGGListHyperGParams", geneIds=entrez,
                 universeGeneIds=universe, drive=FALSE,
                 annotation=annotation(ALLModulesSmall))
paramCHR <- new("CHRListHyperGParams", geneIds=entrez,
                 universeGeneIds=universe, drive=FALSE,
                 annotation=annotation(ALLModulesSmall))

# Enrichment with user-supplied categories, we use a list of
# hand-picked genes that are involved in myelin formation
mygenes <- c("YARS", "NFKB2", "NGFR", "CDH1", "NFAT5", "NDRG1", "GAP43", 
 "EGR2", "MSN", "ROCK1", "SREBF2", "SOX10", "FIG4", "EGR1", "PIK3R1", 
 "CDC42", "EDN3", "EDNRB", "NCAM1", "DHH", "OMG", "PMP22", "LAMA4", 
 "MPDZ", "MTMR2", "REL", "S100A1", "ITGA4", "GFAP", "FGF2", "RPSA",
 "CADM1", "CDH19", "DNM2", "PAX3", "SREBF1", "DAG1", "DRP2", "SDC2",
 "MBP", "RELA", "RELB", "JUN", "NAB1", "MOBP", "SKI",  "COL5A2", "RHOA",
 "NFASC", "NEFL", "MPZ", "MAG", "EDNRA", "ERBB4", "LITAF", "MMP2",
 "PLP1", "CDKN1A", "PAK1", "RDX", "GJB1", "LAMA5", "JAM3", "ITGB1",
 "PARD3", "FABP7", "LAMA2", "ERBB3", "CADM4", "FOXO4", "TSPAN31",
 "GPR126", "PTK2", "RAC1", "CDKN2A", "CLDN5", "ID2", "LAMC1", "SOX2",
 "CNTN2", "ERBB2", "NFKB1", "NAB2", "EDN2", "MMP9", "CCND1", "L1CAM",
 "MOG") 

library(org.Hs.eg.db)
myentrez <- na.omit(unlist(mget(mygenes, revmap(org.Hs.egSYMBOL))))
categories <- list(myelin=myentrez)

data(ALLModules)
genes2 <- getFeatureNames(ALLModules)
entrez2 <- lapply(genes2, function(x) na.omit(unlist(mget(x,
              hgu95av2ENTREZID))))

# Create universe
universe2 <- na.omit(unlist(mget(featureNames(ALLModules),
              hgu95av2ENTREZID)))

paramMY <- new("GeneralListHyperGParams", geneIds=entrez2,
               universeGeneIds=universe2, drive=FALSE,
               annotation=annotation(ALLModulesSmall),
               categories=categories)
MY <- hyperGTest(paramMY)
MY
summary(MY)[[1]]
}
\keyword{cluster}
