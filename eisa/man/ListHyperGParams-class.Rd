\name{ListHyperGParams-class}
\docType{class}

\alias{ListHyperGParams}
\alias{CHRListHyperGParams}
\alias{GOListHyperGParams}
\alias{KEGGListHyperGParams}
\alias{miRNAListHyperGParams}

\alias{ListHyperGParams-class}
\alias{CHRListHyperGParams-class}
\alias{GOListHyperGParams-class}
\alias{KEGGListHyperGParams-class}
\alias{miRNAListHyperGParams-class}

\alias{drive}
\alias{drive<-}
\alias{drive,ListHyperGParams-method}
\alias{drive,GOListHyperGParams-method}
\alias{drive,KEGGListHyperGParams-method}
\alias{drive,CHRListHyperGParams-method}
\alias{drive,miRNAListHyperGParams-method}
\alias{drive<-,ListHyperGParams,logical-method}
\alias{drive<-,GOListHyperGParams,logical-method}
\alias{drive<-,KEGGListHyperGParams,logical-method}
\alias{drive<-,CHRListHyperGParams,logical-method}
\alias{drive<-,miRNAListHyperGParams,logical-method}

\alias{hyperGTest,ListHyperGParams-method}
\alias{hyperGTest,CHRListHyperGParams-method}
\alias{hyperGTest,KEGGListHyperGParams-method}
\alias{hyperGTest,GOListHyperGParams-method}
\alias{hyperGTest,miRNAListHyperGParams-method}

\alias{makeValidParams,ListHyperGParams-method}
\alias{makeValidParams,CHRListHyperGParams-method}
\alias{makeValidParams,KEGGListHyperGParams-method}
\alias{makeValidParams,GOListHyperGParams-method}
\alias{makeValidParams,miRNAListHyperGParams-method}

\alias{conditional,GOListHyperGParams-method}
\alias{conditional<-,GOListHyperGParams,logical-method}

\alias{ontology<-,GOListHyperGParams,character-method}
\alias{ontology,GOListHyperGParams-method}

\title{Classes for quick GO/KEGG/CHR/miRNA target enrichment calculation
  of multiple gene sets.}
\description{
  These classes extend the \code{HyperGParams} and \code{HyperGResult} 
  classes from the \code{Category} package to perform enrichment
  calculation for quickly for multiple gene sets.
}
\usage{
\S4method{makeValidParams}{ListHyperGParams}(p)
\S4method{drive}{ListHyperGParams}(p)
\S4method{drive}{ListHyperGParams,logical}(p) <- dri

\S4method{ontology}{GOListHyperGParams}(p)
\S4method{ontology}{GOListHyperGParams,character}(p) <- go
\S4method{conditional}{GOListHyperGParams}(p)
\S4method{conditional}{GOListHyperGParams,logical}(p) <- cond

\S4method{hyperGTest}{ListHyperGParams}(p)
}
\arguments{
  \item{p}{A \code{ListHyperGParams} object.}
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

  \code{ListHyperGParams} calculates the enrichment must faster than the
  original \code{\link[Category]{HyperGParams}} classes, especially if
  the calculation is performed against the same gene universe for many
  gene sets.

  \code{ListHyperGParams} is an abstract class, it is not possible to
  instantiate objects from it. Instead, its various extensions must be
  used: \code{GOListHyperGParams}, \code{KEGGListHyperGParams},
  \code{CHRListHyperGParams} and \code{miRNAListHyperGParams}.
}
\section{Member functions}{
  Most of these functions are analogous to the ones defined in the
  \code{Category} package, the only difference is that they handle
  \code{ListHyperGParams} objects.

  \code{makeValidParams} validates \code{ListHyperGParams} object, in
  particular, it removed duplicate genes, both from the gene universe
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
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\seealso{Functions for enrichment calculation of ISA modules:
  \code{\link{ISAGO}}, \code{\link{ISAKEGG}}, \code{\link{ISACHR}},
  \code{\link{ISAmiRNA}}.
  
  Perhaps see also the vignette in the \code{GOstats} package.
}
\examples{}
\keyword{cluster}
