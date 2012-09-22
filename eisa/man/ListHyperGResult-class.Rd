\name{ListHyperGResult-class}
\docType{class}

\alias{ListHyperGResult}
\alias{CHRListHyperGResult}
\alias{GOListHyperGResult}
\alias{KEGGListHyperGResult}
\alias{miRNAListHyperGResult}
\alias{GeneralListHyperGResult}

\alias{ListHyperGResult-class}
\alias{CHRListHyperGResult-class}
\alias{GOListHyperGResult-class}
\alias{KEGGListHyperGResult-class}
\alias{miRNAListHyperGResult-class}
\alias{GeneralListHyperGResult-class}

\alias{condGeneIdUniverse,ListHyperGResult-method}
\alias{condGeneIdUniverse,CHRListHyperGResult-method}
\alias{condGeneIdUniverse,KEGGListHyperGResult-method}
\alias{condGeneIdUniverse,GOListHyperGResult-method}
\alias{condGeneIdUniverse,miRNAListHyperGResult-method}
\alias{condGeneIdUniverse,GeneralListHyperGResult-method}

\alias{conditional,GOListHyperGResult-method}

\alias{expectedCounts,ListHyperGResult-method}
\alias{expectedCounts,CHRListHyperGResult-method}
\alias{expectedCounts,KEGGListHyperGResult-method}
\alias{expectedCounts,GOListHyperGResult-method}
\alias{expectedCounts,miRNAListHyperGResult-method}
\alias{expectedCounts,GeneralListHyperGResult-method}

\alias{geneCounts,ListHyperGResult-method}
\alias{geneCounts,CHRListHyperGResult-method}
\alias{geneCounts,KEGGListHyperGResult-method}
\alias{geneCounts,GOListHyperGResult-method}
\alias{geneCounts,miRNAListHyperGResult-method}
\alias{geneCounts,GeneralListHyperGResult-method}

\alias{geneIdsByCategory,ListHyperGResult-method}
\alias{geneIdsByCategory,CHRListHyperGResult-method}
\alias{geneIdsByCategory,KEGGListHyperGResult-method}
\alias{geneIdsByCategory,GOListHyperGResult-method}
\alias{geneIdsByCategory,miRNAListHyperGResult-method}
\alias{geneIdsByCategory,GeneralListHyperGResult-method}

\alias{geneIdUniverse,ListHyperGResult-method}
\alias{geneIdUniverse,CHRListHyperGResult-method}
\alias{geneIdUniverse,KEGGListHyperGResult-method}
\alias{geneIdUniverse,GOListHyperGResult-method}
\alias{geneIdUniverse,miRNAListHyperGResult-method}
\alias{geneIdUniverse,GeneralListHyperGResult-method}

\alias{geneMappedCount,ListHyperGResult-method}
\alias{geneMappedCount,CHRListHyperGResult-method}
\alias{geneMappedCount,KEGGListHyperGResult-method}
\alias{geneMappedCount,GOListHyperGResult-method}
\alias{geneMappedCount,miRNAListHyperGResult-method}
\alias{geneMappedCount,GeneralListHyperGResult-method}

\alias{htmlReport,ListHyperGResult-method}
\alias{htmlReport,CHRListHyperGResult-method}
\alias{htmlReport,KEGGListHyperGResult-method}
\alias{htmlReport,GOListHyperGResult-method}
\alias{htmlReport,miRNAListHyperGResult-method}
\alias{htmlReport,GeneralListHyperGResult-method}

\alias{oddsRatios,ListHyperGResult-method}
\alias{oddsRatios,CHRListHyperGResult-method}
\alias{oddsRatios,KEGGListHyperGResult-method}
\alias{oddsRatios,GOListHyperGResult-method}
\alias{oddsRatios,miRNAListHyperGResult-method}
\alias{oddsRatios,GeneralListHyperGResult-method}

\alias{ontology,GOListHyperGResult-method}

\alias{pvalues,ListHyperGResult-method}
\alias{pvalues,CHRListHyperGResult-method}
\alias{pvalues,KEGGListHyperGResult-method}
\alias{pvalues,GOListHyperGResult-method}
\alias{pvalues,miRNAListHyperGResult-method}
\alias{pvalues,GeneralListHyperGResult-method}

\alias{sigCategories,ListHyperGResult-method}
\alias{sigCategories,CHRListHyperGResult-method}
\alias{sigCategories,KEGGListHyperGResult-method}
\alias{sigCategories,GOListHyperGResult-method}
\alias{sigCategories,miRNAListHyperGResult-method}
\alias{sigCategories,GeneralListHyperGResult-method}

\alias{summary,ListHyperGResult-method}
\alias{summary,CHRListHyperGResult-method}
\alias{summary,KEGGListHyperGResult-method}
\alias{summary,GOListHyperGResult-method}
\alias{summary,miRNAListHyperGResult-method}
\alias{summary,GeneralListHyperGResult-method}

\alias{universeCounts,ListHyperGResult-method}
\alias{universeCounts,CHRListHyperGResult-method}
\alias{universeCounts,KEGGListHyperGResult-method}
\alias{universeCounts,GOListHyperGResult-method}
\alias{universeCounts,miRNAListHyperGResult-method}
\alias{universeCounts,GeneralListHyperGResult-method}

\alias{universeMappedCount,ListHyperGResult-method}
\alias{universeMappedCount,CHRListHyperGResult-method}
\alias{universeMappedCount,KEGGListHyperGResult-method}
\alias{universeMappedCount,GOListHyperGResult-method}
\alias{universeMappedCount,miRNAListHyperGResult-method}
\alias{universeMappedCount,GeneralListHyperGResult-method}

\title{Classes for quick GO/KEGG/CHR/miRNA target or other enrichment
  calculation for multiple gene sets}
\description{
  These classes extend the \code{HyperGResult}
  class from the \code{Category} package to perform enrichment
  calculation quickly for multiple gene sets.
}
\usage{
\S4method{summary}{ListHyperGResult}(object, pvalue = pvalueCutoff(object), 
  categorySize = NULL)
\S4method{htmlReport}{ListHyperGResult}(r, file = "", append = FALSE,
  label = "", digits = 3, summary.args = NULL)
\S4method{pvalues}{ListHyperGResult}(r)
\S4method{sigCategories}{ListHyperGResult}(r, p)

\S4method{geneCounts}{ListHyperGResult}(r)
\S4method{expectedCounts}{ListHyperGResult}(r)
\S4method{oddsRatios}{ListHyperGResult}(r)
\S4method{universeCounts}{ListHyperGResult}(r)
\S4method{geneMappedCount}{ListHyperGResult}(r)
\S4method{universeMappedCount}{ListHyperGResult}(r)
\S4method{geneIdsByCategory}{ListHyperGResult}(r, catids = NULL)

\S4method{geneIdUniverse}{ListHyperGResult}(r, cond = FALSE)
}
\arguments{
  \item{object,r}{A \code{ListHyperGResult} object.}
  \item{pvalue,p}{Numeric vector of length one, the \eqn{p}-value limit,
    up to which the terms are listed.}
  \item{categorySize}{A numeric vector of length one, or \code{NULL}. If
    not \code{NULL}, then it gives the minimum number of annotated genes
    in the universe, in order to list the term.}
  \item{file}{A file name, or a connection object. The result is written
    here. If it is \code{""}, then the result is written to the standard
    output. If it is \code{NULL}, then the result is not written
    anywhere. (But it is always returned, invisibly, see below.)}
  \item{append}{Logical scalar, whether to append the HTML code to the
    given file, or remove its previous contents if it already exists.}
  \item{label}{An HTML label (\code{<A LABEL="">} tag) to add.}
  \item{digits}{The number of digits to use for the numeric columns.}
  \item{summary.args}{A list of arguments to pass to the \code{summary}
    method.}
  \item{catids}{The categories for which the genes are listed. All
    categories will be listed if this argument is \code{NULL}.}
  \item{cond}{Currently not used.}
}
\details{
  A \code{ListHyperGResult} object can store the results of
  hypergeometric tests, several gene sets against the same
  universe. \code{ListHyperGRresult} is an extension of
  \code{HyperGResult}, as defined in the \code{Category} package.

  More precisely, \code{ListHyperGResult} is an abstract class, it is
  not possible to instantiate objects from it. Its extensions are be
  used instead:
  \code{GOListHyperGResult}, \code{KEGGListHyperGResult},
  \code{CHRListHyperGResult} and \code{miRNAListHyperGResult}.  
}
\section{Member functions}{
  Most of the member functions are analogous to the ones defined for
  \code{HyperGResult} in the \code{Category} package. Usually the only
  difference is that they return a list of vectors, with one entry for
  each gene set, instead of just a single vector.

  \code{pvalues} returns the \eqn{p}-values of the hypergeomatric
  tests. A list is returned, with one numeric vector entry for each
  input gene set. The \eqn{p}-values for each gene set are ordered
  according to decreasing significance.

  \code{geneCounts} returns the number of genes from the gene set that
  are annotated with the given term. This is returned for all input gene
  sets, in a list.
  
  \code{expectedCounts} returns the number of genes that are expected to
  be annotated with the given term, just by chance. This is calculated
  for all input gene sets, and returned as a list.
  
  \code{oddsRatios} returns the odds ratios for each term tested, for
  all gene sets, in a list of numeric vectors.
  
  \code{universeCounts} returns the number of genes from the universe
  that are annotated with the given term, for all gene sets, in a list.

  \code{geneMappedCount} gives the size of the gene sets, as used in the
  algorithm. This can be different than the size of the input gene sets,
  because of the elimination of duplicates and genes that are not in the
  universe, before the actual computation.
  
  \code{universeMappedCount} gives the size of the gene universe, as
  used in the computation. This can be different than the size given by
  the user, because duplicates are eliminated before the computation.
  
  \code{sigCategories} returns the significant terms, at the given
  \eqn{p}-value threshold, for all gene sets, as a list.
  
  \code{geneIdsByCategory} returns a list of lists, one entry for each
  input gene set. Every entry is a list itself and for each tested term
  it gives the gene ids from the gene set that are annotated with the
  given term.

  \code{geneIdUniverse} returns a list of character vectors, one for
  each term that was tested, giving the ids of the genes from the
  universe that are annotated with that term.
  
  \code{summary} returns a list of data frames, one for each input gene
  set. Each data frame has columns: 
  \sQuote{Pvalue}, \sQuote{OddsRatio}, \sQuote{ExpCount},
  \sQuote{Count}, \sQuote{Size} and optionally \sQuote{drive}.
  Each row of the data frame corresponds to a tested term.
  
  \code{htmlReport} creates a HTML summary from a
  \code{ListHyperGParams} object. This consists of one table for each
  input gene get. The summary can be written to a file, but it is also
  returned in a list of character vectors. There is one list entry for
  each input gene set, and each element of the character vector
  corresponds to one line of HTML code. You need the \code{xtable}
  package to use this function.  

  The following functions are defined for \code{GOListHyperGResult}
  objects only.
  
  \code{conditional} returns a logical vector of length one, whether the
  test was conditional or not. Conditional testing is currently not
  implemented, please see the \code{GOstats} package for a working
  implementation.
  
  \code{ontology} returns a character vector of length one, the name of
  the ontology for the GO test.
}
\value{
  \code{pvalues}, \code{geneCounts}, \code{expectedCounts},
  \code{oddsRatios} and \code{universeCounts} return a list of named
  numeric vectors.

  \code{geneMappedCount} returns a numeric vector,
  \code{universeMappedCount} returns a numeric vector of length one.

  \code{sigCategories} returns a list of character vectors.

  \code{geneIdsByCategory} returns a list of lists of character vectors.

  \code{geneIdUniverse} returns a list of character vectors.

  \code{summary} returns a list of data frames with columns:
  \sQuote{Pvalue}, \sQuote{OddsRatio}, \sQuote{ExpCount},
  \sQuote{Count}, \sQuote{Size} and optionally \sQuote{drive}.

  \code{htmlReport} returns a list of chracter vectors, invisibly.

  \code{conditional} returns a logical vector of length one.
  \code{ontology} returns a character vector of length one.
}
\author{ Gabor Csardi \email{csardi.gabor@gmail.com} }
\seealso{Functions for enrichment calculation of ISA modules:
  \code{\link{ISAGO}}, \code{\link{ISAKEGG}}, \code{\link{ISACHR}},
  \code{\link{ISAmiRNA}}, \code{\link{ISAEnrichment}}.
  
  Perhaps see also the vignette in the \code{GOstats} package.
}
\examples{
data(ALLModulesSmall)
GO <- ISAGO(ALLModulesSmall)
GO$CC
sigCategories(GO$CC)[[1]]
summary(GO$CC)[[1]][,1:5]
}
\keyword{cluster}
