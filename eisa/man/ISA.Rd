\name{ISA}
\alias{ISA}
\concept{Iterative Signature Algorithm}
\concept{Biclustering}
\title{Iterative Signature Algorithm on Gene Expression data}
\description{Run ISA on an ExpressionSet with the default parameters.}
\usage{
ISA (data, flist = filterfun(function(x) IQR(x) > 0.5), 
    uniqueEntrez = TRUE, thr.gene = seq(2, 4, by = 0.5),
    thr.cond = seq(1, 3, by = 0.5), no.seeds = 100) 
}
\arguments{
  \item{data}{The input, an ExpressionSet object.}
  \item{flist}{A \sQuote{list} of filter functions to apply to the
    array. This is passed to the \code{\link[genefilter]{genefilter}}
    function without touching it. Supply \code{NA} here if you don't
    want to filter the expression set before running ISA on it.}
  \item{uniqueEntrez}{Logical scalar, whether to filter the input
    expression set to keep exactly one probeset for each Entrez
    gene. Probesets that are not mapped to an Entrez gene are
    dropped.}
  \item{thr.gene}{Numeric vector. The threshold parameters for the ISA,
    for features (=probesets or genes). All combinations of
    \code{thr.gene} and \code{thr.cond} 
    will be used to run ISA.} 
  \item{thr.cond}{Numeric vector. The threshold parameters for the ISA,
    for samples. All combinations of \code{thr.gene} and \code{thr.cond}
    will be used to run ISA.}
  \item{no.seeds}{Number of seeds to run ISA from.}
}
\details{
  Please read tutorial vignette included in this package for an
  introduction on ISA. The \code{isa2-package} manual page in the
  \code{isa2} package is also useful.

  The \code{ISA} function performs the ISA algorithm on the supplied
  expression data. This involves the following steps:
  \enumerate{
    \item Filtering the features (i.e. probe sets) according to their
    variance. You will need the \code{genefilter} package for
    this. The default filtering function keeps the features that have
    an \code{\link[stats]{IQR}} of 0.5 or more. See
    \code{\link[genefilter]{genefilter}} for details on how to create
    filtering functions. If \code{NA} is given as the \code{flist}
    argument, then no filtering is performed.
    \item Filtering the features by mapping them to Entrez
    genes. Features that do not map to Entrez genes are removed from
    the data set. If more features map to the same Entrez gene, then
    only the one with the highest variance will be kept.
    \item Calling the \code{\link[isa2]{isa}} function in the
    \code{isa2} package to perform the Iterative Signature
    Algorithm. This itself performs the following steps:
    \enumerate{
      \item Normalizing the data by calling
      \code{\link[isa2]{isa.normalize}}. 
      \item Generating random input seeds via
      \code{\link[isa2]{generate.seeds}}.
      \item Running ISA with all combinations of given feature and
      sample thresholds, by calling \code{\link[isa2]{isa.iterate}}. 
      \item Merging similar modules, separately for each threshold
      combination, by calling \code{\link[isa2]{isa.unique}}.
      \item Filtering the modules separately for each threshold combination,
      by calling \code{isa.filter.robust} in the \code{isa2} package.
      \item Putting all modules from the runs with different thresholds into
      a single object.
      \item Merging similar modules, across all threshold combinations, if
      two modules are similar, then the one with the
      milder thresholds is kept.
    }
    \item Creates an \code{ISAModules} object from the ISA
    results.
  }
}
\value{
  An \code{\link{ISAModules-class}} object.
}
\author{ Gabor Csardi \email{csardi.gabor@gmail.com} }
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
  
  Ihmels J, Friedlander G, Bergmann S, Sarig O, Ziv Y, Barkai N:
  Revealing modular organization in the yeast transcriptional network
  \emph{Nat Genet.} 2002 Aug;31(4):370-7. Epub 2002 Jul 22

  Ihmels J, Bergmann S, Barkai N:
  Defining transcription modules using large-scale gene expression data
  \emph{Bioinformatics} 2004 Sep 1;20(13):1993-2003. Epub 2004 Mar 25.
}
\seealso{
  The vignette included in the \code{eisa} package.
}
\examples{
library(ALL)
data(ALL)
modules <- ISA(ALL, thr.gene=2.7, thr.cond=1.4)
modules
}
\keyword{cluster}
