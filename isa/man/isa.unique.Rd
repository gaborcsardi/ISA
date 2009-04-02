\name{isa.unique}
\alias{isa.unique}
\title{Filter out biclusters that are very similar to each other}
\description{
  From a potentially non-unique set of ISA biclusters, create a unique
  set by removing all biclusters that are similar to others.
}
\synopsis{
isa.unique (normed.data, ...)
}
\usage{
isa.unique (normed.data, isaresult, method = c("cor"),
     ignore.div = TRUE, cor.limit = 0.9, neg.cor = TRUE, drop.zero = TRUE) 

}
\arguments{
  \item{normed.data}{The normalized input data, a list of two matrices,
    usually the output of \code{\link{isa.normalize}}.}
  \item{isaresult}{The result of an ISA run, a set of biclusters.}
  \item{method}{Character scalar giving the method to be used to
    determine if two biclusters are similar. Right now only
    \sQuote{\code{cor}} is implemented, this keeps both biclusters if
    their Pearson correlation is less than \code{cor.limit}, both for
    their row and column scores. See also the \code{neg.cor} argument.}
  \item{ignore.div}{Logical scalar, if \code{TRUE}, then the divergent
    biclusters will be removed.}
  \item{cor.limit}{Numeric scalar, giving the correlation limit for the
    \sQuote{\code{cor}} method.}
  \item{neg.cor}{Logical scalar, if \code{TRUE}, then the
    \sQuote{\code{cor}} method considers the absolute value of the
    correlation.}
  \item{drop.zero}{Logical scalar, whether to drop biclusters that have
    all zero scores.}
}
\details{
  Because of the nature of the ISA algorithm, the set of biclusters
  created by \code{\link{isa.iterate}} is not unique; many input seeds
  may converge to the same biclusters, even if the input seeds are not
  random.

  \code{isa.unique} filters a set of biclusters and removed the ones
  that are very similar to ones that were already found for another
  seed.
}
\value{
  A named list, the filtered \code{isaresult}. See the return value of
  \code{\link{isa.iterate}} for the details.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
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
\seealso{ \link{isa-package} for a short introduction on the Iterative
  Signature Algorithm. See \code{\link{isa}} for an easy way of running
  ISA.
}
\examples{
## Create an ISA module set
set.seed(1)
insili <- isa.in.silico(noise=0.01)

## Random seeds
seeds <- generate.seeds(length=nrow(insili[[1]]), count=20)

## Normalize input matrix
nm <- isa.normalize(insili[[1]])

## Do ISA
isares <- isa.iterate(nm, row.seeds=seeds, thr.row=2, thr.col=1)

## Check correlation among modules
cc <- cor(isares$rows)
if (interactive()) { hist(cc[lower.tri(cc)],10) }

## Some of them are quite high, how many?
undiag <- function(x) { diag(x) <- 0; x }
sum(undiag(cc) > 0.99, na.rm=TRUE)

## Eliminate duplicated modules
isares.unique <- isa.unique(nm, isares)

## How many modules left?
ncol(isares.unique$rows)

## Double check
cc2 <- cor(isares.unique$rows)
if (interactive()) { hist(cc2[lower.tri(cc2)],10) }

## High correlation?
sum(undiag(cc2) > 0.99, na.rm=TRUE)

}
\keyword{cluster}
