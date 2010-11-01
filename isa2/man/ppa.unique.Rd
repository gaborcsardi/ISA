\name{ppa.unique}
\alias{ppa.unique}
\alias{ppa.unique,list,list-method}
\title{Filter co-modules that are very similar to each other}
\description{
  From a potentially non-unique set of PPA co-modules, create a unique
  set by removing all co-modules that are similar to others.
}
\usage{
\S4method{ppa.unique}{list,list} (normed.data, pparesult, \dots)
}
\arguments{
  \item{normed.data}{The normalized input data, a list of four matrices,
    usually the output of the \code{\link{ppa.normalize}} function.}
  \item{pparesult}{The result of a PPA run, a set of co-modules.}
  \item{\dots}{Additional arguments, see details below.}
}
\details{
  This function can be called as
  \preformatted{
    ppa.unique(normed.data, pparesult, method = c("cor"),
               ignore.div = TRUE, cor.limit = 0.9,
	       neg.cor = TRUE, drop.zero = TRUE)
  }
  where the arguments are:
  \describe{
    \item{normed.data}{The normalized input data, a list of four
      matrices, usually the output of \code{\link{ppa.normalize}}.}
    \item{pparesult}{The results of a PPA run, a set of co-modules.}
    \item{method}{Character scalar giving the method to be used to
      determine if two co-modules are similar. Right now only
      \sQuote{\code{cor}} is implemented, this keeps both co-modules if
      their Pearson correlation is less than \code{cor.limit}, for
      their row1, row2 and column scores. See also the \code{neg.cor}
      argument.}
    \item{ignore.div}{Logical scalar, if \code{TRUE}, then the divergent
      co-modules will be removed.}
    \item{cor.limit}{Numeric scalar, giving the correlation limit for the
      \sQuote{\code{cor}} method.}
    \item{neg.cor}{Logical scalar, if \code{TRUE}, then the
      \sQuote{\code{cor}} method considers the absolute value of the
      correlation.}
    \item{drop.zero}{Logical scalar, whether to drop co-modules that have
      all zero scores.}
  }
}
\value{
  A named list, the filtered \code{pparesult}. See the return value of
  \code{\link{ppa.iterate}} for the details.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Kutalik Z, Bergmann S, Beckmann, J: A modular approach for integrative
  analysis of large-scale gene-expression and drug-response data
  \emph{Nat Biotechnol} 2008 May; 26(5) 531-9.
}
\seealso{
  See \code{\link{ppa}} for an easy way of running the PPA
}
\examples{
## Create an PPA module set
set.seed(1)
insili <- ppa.in.silico(noise=0.01)

## Random seeds
seeds <- generate.seeds(length=nrow(insili[[1]]), count=20)

## Normalize input matrix
nm <- ppa.normalize(insili[1:2])

## Do PPA
ppares <- ppa.iterate(nm, row1.seeds=seeds,
                      thr.row1=2, thr.row2=2, thr.col=1)

## Check correlation among modules
cc <- cor(ppares$rows1)
if (interactive()) { hist(cc[lower.tri(cc)],10) }

## Some of them are quite high, how many?
undiag <- function(x) { diag(x) <- 0; x }
sum(undiag(cc) > 0.99, na.rm=TRUE)

## Eliminate duplicated modules
ppares.unique <- ppa.unique(nm, ppares)

## How many modules left?
ncol(ppares.unique$rows1)

## Double check
cc2 <- cor(ppares.unique$rows1)
if (interactive()) { hist(cc2[lower.tri(cc2)],10) }

## High correlation?
sum(undiag(cc2) > 0.99, na.rm=TRUE)

}
\keyword{cluster}
