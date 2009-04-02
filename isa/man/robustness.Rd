\name{robustness}
\alias{robustness}
\alias{isa.filter.robust}
\alias{robustness,list-method}
\alias{isa.filter.robust,matrix-method}
\title{Robustness of ISA biclusters}
\description{
  Robustness of ISA biclusters. The more robust biclusters are more
  significant in the sense that it is less likely to see them in random
  data.
}
\usage{
\S4method{robustness}{list}(normed.data, row.scores, col.scores)
\S4method{isa.filter.robust}{matrix}(data, normed.data, isares, perms = 1, row.seeds, col.seeds)
}
\arguments{
  \item{normed.data}{The normalized input data, usually calculated with
    \code{\link{isa.normalize}}.}
  \item{row.scores}{The scores of the row components of the
    biclusters. Usually the \code{rows} member of the result list, as
    returned by \code{\link{isa}}, or \code{\link{isa.iterate}} or some
    other ISA function.}
  \item{col.scores}{The scores of the columns of the biclusters, usually
    the \code{columns} member of the result list from
    \code{\link{isa}}.}
  \item{data}{The original, not normalized input data.}
  \item{isares}{The result of ISA, coming from \code{\link{isa}} or
    \code{\link{isa.iterate}} or any other function that return the same
    format.}
  \item{perms}{The number of permutations to perform on the input data.}
  \item{row.seeds}{Optionally the row seeds for the ISA run on the
    scrambled data. If this and \code{col.seeds} are both omitted the
    same number of random seeds are used as for \code{isaresult}.}
  \item{col.seeds}{Optionally the column seed to use for the ISA on the
    scrambled input matrix. If both this and \code{row.seeds} are
    omitted, then the same number of random (row) seeds will be used as
    for \code{isares}.}
}
\details{
  Even if you generate a matrix with uniform random noise in it, if you
  calculate ISA on it, you will get some biclusters, except maybe if you
  use very strict threshold parameters. These biclusters contain rows
  and columns that are correlated just by chance.

  To circumvent this, you can use the so-called robustness measure of
  the biclusters. The robustness of a bicluster is the function of its
  rows, columns and the input data, and it is a real number, usually
  positive. It is roughly equivalent to the principal singular value of
  the submatrix (of the reordered input matrix) defined by the
  bicluster.

  \code{robustness} calculates the robustness score of a set of
  biclusters, usually coming from one or more ISA iterations.

  \code{isa.filter.robust} provides filtering based on the robustness
  measure. It reshuffles the input matrix and calculates ISA on it, with
  the parameters that were used to find the biclusters under
  evaluation. It then calculates the robustness for the modules that
  were found in the scrambled matrix (if there is any) and removes any
  modules from the data set that have a lower robustness score than at
  least one module in the scrambled data.

  You can think of \code{isa.filter.robust} as a permutation test, but
  the input data is shuffled only once, because of the relatively high
  computational demands of the ISA.
}
\value{
  \code{robustness} returns a numeric vector, the robustness score of
  each bicluster.

  \code{isa.filter.robust} returns a named list, the filtered
  \code{isares}, see the return value of \code{\link{isa.iterate}} for
  the structure of the list.
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
## A basic ISA work flow for a single threshold combination
## In-silico data
set.seed(1)
insili <- isa.in.silico()

## Random seeds
seeds <- generate.seeds(length=nrow(insili[[1]]), count=100)

## Normalize input matrix
nm <- isa.normalize(insili[[1]])

## Do ISA
isares <- isa.iterate(nm, row.seeds=seeds, thr.row=2, thr.col=1)

## Eliminate duplicates
isares <- isa.unique(nm, isares)

## Calculate robustness
rob <- robustness(nm, isares$rows, isares$columns)
rob

## There are three robust ones and a lot of less robust ones
## Plot the three robust ones and three others
if (interactive()) {
  toplot1 <- rev(order(rob))[1:3]
  toplot2 <- sample(seq_along(rob)[-toplot1], 3)
  layout( rbind(1:3,4:6) )
  for (i in c(toplot1, toplot2)) {
    image(outer(isares$rows[,i], isares$column[,i]),
          main=round(rob[i],2))
  }
}

## Filter out not robust ones
isares2 <- isa.filter.robust(insili[[1]], nm, isares)

## Probably there are only three of them left
ncol(isares2$rows)
}
\keyword{cluster}
