\name{ppa.iterate}
\alias{ppa.iterate}
\alias{ppa.iterate,list-method}
\title{The Ping-Pong Algorithm}
\description{
  Perform PPA on two (normalized) input matrices.
}
\usage{
\S4method{ppa.iterate}{list}(normed.data, \dots)
}
\arguments{
  \item{normed.data}{The normalized input matrices. A list of four
    matrices, usually coming from the \code{\link{ppa.normalize}}
    function.}
  \item{\dots}{Additional arguments, see details below.}
}
\details{
  \code{ppa.iterate} performs the PPA iteration on the specified input
  matrices using given input seeds. It can be called
  as \preformatted{
    ppa.iterate(normed.data, row1.seeds, col1.seeds,
                row2.seeds, col2.seeds,
                thr.row1, thr.col=thr.row1, thr.row2=thr.row1,
		direction="updown",
		convegence=c("cor"), cor.limit=0.9,
		oscillation=FALSE, maxiter=100)
	      }
  where the arguments are:
  \describe{
    \item{normed.data}{The normalized data, a list of four matrices with
      the appropriate size. They are usually coming from the output of
      the \code{\link{ppa.normalize}} function.}
    \item{row1.seeds}{The row seed vectors for the first matrix. At
      least one of the four possible seed vectors must be present and
      they will be concatenated, after doing the suitable
      half-iterations.}
    \item{col1.seeds}{The column seed vectors for the first matrix. At
      least one of the four possible seed vectors must be present and
      they will be concatenated, after doing the suitable
      half-iterations.}
    \item{row2.seeds}{The row seed vectors for the second matrix. At
      least one of the four possible seed vectors must be present and
      they will be concatenated, after doing the suitable
      half-iterations.}
    \item{col2.seeds}{The column seed vectors for the second matrix. At
      least one of the four possible seed vectors must be present and
      they will be concatenated, after doing the suitable
      half-iterations.}
    \item{thr.row1}{Numeric scalar or vector giving the threshold
      parameter for the rows of the first matrix. Higher values indicate
      a more stringent threshold and the result comodules will contain
      less rows for the first matrix on average. The threshold is
      measured by the number of standard deviations from the 
      mean, over the values of the first row vector. If it is a vector
      then it must contain an entry for each seed.}
    \item{thr.col}{Numeric scalar or vector giving the threshold
      parameter for the columns of both matrices. The analogue of
      \code{thr.row1}.}
    \item{thr.row2}{Numeric scalar or vector, the threshold parameter(s)
      for the rows of the second matrix. See \code{thr.row1} for
      details.}
    \item{direction}{Character vector of length four, one for each matrix
      multiplication performed during a PPA iteration. 
      It specifies whether we are interested in
      rows/columns that are higher (\sQuote{\code{up}}) than average,
      lower than average (\sQuote{\code{down}}), or both
      (\sQuote{\code{updown}}). The first and the second entry both
      corresponds to the common column dimension of the two matrices, so
      they should be equal, otherwise a warning is given.}
    \item{convergence}{Character scalar, the convergence criteria for the
      PPA iteration. If it is \sQuote{\code{cor}}, then convergence is
      measured based on high Pearson correlation (see the \code{cor.limit}
      argument below) of the subsequent row and column
      vectors. Currently this is the only option implemented.}
    \item{cor.limit}{The correlation limit for convergence if the
      \sQuote{\code{cor}} method is used.}
    \item{oscillation}{Logical scalar, whether to look for oscillating
      seeds. Usually there are not too many oscillating seeds, so it is
      safe to leave this on \code{FALSE}.}
    \item{maxiter}{The maximum number of iterations allowed.}
  }
}
\value{A named list with many components. Please see the manual page of
  \code{link{isa}} for a complete description.
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
## A basic PPA workflow with a single threshold combination
## In-silico data
set.seed(1)
insili <- ppa.in.silico()

## Random seeds
seeds <- generate.seeds(length=nrow(insili[[1]]), count=100)

## Normalize input matrices
nm <- ppa.normalize(insili[1:2])

## Do PPA
ppares <- ppa.iterate(nm, row1.seeds=seeds, thr.row1=1, direction="up")

## Eliminate duplicates
ppares <- ppa.unique(nm, ppares)

## Fitler out not robust ones
ppares <- ppa.filter.robust(insili[1:2], nm, ppares)

## Print the sizes of the co-modules
cbind(colSums(ppares$rows1 != 0), colSums(ppares$rows1 != 0),
      colSums(ppares$columns != 0))

## Plot the original data and the first three modules found
if (interactive()) {
  layout(rbind(1:2,3:4))
  image(insili[[1]], main="In silico data 1")
  image(outer(ppares$rows1[,1],ppares$columns[,1])+
        outer(ppares$rows1[,2],ppares$columns[,2])+
        outer(ppares$rows1[,3],ppares$columns[,3]), main="PPA co-modules 2")
  image(insili[[2]], main="In silico data 2")
  image(outer(ppares$rows2[,1],ppares$columns[,1])+
        outer(ppares$rows2[,2],ppares$columns[,2])+
        outer(ppares$rows2[,3],ppares$columns[,3]), main="PPA co-modules 2")
}

}
\keyword{cluster}
