\name{isa.iterate}
\alias{isa.iterate}
\alias{isa.iterate,list-method}
\title{The Iterative Signature Algorithm}
\description{
  Perform ISA on the (normalized) input matrix.
}
\usage{
\S4method{isa.iterate}{list}(normed.data, row.seeds, col.seeds, thr.row, thr.col = thr.row,    
     direction = c("updown", "updown"), convergence = c("cor",               
         "eps"), cor.limit = 0.99, eps = 1e-04, oscillation = FALSE, 
     maxiter = 100)
}
\arguments{
  \item{normed.data}{The normalized data. A list of two matrices,
    usually coming from \code{\link{isa.normalize}}.}
  \item{row.seeds}{The row seed vectors to start the ISA runs
    from. Every column is a seed vector. (If this argument and
    \code{col.seeds} are both present, then both of them are used.)}
  \item{col.seeds}{The column seed vectors to start the ISA runs from,
    every column is a seed vector. (If this argument and
    \code{row.seeds} are both present, then both of them are used.)}
  \item{thr.row}{Numeric scalar or vector giving the threshold parameter
    for the rows. Higher values indicate a more stringent threshold and
    the result biclusters will contain less rows on average. The
    threshold is measured by the number of standard deviations from the
    mean, over the values of the row vector. If it is a vector then it
    must contain an entry for each seed.}
  \item{thr.col}{Numeric scalar or vector giving the threshold parameter
    for the columns. The analogue of \code{thr.row}.}
  \item{direction}{Character vector of length two, one for the rows, one
    for the columns. It specifies whether we are interested in
    rows/columns that are higher (\sQuote{\code{up}}) than average,
    lower than average (\sQuote{\code{down}}), or both
    (\sQuote{\code{updown}}).}
  \item{convergence}{Character scalar, the convergence criteria for the
    ISA iteration. If it is \sQuote{\code{cor}}, then convergence is
    measured based on high Pearson correlation (see the \code{cor.limit}
    argument below) of the subsequent row and
    column vectors. If it is \sQuote{\code{eps}}, then all entries of
    the subsequent row and column vectors must be close to each other,
    see the \code{eps} argument below.}
  \item{cor.limit}{The correlation limit for convergence if the
    \sQuote{\code{cor}} method is used.}
  \item{eps}{Limit for convergence if the \sQuote{\code{eps}} method is
    used.}
  \item{oscillation}{Logical scalar, whether to look for oscillating
    seeds. Usually there are not too many oscillating seeds, so it is
    safe to leave this on \code{FALSE}.}
  \item{maxiter}{The maximum number of iterations allowed.}
}
\details{
  \code{isa.iterate} performs the ISA iteration on the specified input
  seeds.
}
\value{
  A named list with many components. Please see the manual page of
  \code{\link{isa}} for a complete description.
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

## Filter out not robust ones
isares <- isa.filter.robust(insili[[1]], nm, isares)

## Print the sizes of the modules
cbind( colSums(isares$rows!=0), colSums(isares$columns!=0) )

## Plot the original data and the modules found
if (interactive()) {
  layout(rbind(1:2))
  image(insili[[1]], main="In silico data")
  image(outer(isares$rows[,1],isares$columns[,1])+
        outer(isares$rows[,2],isares$columns[,2])+
        outer(isares$rows[,3],isares$columns[,3]), main="ISA modules")
}
}
\keyword{cluster}
