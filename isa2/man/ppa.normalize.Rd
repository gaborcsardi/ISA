\name{ppa.normalize}
\alias{ppa.normalize}
\alias{ppa.normalize,list-method}
\title{Normalize input data for use with the PPA}
\description{
  Normalize the two input matrices and store them in a form that can be
  used effectively to perform the Ping-Pong Algorithm}
\usage{
\S4method{ppa.normalize}{list}(data, \dots)
}
\arguments{
  \item{data}{A list of two numeric matrices, with matching number of
    columns. They might contain \code{Na} and/or \code{NaN} values.}
  \item{\dots}{Additional arguments, see details below.}
}
\details{
  This function can be called as
  \preformatted{
    isa.normalize(data, prenormalize = FALSE)
  }
  where the arguments are:
  \describe{
    \item{data}{A list of two numeric matrices, with matching number of
      columns. They might contain \code{Na} and/or \code{NaN} values.}
    \item{prenormalize}{Logical scalar, see details below.}
  }

  It was observed that the PPA works best if the input matrices are
  scaled to have mean zero and standard deviation one.

  A PPA step consist of four matrix-multiplications and this requires
  four different matrices, each of the two input matrices scaled
  row-wise and column-wise.

  If the \code{prenormalized} argument is set to \code{TRUE}, then
  row-wise scaling is calculated on the column-wise scaled matrices and
  not on the raw input.
}
\value{
  A list of four matrices, the first two corresponds to the first input
  matrix, the second two to the second matrix.
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
## In-silico data
set.seed(1)
insili <- ppa.in.silico()
nm <- ppa.normalize(insili[1:2])

## Check it
max(abs(rowSums(nm[[1]])))
max(abs(rowSums(nm[[2]])))
max(abs(rowSums(nm[[3]])))
max(abs(rowSums(nm[[4]])))

max(abs(rowSums(nm[[1]])))
max(abs(rowSums(nm[[2]])))
max(abs(rowSums(nm[[3]])))
max(abs(rowSums(nm[[4]])))

## Plot them
if (interactive()) {
  layout(rbind(1:3,4:6))
  image(insili[[1]], main="Original data 1")
  image(t(nm[[1]]), main="Row normalized 1")
  image(nm[[2]], main="Column normalized 1")

  image(insili[[2]], main="Original data 2")
  image(t(nm[[3]]), main="Row normalized 2")
  image(nm[[4]], main="Column normalized 2")
}

}
\keyword{cluster}
