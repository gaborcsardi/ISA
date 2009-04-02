\name{isa}
\alias{isa}
\concept{Iterative Signature Algorithm}
\title{Iterative Signature Algorithm}
\description{Run ISA with the default parameters}
\synopsis{
isa(data, ...)
}
\usage{
isa(data, thr.row=seq(1,3,by=0.5), thr.col=seq(1,3,by=0.5), no.seeds=100)
}
\arguments{
  \item{data}{The input. It must be a numeric matrix. It may contain
    \code{NA} and/or \code{NaN} values, but then the algorithm might be a
    bit slower, as R matrix multiplication might be slower for these
    matrices, depending on your platform.}
  \item{thr.row}{Numeric vector.
    The row threshold parameters for which the ISA will be
    run. We use all possible combinations of \code{thr.row} and
    \code{thr.col}.}
  \item{thr.col}{Numeric vector.
    The column threshold parameters for which the ISA will be run. We
    use all possible combinations of \code{thr.row} and \code{thr.col}.
  }
  \item{no.seeds}{Integer scalar, the number of seeds to use.}
}
\details{
  Please read the \link{isa-package} manual page for an introduction on
  ISA.
  
  The \code{isa} function provides an easy to use interface to the
  ISA. It runs all steps of a typical ISA work flow with their default
  parameters. 

  This involves:
  \enumerate{
    \item Normalizing the data by calling \code{\link{isa.normalize}}.
    \item Generating random input seeds via
    \code{\link{generate.seeds}}.
    \item Running ISA with all combinations of given row and column
    thresholds, (by default 1, 1.5, 2, 2.5, 3); by calling
    \code{\link{isa.iterate}}. 
    \item Merging similar modules, separately for each threshold
      combination, by calling \code{\link{isa.unique}}.
    \item Filtering the modules separately for each threshold combination,
      by calling \code{\link{isa.filter.robust}}.
    \item Putting all modules from the runs with different thresholds into
      a single object.
    \item Merging similar modules, across all threshold combinations, if
      two modules are similar, then the larger one, the one with the
      milder thresholds is kept.
  }
  Please see the manual pages of these functions for the details or if
  you want to change their default parameters.

}
\value{
  A named list is returned with the following elements:
  \item{rows}{The row components in the biclusters, a numeric
    matrix. Every column in it corresponds to the a bicluster, if an
    element (the score of the row) is non-zero, that means that the row
    is included in the bicluster, otherwise it is not. Scores are
    between -1 and 1. If the scores of two rows have the same (nonzero)
    sign, that means that the two corresponding rows \dQuote{behave} the
    same way. If they have opposite sign, that means that they behave
    the opposite way.

    If the corresponding seed has not converged during the allowed
    number of iterations, then that column of \code{rows} contains
    \code{NA} values.}
  \item{columns}{The column components of the biclusters, in the same
    format as the rows.
    
    If the corresponding seed has not converged during the allowed
    number of iterations, then that column of \code{columns} contains
    \code{NA} values.}
  \item{seeddata}{A data frame containing information about the
    biclusters. There is one row for each bicluster. The data frame has
    the following columns:
    \describe{
      \item{\code{iterations}}{The number of iterations needed to
	converge to the bicluster.}
      \item{\code{oscillation}}{The oscillation period for oscillating
	biclusters. It is zero for non-oscillating ones.}
      \item{\code{thr.row}}{The row threshold that was used for find the
	bicluster.}
      \item{\code{thr.col}}{The column threshold that was used for finding the
	bicluster.}
      \item{\code{freq}}{The number of times the bicluster was found.}
      \item{\code{rob}}{The robustness score of the bicluster, see
	\code{\link{robustness}} for details.}
      \item{\code{rob.limit}}{The robustness limit that was used to filter the
	module. See \code{\link{isa.filter.robust}} for details.}
    }
  }
  \item{rundata}{A named list with information about the ISA runs. It
    has the following entries:
    \describe{
      \item{\code{direction}}{Character vector of length two.
	Specifies which side(s) of the score distribution
	were kept in each ISA step. See the \code{direction} argument
	of \code{\link{isa.iterate}} for details.}
      \item{\code{convergence}}{Character scalar. The convergence criteria for
	the iteration. See the \code{convergence} argument of
	\code{\link{isa.iterate}} for details.}
      \item{\code{eps}}{Numeric scalar. The threshold for convergence, if the
	\sQuote{eps} convergence criteria was used.}
      \item{\code{cor.limit}}{Numeric scalar. The threshold for convergence, if
	the \sQuote{cor} convergence criteria was used.}
      \item{\code{maxiter}}{Numeric scalar. The maximum number of iterations
	that were allowed for an input seed.}
      \item{\code{N}}{Numeric scalar. The total number of seeds that were used
	for all the thresholds.}
      \item{\code{prenormalize}}{Logical scalar. Whether the data was
	pre-normalized.} 
      \item{\code{hasNA}}{Logical scalar. Whether the (normalized) data had
	\code{NA} or \code{NaN} values.}
      \item{\code{unique}}{Logical scalar. Whether the similar biclusters were
	merged by calling \code{\link{isa.unique}}.}
      \item{\code{oscillation}}{Logical scalar. Whether the algorithm looked
	for oscillating modules as well.} 
      \item{\code{rob.perms}}{Numeric scalar, the number of permutations that
	were used to calculate the baseline robustness for
	filtering. See the \code{perms} argument of the
	\code{\link{isa.filter.robust}} function for details.}
    }
  }
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
  Signature Algorithm. See the functions mentioned above if you want to
  change the default ISA parameters.
}
\examples{
## We generate some noisy in-silico data with modules and try to find
## them with the ISA. This might take one or two minutes.
data <- isa.in.silico(noise=0.1)
isa.result <- isa(data[[1]])

## Find the best bicluster for each block in the input
best <- apply(cor(isa.result$rows, data[[2]]), 2, which.max)

## Check correlation
sapply(seq_along(best),
       function(x) cor(isa.result$rows[,best[x]], data[[2]][,x]))

## The same for the columns
sapply(seq_along(best),
       function(x) cor(isa.result$columns[,best[x]], data[[3]][,x]))

## Plot the data and the modules found
if (interactive()) {
  layout(rbind(1:2,3:4))
  image(data[[1]], main="In-silico data")
  sapply(best, function(b) image(outer(isa.result$rows[,b],
                                       isa.result$columns[,b]),
                                 main=paste("Module", b)))  
}
}
\keyword{cluster}
