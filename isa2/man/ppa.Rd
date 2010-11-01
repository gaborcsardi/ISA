\name{ppa}
\alias{ppa}
\alias{ppa,list-method}
\concept{Ping-Pong Algorithm}
\title{The Ping-Pong Algorithm}
\description{Run the PPA with the default parameters}
\usage{
\S4method{ppa}{list}(data, \dots)
}
\arguments{
  \item{data}{The input, a list of two numeric matrices, with the same
    number of columns. They may contain \code{NA} and/or \code{NaN}
    values, but then the algorithm might get slower, as R matrix
    multiplication is slower sometimes slower for these matrices,
    depending on your platform.}
  \item{\dots}{Additional arguments, see details below.}
}
\details{
  Please read the \link{isa2-package} manual page for and introductino
  on ISA and PPA.

  This function can be called as
  \preformatted{
    ppa(data, thr.row1 = seq(1, 3, by = 0.5),
        thr.row2 = seq(1, 3, by = 0.5),
        thr.col = seq(1, 3, by = 0.5),
        no.seeds = 100, direction = "updown")
      }
  where the arguments are:
  \describe{
    \item{data}{The input, a list of two numeric matrices, with the same
      number of columns. They may contain \code{NA} and/or \code{NaN}
      values, but then the algorithm might get slower, as R matrix
      multiplication is slower sometimes slower for these matrices,
      depending on your platform.}
    \item{thr.row1}{Numeric scalar or vector giving the threshold
      parameter for the rows of the first matrix. Higher values indicate
      a more stringent threshold and the result comodules will contain
      less rows for the first matrix on average. The threshold is
      measured by the number of standard deviations from the 
      mean, over the values of the first row vector. If it is a vector
      then it must contain an entry for each seed.}
    \item{thr.row2}{Numeric scalar or vector, the threshold parameter(s)
      for the rows of the second matrix. See \code{thr.row1} for
      details.}
    \item{thr.col}{Numeric scalar or vector giving the threshold
      parameter for the columns of both matrices. The analogue of
      \code{thr.row1}.}
    \item{no.seeds}{Integer scalar, the number of random seeds to use.}
    \item{direction}{Character vector of length four, one for each matrix
      multiplication performed during a PPA iteration. 
      It specifies whether we are interested in
      rows/columns that are higher (\sQuote{\code{up}}) than average,
      lower than average (\sQuote{\code{down}}), or both
      (\sQuote{\code{updown}}). The first and the second entry both
      corresponds to the common column dimension of the two matrices, so
      they should be equal, otherwise a warning is given.}
  }

  The \code{ppa} function provides and easy interface to the PPA. It
  runs all sptes of a typical PPA work flow, with their default
  paramers.

  This involves:
  \enumerate{
    \item Normalizing the input matrices by calling
    \code{\link{ppa.normalize}}.
    \item Generating random input seeds via
    \code{\link{generate.seeds}}.
    \item Running the PPA with all combinations of the given row1, row2
    and column thresholds (by default 1, 1.5, 2, 2.5, 3); by calling
    \code{\link{ppa.iterate}}.
    \item Merging similar co-modules, separately for each threshold
    combination, by calling \code{\link{ppa.unique}}.
    \item Filtering the co-modules separately for each threshold
    combination, by calling \code{\link{ppa.filter.robust}}.
    \item Putting all co-modules from the run with different thresholds,
    into a single object.
    \item Merging similar co-modules, again, but now across all
    threshold combinations. If two co-modules are similar, then the
    larger one, the one with milder thresholds is kept.
  }
  Please see the manual pages of these functions for the details.
}
\value{
  A named list is returned with the following elements:
  \item{rows1}{The first components of the co-modules, corresponding to
    the rows of the first input matrix. Every column corresponds to a
    co-module, if an element (the score of the row) is non-zero, that
    means that that component is included in the co-module, otherwise it
    is not. Scores are between -1 and 1. If two scores have
    the same non-zero sign, then the corresponding first matrix rows are
    collelated. If they have an opposite sign, then they are
    anti-correlated.

    If an input seed did not converge within the allowed number of
    iterations, then that column of \code{rows1} contains \code{NA}
    values. The \code{ppa} function does not produce such columns,
    because it always drops the non-convergent seeds via a call to
    \code{\link{ppa.unique}}. The result of the
    \code{\link{ppa.iterate}} function might contain such columns,
    though.}  
  \item{rows2}{This is the same as \code{rows1}, but for the second
    input matrix.}
  \item{columns}{The same as \code{rows1} and \code{rows2}, but for the
    columns of both input matrices.}
  \item{seeddata}{A data frame containing information about the
    co-modules. There is one row for each co-module. The data frame has
    the following columns:
    \describe{
      \item{\code{iterations}}{The number of iterations needed for
	convergence.}
      \item{\code{oscillation}}{The oscillation cycle of this is
	oscillating co-module. Zero otherwise.}
      \item{\code{thr.row1}}{The threshold used for the rows of the
	first matrix.}
      \item{\code{thr.row2}}{The threshold used for the rows of the
	second matrix.}
      \item{\code{thr.col}}{The threshold used for the common column
	dimension.}
      \item{\code{freq}}{Numeric scalar, the number of times the same
	(or a very similar) co-module was found.}
      \item{\code{rob}}{The robustness score of the module.}
      \item{\code{rob.limit}}{The robustness limit that was used to
	filter the module. See \code{\link{ppa.filter.robust}} for
	details.}
    }
  }
  \item{rundata}{
    A named list with information about the PPA run. It has the
    following entries:
    \describe{
      \item{\code{direction}}{Character vector of length four, the
	\code{direction} argument of the \code{\link{ppa.iterate}}
	call.}
      \item{\code{convergence}}{Character scalar, the convergence
	criteria that was used, see the \code{\link{ppa.iterate}}
	function for details.}
      \item{\code{cor.limit}}{Numeric scalar, the correlation
	threshold, that was used if the convergence criteria was
	\sQuote{\code{cor}}.}
      \item{\code{maxiter}}{The maximum number of PPA iterations.}
      \item{\code{N}}{The total number of input seeds that were used to
	find the co-modules.}
      \item{\code{prenormalize}}{Logical scalar, whether the input
	matrices were pre-normalized, see \code{\link{ppa.normalize}}
	for details.}
      \item{\code{hasNA}}{Logical vector of length two. Whether the two
	input matrices contained any \code{NA} or \code{NaN} values.}
      \item{\code{unique}}{Logical scalar, whether the co-modules are
	unique, i.e. whether \code{\link{ppa.unique}} was called.}
      \item{\code{oscillation}}{Logical scalar, whether the
	\code{\link{ppa.iterate}} run looked for oscillating modules.}
      \item{\code{rob.perms}}{The number of data permutations that was
	performed during the robustness filtering, see
	\code{\link{ppa.filter.robust}} for details.}
    }
  }
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch} }
\references{
  Kutalik Z, Bergmann S, Beckmann, J: A modular approach for integrative
  analysis of large-scale gene-expression and drug-response data
  \emph{Nat Biotechnol} 2008 May; 26(5) 531-9.
}
\seealso{ \link{isa2-package} for a short introduction to the ISA and
  the Ping-Pong algorithms. See the functions mentioned above if you
  want to change the default ISA parameters.
}
\examples{
data <- ppa.in.silico(noise=0.1)
ppa.result <- ppa(data[1:2], direction="up")

## Find the best bicluster for each block in the input
## (based on the rows of the first input matrix)
best <- apply(cor(ppa.result$rows1, data[[3]]), 2, which.max)

## Check correlation
sapply(seq_along(best),
       function(x) cor(ppa.result$rows1[,best[x]], data[[3]][,x]))

## The same for the rows of the second matrix
sapply(seq_along(best),
       function(x) cor(ppa.result$rows2[,best[x]], data[[4]][,x]))

## The same for the columns
sapply(seq_along(best),
       function(x) cor(ppa.result$columns[,best[x]], data[[5]][,x]))

## Plot the data and the modules found
if (interactive()) {
  layout(rbind(1:2,c(3,6),c(4,7), c(5,8)))
  image(data[[1]], main="In-silico data, first matrix")
  image(data[[2]], main="In-silico data, second matrix")
  sapply(best[1:3], function(b) image(outer(ppa.result$rows1[,b],
                                       ppa.result$columns[,b]),
                                 main=paste("Module", b)))  
  sapply(best[1:3], function(b) image(outer(ppa.result$rows2[,b],
                                       ppa.result$columns[,b]),
                                 main=paste("Module", b)))  
}

}
\keyword{cluster}
