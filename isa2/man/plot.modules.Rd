\name{plotModules}
\alias{images}
\alias{plotModules}
\alias{plotModules,list-method}
\concept{Bicluster}
\title{Image plots of biclusters}
\description{Make several image plots, one for each bicluster, and
  optionally one for the original data as well.}
\usage{
images(matrices, names=NULL, \dots)
\S4method{plotModules}{list} (modules, \dots)
}
\arguments{
  \item{matrices}{A list of matrices to plot. Please note that this
    argument is \emph{always} interpreted as a list, even if want to
    plot a single matrix, put it into a list.}
  \item{names}{Character vector, the labels to show above the image
    plots. If you give the \code{data} argument to \code{plotModules},
    then the first label corresponds to that.}
  \item{\dots}{Additional arguments, for \code{images} these are passed
    to \code{\link[lattice]{levelplot}}, for \code{plotModules} see the
    details below.} 
  \item{modules}{The object with the ISA modules, as returned by the
    \code{\link{isa}} function or other such functions.}
}
\details{
  \code{images} creates image plots for a series of matrices, using
  \code{\link[lattice]{levelplot}} from the \code{lattice} package.
  
  \code{plotModules} calls \code{images} from the
  to create image plots for a set of modules. It can be called as
  \preformatted{
    plotModules(modules, to.plot=seq_len(ncol(modules$rows)),
                data, binary=TRUE, names=NULL, xlab="", ylab="",
		\dots)
	      }
  where the arguments are:
  \describe{
    \item{modules}{The object with the ISA modules, as returned by the
      \code{\link{isa}} function or other such functions.}
    \item{to.plot}{Numeric vector, the modules to plot, the numbers
      correspond to the columns in \code{modules$rows} and
      \code{modules$columns}. By default all modules will be plot.}
    \item{data}{An optional data matrix to plot. Most often this is the
      original data. If given, its dimension must much the dimensions in
      the \code{modules} object. If given, then this matrix is plotted
      first, before the modules.}
    \item{binary}{Logical scalar, whether to binarize the biclusters
      before plotting or use the actual ISA scores. By default the
      biclusters are binarized.}
    \item{names}{Character vector, the labels to show above the image
      plots. If you give the \code{data} argument to \code{plotModules},
      then the first label corresponds to that.}
    \item{xlab}{Character scalar, the label to put on the horizontal
      axis.}
    \item{ylab}{Character scalar, the label to put on the vertical axis.}
    \item{\dots}{Further arguments are passed to
      \code{\link[lattice]{levelplot}}.}
  }

  Note, that if you want to export these plots to a file, then a
  bitmap-based format might be more appropriate. For larger matrices
  vector formats tend to generate huge file because of the many dots.
}
\value{
  Since these function use the \code{lattice} package, they return an
  object of class \code{trellis}. You will need to \code{print} this
  object to create the actual plots.
}
\author{ Gabor Csardi \email{Gabor.Csardi@unil.ch}}
%\references{}
\seealso{
  \code{\link{image}} and the other version: \code{image}
  from the \code{Matrix} package, for alternatives to create image
  plots.
}
\examples{
## The following should plot the input matrix and the four modules
## found by ISA
set.seed(1)   # to get same plot every time
data <- isa.in.silico(100, 100, num.fact=4)
modules <- isa(data[[1]], thr.row=2, thr.col=2)
plotModules(modules, data=data[[1]], binary=FALSE,
            names=c("Input matrix",
                    paste("Module", seq_len(ncol(modules$rows)))))
}
\keyword{cluster}
