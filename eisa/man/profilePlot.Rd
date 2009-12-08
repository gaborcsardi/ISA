\name{profilePlot}
\alias{profilePlot}
\title{Profile plots for ISA biclusters}
\description{Line plots to compare biclusters to the background,
  i.e. the rest of the expression matrix.}
\usage{
profilePlot (modules, module, eset, plot = c("samples", "features", 
     "both"), norm = "default", background = TRUE, 
    col = gray(0.7), col.mod = 1, type = "l", type.mod = type,
    mean = TRUE, meancol = "green", meancol.mod = "red",
    xlabs = c("Features", "Samples"), ylab = "Expression", 
    \dots) 
}
\arguments{
  \item{modules}{An \code{ISAModules} object.}
  \item{module}{Numeric scalar, the module to plot.}
  \item{eset}{An \code{ExpressionSet} or \code{ISAExpressionSet}
    object. If an \code{ExpressionSet} object is
    supplied (and the \code{norm} argument is not set to \sQuote{raw}),
    then it is normalised by calling \code{\link{ISANormalize}} on it.
    A subset of \code{eset} is selected that corresponds to
    the features included in \code{modules}.
  }
  \item{plot}{Character constant, specifies what to
    plot. \sQuote{sample} plots sample scores, \sQuote{features} plots
    feature scores. If \sQuote{both} is given, then the plot is divided
    into two subplots and both scores are plotted.}
  \item{norm}{Character constant, specifies how to normalize the
    expression matrix for plotting. It can be of length one or two, the
    latter for the case when plots are made both for features and
    samples. Possible values: \sQuote{\code{raw}} uses the raw
    expression values; \sQuote{\code{feature}} uses
    \code{\link{featExprs}} to extract the expression values from the
    expression set object; \sQuote{\code{sample}} uses
    \code{\link{sampExprs}}; \sQuote{\code{default}} means
    \sQuote{\code{feature}} for sample plots and \sQuote{\code{sample}} 
    for feature plots. }
  \item{background}{Logical scalar, whether to plot the features/samples
    that are not in the module.}
  \item{col}{Color of lines corresponding to the background
    features/samples.}
  \item{col.mod}{Color of the lines corresponding to the
    features/samples included in the module.}
  \item{type}{Type of the plot, for the background features/samples. It
    is passed to \code{\link[graphics]{plot}}.} 
  \item{type.mod}{Type of the plot, for the features/samples included in
    the module. It is passed to \code{\link[graphics]{plot}}.}
  \item{mean}{Logical scalar, whether to plot the mean expression for
    each feature/sample, separately for the samples/features that are in
    the module and the ones that are not.}
  \item{meancol}{Color of the line for the mean expression values,
    background.}
  \item{meancol.mod}{Color of the line for the mean expression values,
    module.}
  \item{xlabs}{Character vector of length one or two. The labels of
    the horizontal axes of the plot, the second value is used if both
    the feature and the sample plots are drawn.}
  \item{ylab}{Character vector of length one. The label of the vertical
    axes.}
  \item{\dots}{Additional graphical arguments. They are passed to the
    \code{\link[graphics]{lines}} function that creates the lines of the
    plot.}
}
\details{
  \code{plot="both"} uses the \code{mfrow} graphical parameter to create
  the two subplots. This does not work properly if you already have
  subplots.
}
\value{
  None. (Well, \code{NULL}, invisibly.)
}
\author{
  Gabor Csardi \email{Gabor.Csardi@uni.ch}
}
\references{
  Bergmann S, Ihmels J, Barkai N: Iterative signature algorithm for the
  analysis of large-scale gene expression data \emph{Phys Rev E Stat
    Nonlin Soft Matter Phys.} 2003 Mar;67(3 Pt 1):031902. Epub 2003 Mar 11.
}
\seealso{The similar \code{\link[biclust]{parallelCoordinates}} function
  in the \code{biclust} package.}
\examples{
data(ALLModulesSmall)
library(ALL)
data(ALL)
if (interactive()) {
  profilePlot(ALLModulesSmall, 2, ALL, plot="samples")
}
}
\keyword{cluster}
