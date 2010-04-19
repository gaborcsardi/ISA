\encoding{UTF-8}
\name{LaunchEV}
\alias{LaunchEV}
\title{Launch the ExpressionView Flash applet}
\concept{biclustering, gene expression data, interactive}
\description{Launches the ExpressionView Flash applet that comes with
  the R package.}
\usage{
LaunchEV()
}
\details{
  LaunchEV opens the ExpressionView Flash applet for visualizing
  biclusters identified in gene expression data. Running the applet
  requires a Browser with Adobe Flash 10 installed. The program is
  written in ActionScript and Adobe Flex. The source code is freely
  available. You can get it by right-clicking into the applet and
  selecting \dQuote{View Source}.
}
\seealso{\code{\link{OrderEV}}, \code{\link{ExportEV}},
  \code{\link[eisa]{ISA}}, \code{\link[biclust]{biclust}}}
\author{Andreas \enc{Lüscher}{Luscher}
  \email{andreas.luescher@a3.epfl.ch}}
\examples{
## Launch the ExpressionView applet
\dontrun{LaunchEV()}
}
\keyword{launch, open, ExpressionView, bicluster, order, isa}
