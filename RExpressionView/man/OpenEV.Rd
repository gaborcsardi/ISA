\encoding{UTF-8}
\name{OpenEV}
\alias{OpenEV}
\title{Open an ExpressionView file}
\concept{biclustering, gene expression data, interactive}
\description{Opens the ExpressionView Flash applet that comes with the R package.}
\usage{
OpenEV()
}
\details{
OpenEV opens the ExpressionView Flash applet for visualizing biclusters identified in gene expression data. Running the applet requires a Browser with Adobe Flash 10 installed. The program is written in ActionScript and Adobe Flex. The source code is freely available. You can get it by right-clicking into the applet and selecting \dQuote{View Source}.
}
\seealso{\code{\link{OrderEV}}, \code{\link{ExportEV}}, \code{\link{ISA}}, \code{\link{biclust}}}
\author{Andreas \enc{Lüscher}{Luscher} \email{andreas.luescher@a3.epfl.ch}}
\references{\enc{Lüscher}{Luscher}, A., ...}
\examples{
## Open the ExpressionView applet
OpenEV()
}
\keyword{open, ExpressionView, bicluster, order, isa}
