OpenEV <- function() {
	swf <- system.file("ExpressionView.html", package="ExpressionView")
	url <- URLencode(paste("file://", swf, sep=""))
	browseURL(url)
}

#OpenEV <- function(modules=NULL, eset=NULL, order=NULL) {
#	swf <- system.file("ExpressionView.html", package="ExpressionView")
#	if ( is.null(modules) ) {
#		url <- URLencode(paste("file://", swf, sep=""))
#	} else {
#		filename <- paste(tempfile(), sep="", ".xml")
#		ExportEV(modules, eset, order, filename=filename)
#		url <- URLencode(paste("file://", swf, sep="", "?filename=", filename))
#	}
#	browseURL(url)
#}

