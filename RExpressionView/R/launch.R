LaunchEV <- function() {
	swf <- system.file("ExpressionView.html", package="ExpressionView")
	url <- URLencode(paste("file://", swf, sep=""))
	browseURL(url)
}
