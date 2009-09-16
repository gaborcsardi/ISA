open.ev <- function(modules, eset, order) {
  filename <- paste(tempfile(), sep="", ".ged")
  export.ev(modules, eset, order, filename=filename)
  swf <- system.file("ExpressionView.html", package="ExpressionView")
  url <- URLencode(paste("file://", swf, sep="", "?filename=", filename))
  browseURL(url)
}

