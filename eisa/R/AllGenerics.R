
setGeneric("feat.exprs", function(object) standardGeneric("feat.exprs"))
setGeneric("samp.exprs", function(object) standardGeneric("samp.exprs"))
setGeneric("prenormalized", function(object) standardGeneric("prenormalized"))
setGeneric("prenormalized<-", function(object, value) standardGeneric("prenormalized<-"))
setGeneric("hasNA", function(object) standardGeneric("hasNA"))
setGeneric("hasNA<-", function(object, value) standardGeneric("hasNA<-"))

###########

setGeneric("drive", function(r) standardGeneric("drive"))
setGeneric("drive<-", function(r, value) standardGeneric("drive<-"))

setGeneric("seedData", function(object) standardGeneric("seedData"))
setGeneric("runData", function(object) standardGeneric("runData"))
setGeneric("getOrganism", function(object) standardGeneric("getOrganism"))

setGeneric("getFeatures", function(object, ...) standardGeneric("getFeatures"))
setGeneric("getSamples", function(object, ...) standardGeneric("getSamples"))

setGeneric("getFeatureNames", function(object, ...) standardGeneric("getFeatureNames"))
setGeneric("getSampleNames", function(object, ...) standardGeneric("getSampleNames"))

setGeneric("getFeatureMatrix", function(object, ...) standardGeneric("getFeatureMatrix"))
setGeneric("getSampleMatrix", function(object, ...) standardGeneric("getSampleMatrix"))

setGeneric("getFeatureScores", function(object, ...) standardGeneric("getFeatureScores"))
setGeneric("getSampleScores", function(object, ...) standardGeneric("getSampleScores"))

setGeneric("getNoFeatures", function(object, ...) standardGeneric("getNoFeatures"))
setGeneric("getNoSamples", function(object, ...) standardGeneric("getNoSamples"))

setGeneric("featureThreshold", function(object, ...) standardGeneric("featureThreshold"))
setGeneric("sampleThreshold", function(object, ...) standardGeneric("sampleThreshold"))
