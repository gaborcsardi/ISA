
setGeneric("featExprs", function(object) standardGeneric("featExprs"))
setGeneric("sampExprs", function(object) standardGeneric("sampExprs"))
setGeneric("prenormalized", function(object) standardGeneric("prenormalized"))
setGeneric("prenormalized<-", function(object, value) standardGeneric("prenormalized<-"))
setGeneric("hasNA", function(object) standardGeneric("hasNA"))
setGeneric("hasNA<-", function(object, value) standardGeneric("hasNA<-"))

###########

setGeneric("drive", function(p) standardGeneric("drive"))
setGeneric("drive<-", function(p, value) standardGeneric("drive<-"))

setGeneric("seedData", function(modules) standardGeneric("seedData"))
setGeneric("runData", function(modules) standardGeneric("runData"))
setGeneric("getOrganism", function(modules) standardGeneric("getOrganism"))

setGeneric("getFeatures", function(modules, ...) standardGeneric("getFeatures"))
setGeneric("getSamples", function(modules, ...) standardGeneric("getSamples"))

setGeneric("getFeatureNames", function(modules, ...) standardGeneric("getFeatureNames"))
setGeneric("getSampleNames", function(modules, ...) standardGeneric("getSampleNames"))

setGeneric("getFeatureMatrix", function(modules, ...) standardGeneric("getFeatureMatrix"))
setGeneric("getSampleMatrix", function(modules, ...) standardGeneric("getSampleMatrix"))

setGeneric("getFullFeatureMatrix", function(modules, ...) standardGeneric("getFullFeatureMatrix"))
setGeneric("getFullSampleMatrix", function(modules, ...) standardGeneric("getFullSampleMatrix"))

setGeneric("getFeatureScores", function(modules, ...) standardGeneric("getFeatureScores"))
setGeneric("getSampleScores", function(modules, ...) standardGeneric("getSampleScores"))

setGeneric("getNoFeatures", function(modules, ...) standardGeneric("getNoFeatures"))
setGeneric("getNoSamples", function(modules, ...) standardGeneric("getNoSamples"))

setGeneric("featureThreshold", function(modules, ...) standardGeneric("featureThreshold"))
setGeneric("sampleThreshold", function(modules, ...) standardGeneric("sampleThreshold"))
