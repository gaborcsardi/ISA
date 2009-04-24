

setGeneric("drive", function(r) standardGeneric("drive"))
setGeneric("drive<-", function(r, value) standardGeneric("drive<-"))

setGeneric("seedData", function(object) standardGeneric("seedData"))
setGeneric("runData", function(object) standardGeneric("runData"))

setGeneric("getGenes", function(object, ...) standardGeneric("getGenes"))
setGeneric("getConditions", function(object, ...) standardGeneric("getConditions"))

setGeneric("getGeneScores", function(object, ...) standardGeneric("getGeneScores"))
setGeneric("getConditionScores", function(object, ...) standardGeneric("getConditionScores"))

setGeneric("getAllGeneScores", function(object, ...) standardGeneric("getAllGeneScores"))
setGeneric("getAllConditionScores", function(object, ...) standardGeneric("getAllConditionScores"))

setGeneric("getNoGenes", function(object, ...) standardGeneric("getNoGenes"))
setGeneric("getNoConditions", function(object, ...) standardGeneric("getNoConditions"))

setGeneric("geneThreshold", function(object, ...) standardGeneric("geneThreshold"))
setGeneric("conditionThreshold", function(object, ...) standardGeneric("conditionThreshold"))
