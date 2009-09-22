
setMethod("featExprs", signature(object="ISAExpressionSet"),
          function(object) assayDataElement(object, "er.exprs"))

setMethod("sampExprs", signature(object="ISAExpressionSet"),
          function(object) assayDataElement(object, "ec.exprs"))

setMethod("prenormalized", signature(object="ISAExpressionSet"),
          function(object) object@prenormalized)

setReplaceMethod("prenormalized", signature(object="ISAExpressionSet"),
                 function(object, value) {
                   object@prenormalized <- as.logical(value)
                   object
                 })
                
setMethod("hasNA", signature(object="ISAExpressionSet"),
          function(object) object@hasNA)

setReplaceMethod("hasNA", signature(object="ISAExpressionSet"),
                 function(object, value) {
                   object@hasNA <- as.logical(value)
                   object
                 })
