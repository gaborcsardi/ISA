
setGeneric("isa", function(data, ...) standardGeneric("isa"))
setGeneric("isa.normalize",
           function(data, ...) standardGeneric("isa.normalize"))
setGeneric("isa.iterate",
           function(normed.data, ...) standardGeneric("isa.iterate"))
setGeneric("isa.unique",
           function(normed.data, ...) standardGeneric("isa.unique"))

setGeneric("robustness",
           function(normed.data, ...) standardGeneric("robustness"))
setGeneric("isa.filter.robust",
           function(data, ...) standardGeneric("isa.filter.robust"))

setGeneric("isa.sweep",
           function(data, ...) standardGeneric("isa.sweep"))
setGeneric("sweep.graph",
           function(sweep.result, ...) standardGeneric("sweep.graph"))

