
setClass("ISAModules",
         representation=representation(
           genes="matrix",
           conditions="matrix",
           rundata="list",
           seeddata="data.frame"),
         prototype=prototype(
           genes=matrix(NA,0,0),
           conditions=matrix(NA,0,0),
           rundata=list(),
           seeddata=data.frame()
           )
         )
