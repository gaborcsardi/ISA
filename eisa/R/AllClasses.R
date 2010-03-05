
setClass("ISAExpressionSet",
         representation(prenormalized="logical",
                        hasNA="logical"),
         contains = "ExpressionSet")

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

setClass("ListHyperGParams",
         representation=representation(drive="logical"),
         contains=c("HyperGParams", "VIRTUAL"),
         prototype=prototype(drive=FALSE))

setClass("KEGGListHyperGParams",
         contains="ListHyperGParams",
         prototype=prototype(categoryName=c("KEGG", "List")))

setClass("GeneralListHyperGParams",
         representation=representation(categories="list"),
         contains="ListHyperGParams",
         prototype=prototype(categoryName=c("General", "List")))

setClass("miRNAListHyperGParams",
         contains="ListHyperGParams",
         prototype=prototype(categoryName=c("miRNA", "List")))

setClass("CHRListHyperGParams",
         contains="ListHyperGParams",
         prototype=prototype(categoryName=c("CHR", "List")))

setClass("GOListHyperGParams",
         representation=representation(ontology="character",
           conditional="logical"),
         contains=c("ListHyperGParams"),
         prototype=prototype(conditional=FALSE,
           categoryName=c("GO", "List")))

setClass("ListHyperGResult",
         contains=c("HyperGResultBase", "VIRTUAL"),
         representation=representation(
           reslist="list",
           drive="logical",
           universeGeneIds="character",
           catToGeneId="list"),
         prototype=prototype(
           testname=NA,
           reslist=list(),
           universeGeneIds=character(),
           catToGeneId=list()))

setClass("KEGGListHyperGResult",
         contains="ListHyperGResult",
         prototype=prototype(testname="KEGG"))

setClass("GeneralListHyperGResult",
         contains="ListHyperGResult",
         prototype=prototype(testname="General"))

setClass("miRNAListHyperGResult",
         contains="ListHyperGResult",
         prototype=prototype(testname="miRNA"))

setClass("CHRListHyperGResult",
         contains="ListHyperGResult",
         prototype=prototype(testname="CHR"))

setClass("GOListHyperGResult",
         representation=representation(conditional="logical"),
         contains="ListHyperGResult",
         prototype=prototype(testname="GO"))
