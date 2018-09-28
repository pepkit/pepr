setClass(
  "Interface",
  # Inherits from Project and SummarizedExperiment objects
  contains =c("RangedSummarizedExperiment","Project")
)

Interface <- function(file, ...) {
  new("Interface", file, ...)
}

setMethod(
  f = "initialize",
  signature = "Interface",
  definition = function(.Object, file, ...) {
    ellipsisArgs = list(...); cat("Ellipsis args:\n"); print(ellipsisArgs)
    .Object=callNextMethod(.Object, file)
    argsNames=names(ellipsisArgs)
    # for(arg in seq_along(ellipsisArgs)){
    #   print(argsNames[arg])
    #   if(argsNames[arg] == "assay") ellipsisArgs[[argsNames[arg]]] = Assays(ellipsisArgs[[argsNames[arg]]])
    #   slot(.Object,argsNames[arg]) = ellipsisArgs[[argsNames[arg]]]
    # }
    .Object@assays=Assays(assays = assays)
    .Object@colData=colData
    .Object@rowRanges=rowRanges
    return(.Object)
  }
)


