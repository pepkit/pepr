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
    ellipsisArgs = list(...)
    .Object=callNextMethod(.Object, file)
    argsNames=names(ellipsisArgs)
    if(any(argsNames=="assays")) .Object@assays=Assays(ellipsisArgs$assays)
    if(any(argsNames=="colData")) .Object@colData=ellipsisArgs$colData
    if(any(argsNames=="rowRanges")) .Object@rowRanges=ellipsisArgs$rowRanges
    return(.Object)
  }
)


