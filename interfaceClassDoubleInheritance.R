setClass("Interface",
         # Inherits from Project and SummarizedExperiment objects
         contains = c("RangedSummarizedExperiment", "Project"))

Interface <- function(file, ...) {
  new("Interface", file, ...)
}

setMethod(
  f = "initialize",
  signature = "Interface",
  definition = function(.Object, file, ...) {
    ellipsisArgs = list(...)
    .Object = callNextMethod(.Object, file)
    argsNames = names(ellipsisArgs)
    if (any(argsNames == "assays"))
      .Object@assays = Assays(ellipsisArgs$assays)
    if (any(argsNames == "colData"))
      .Object@colData = ellipsisArgs$colData
    if (any(argsNames == "rowRanges")) {
      .Object@rowRanges = ellipsisArgs$rowRanges
      elementMetadata = S4Vectors:::make_zero_col_DataFrame(length(rowRanges))
      .Object@elementMetadata = elementMetadata
    }
    return(.Object)
  }
)

setMethod(
  f = "show",
  signature = "Interface",
  definition = function(.Object) {
    do.call(selectMethod(f = "show", signature = "RangedSummarizedExperiment"),
            list(.Object))
    cat("\n")
    do.call(selectMethod(f = "show", signature = "Project"), list(.Object))
  }
)

setGeneric("getColData", function(.Object, ...)
  standardGeneric("getColData"))

#' Get colData from the Project object (PEP)
#'
#' This method copies info about samples from \linkS4class{Project} object to \code{colData} slot on the \code{Interface} object.
#'
#' @param .Object An object of Interface class
#'
#' @return .Object An object of \code{Interface} class. The colData slot is derived from samples attribute of \linkS4class{Project}
#' @export getColData
setMethod(
  "getColData",
  signature = "Interface",
  definition = function(.Object) {
    # Check the compatibility - dimensions
    if (NCOL(.Object) != NROW(.Object@samples))
      stop(
        "The number of rows in Project samples (",
        NROW(.Object@samples),
        ") and SummarizedExperiment colData columns (",
        NCOL(.Object),
        ") are not equal."
      )
    colData(.Object) = DataFrame(.Object@samples)
    return(.Object)
  }
)

setGeneric("getMetadata", function(.Object, ...)
  standardGeneric("getMetadata"))

#' Get metadata from the Project object (PEP)
#'
#' This method copies metadata from \linkS4class{Project} object to \linkS4class{MIAME} in the \code{Interface} object.
#'
#' @param .Object An object of Interface class
#'
#' @return .Object An object of Interface class. The metadata slot of \code{Interface} is enriched with data from \linkS4class{Project} object as an additional \code{PEP config file} field in the MIAME object.
#' @export getMetadata
setMethod(
  "getMetadata",
  signature = "Interface",
  definition = function(.Object) {
    # Check if MIAME object exists in the metadata section of SummarizedExperiment, create new if not
    if (is.list(metadata(.Object)) &
        length(metadata(.Object)) == 0) {
      metadata(.Object) = list(MIAME(other = list(`PEP config file` = .Object@config)))
      # Otherwise append existing one with the PEP file info
    } else{
      metadata(.Object) = lapply(metadata(.Object), function(x) {
        x = tryCatch({
          combine(x, MIAME(other = list(`PEP config file` = .Object@config)))
        }, error = function(e) {
          stop("The metadata section of the Interface object is not a MIAME object.")
        })
        return(x)
      })
    }
    return(.Object)
  }
)

# setGeneric("metadata", function(.Object, ...)
#   standardGeneric("metadata"))

setMethod("metadata",
          signature = "Interface",
          definition = function(x) {
            met=do.call(selectMethod(f = "metadata",signature = "SummarizedExperiment"),list(x))
            if(length(met)>0){
              met=met[[1]]
              config=met@other$`PEP config file`
              met@other=list()
              show(met)
              printNestedList(config)
            }else{
              do.call(selectMethod(f = "metadata",signature = "SummarizedExperiment"),list(x))
            }
          })
