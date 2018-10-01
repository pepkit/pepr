
#' A class representing a Portable Encapsulated Project and Summarized Experiment objects interface
#'
#' Provides a representation and functions to access project
#' configuration and sample annotation values for a PEP as well as functions concerning experimental results organization provided by the \linkS4class{SummarizedExperiment} class.
#' Additionally, this class privides an interfece that connects them.
#' 
#' This class inherits from classes \linkS4class{Project} and \linkS4class{RangedSummarizedExperiment}
#'
#' @exportClass Interface
setClass("Interface",
         # Inherits from Project and SummarizedExperiment objects
         contains = c("RangedSummarizedExperiment", "Project"))

#' A class representing a Portable Encapsulated Project and Summarized Experiment objects interface
#' This is a helper that creates the Interface with empty Project and SummarizedExperiment objects included
#'
#' @param file a string with a path to the config file as in \linkS4class{Project}
#' @inheritParams  SummarizedExperiment::SummarizedExperiment 
#'
#' @export Interface
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
    # Adds slots depending on the provided arguments in the constructor call
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


setMethod("metadata",
          signature = "Interface",
          definition = function(x) {
            # gets the metadata into variable
            met=do.call(selectMethod(f = "metadata",signature = "SummarizedExperiment"),list(x))
            # if there is any metadata
            if(length(met)>0){
              # extract the PEP config data
              met=met[[1]]
              config=met@other$`PEP config file`
              met@other=list()
              # print the remaining part
              show(met)
              # print the config 
              printNestedList(config)
            }else{
              # if no metadata, print empty list
              do.call(selectMethod(f = "metadata",signature = "SummarizedExperiment"),list(x))
            }
          })
