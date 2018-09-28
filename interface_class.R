

#' A class representing a Portable Encapsulated Project and Summarized Experiment objects interface
#' This is a helper that creates the Interface with empty Project and SummarizedExperiment objects included
#'
#' @param pep an object of \linkS4class{Project} class
#' @inheritParams  SummarizedExperiment::SummarizedExperiment 
#'
#' @export Interface
Interface = function(pep = NULL, ...) {
  # The three dots pass the arguments
  rse=SummarizedExperiment(...)
  new("Interface", rse, pep=pep)
}

#' A class representing a Portable Encapsulated Project and Summarized Experiment objects interface
#'
#' Provides a representation and functions to access project
#' configuration and sample annotation values for a PEP as well as functions concerning experimental results organization provided by the \linkS4class{SummarizedExperiment} class.
#' Additionally, this class privides an interfece that connects them.
#'
#' @slot pep an object of \linkS4class{Project} class
#'
#' @exportClass Interface
setClass(
  "Interface",
  slots = c(pep = "Project"),
  # Inherits from Project and SummarizedExperiment objects
  contains ="RangedSummarizedExperiment"
)

setMethod(
  f = "initialize",
  signature = "Interface",
  definition = function(.Object, pep, ...) {
    # if no pep argument provided an empty Project object is created
    if (is.null(pep)) pep = new("Project")
    # call initialize method of SummarizedExperiment class
    .Object=callNextMethod()
    return(.Object)
  }
)

setMethod(
  f = "show",
  signature = "Interface",
  definition = function(.Object) {
    # TODO: Need to make the look consistent
    cat("\n")
    callNextMethod(.Object)
    cat("\n")
    show(.Object@pep)
    invisible(NULL)
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
    if (NCOL(.Object) != NROW(.Object@pep@samples))
      stop(
        "The number of rows in Project samples (",
        NROW(.Object@pep@samples),
        ") and SummarizedExperiment colData columns (",
        NCOL(.Object),
        ") are not equal."
      )
    colData(.Object) = DataFrame(.Object@pep@samples)
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
      metadata(.Object) = list(MIAME(other = list(`PEP config file` = .Object@pep@file)))
      # Otherwise append existing one with the PEP file info
    } else{
      metadata(.Object) = lapply(metadata(.Object), function(x) {
        x = tryCatch({
          combine(x, MIAME(other = list(`PEP config file` = .Object@pep@file)))
        }, error = function(e) {
          stop("The metadata section of the Interface object is not a MIAME object.")
        })
        return(x)
      })
    }
    return(.Object)
  }
)