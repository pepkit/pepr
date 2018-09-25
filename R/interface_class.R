



#' A class representing a Portable Encapsulated Project and Summarized Experiment objects interface
#' This is a helper that creates the Interface with empty Project and SummarizedExperiment objects included
#'
#' @param pep an object of \linkS4class{Project} class
#' @param  se and object of \linkS4class{SummarizedExperiment} class
#'
#' @export Interface
Interface = function(pep = NULL,
                     se = NULL) {
  new("Interface", pep = pep, se = se)
}

#' A class representing a Portable Encapsulated Project and Summarized Experiment objects interface
#'
#' Provides a representation and functions to access project
#' configuration and sample annotation values for a PEP as well as functions concerning experimental results organization provided by the \linkS4class{SummarizedExperiment} class.
#' Additionally, this class privides an interfece that connects them.
#'
#' @slot pep an object of \linkS4class{Project} class
#' @slot se and object of \linkS4class{SummarizedExperiment} class
#'
#' @exportClass Interface
setClass(
  "Interface",
  slots = c(pep = "Project", se = "SummarizedExperiment"),
  # Inherits from Project and SummarizedExperiment objects
  contains = c("Project", "SummarizedExperiment")
)

setMethod(
  f = "initialize",
  signature = "Interface",
  definition = function(.Object, pep, se) {
    # Functional object is created even if neither Project nor SummarizedExperiment object is provided
    if (missing(pep)) {
      .Object@pep = new("Project")
    } else{
      .Object@pep = pep
    }
    if (missing(se)) {
      .Object@se = new("SummarizedExperiment")
    } else{
      .Object@se = se
    }
    return(.Object)
  }
)

setMethod(
  f = "show",
  signature = "Interface",
  definition = function(.Object) {
    # TODO: Need to make the look consistent
    message("Project:")
    show(.Object@pep)
    message("")
    cat("SummarizedExperiment:\n")
    show(.Object@se)
    invisible(NULL)
  }
)


setGeneric("getColData", function(.Object, ...)
  standardGeneric("getColData"))

#' Get colData from Project object
#'
#' This method copies info about samples from \linkS4class{Project} object to \linkS4class{SummarizedExperiment} object.
#'
#' @param .Object An object of Interface class
#'
#' @return .Object An object of Interface class. The colData attribute of \linkS4class{SummarizedExperiment} is derived from samples attribute of \linkS4class{Project}
#' @export getColData
setMethod(
  "getColData",
  signature = "Interface",
  definition = function(.Object) {
    # Check the compatibility - dimensions
    if (NCOL(.Object@se) != NROW(.Object@pep@samples))
      stop(
        "The number of rows in Project samples (",
        NROW(.Object@pep@samples),
        ") and SummarizedExperiment colData columns (",
        NCOL(.Object@se),
        ") are not equal."
      )
    .Object@se@colData = DataFrame(.Object@pep@samples)
    return(.Object)
  }
)


setGeneric("getMetadata", function(.Object, ...)
  standardGeneric("getMetadata"))

#' Get metadata from Project object
#'
#' This method copies metadata from \linkS4class{Project} object to \linkS4class{MIAME} in the \linkS4class{SummarizedExperiment} object.
#'
#' @param .Object An object of Interface class
#'
#' @return .Object An object of Interface class. The metadata attribute of \linkS4class{SummarizedExperiment} is enriched with data from \linkS4class{Project} object as an additional \code{PEP config file} field in the MIAME object.
#' @export getMetadata
setMethod(
  "getMetadata",
  signature = "Interface",
  definition = function(.Object) {
    # Check if MIAME object exists in the metadata section of SummarizedExperiment, create new if not
    if (is.list(.Object@se@metadata) &
        length(.Object@se@metadata) == 0) {
      .Object@se@metadata = MIAME(name = "Experiment data",
                                  other = list(`PEP config file` = .Object@pep@file))
      # Otherwise append existing one with the PEP file info
    } else{
      .Object@se@metadata = lapply(.Object@se@metadata, function(x) {
        x = combine(x, MIAME(other = list(`PEP config file` = .Object@pep@file)))
        return(x)
      })
    }
    return(.Object)
  }
)
