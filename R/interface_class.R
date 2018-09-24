


Interface = function(pep = NULL,
                     se = NULL) {
  new("Interface", pep = pep, se = se)
}

setClass(
  "Interface",
  slots = c(pep = "Project", se = "SummarizedExperiment"),
  contains = c("Project", "SummarizedExperiment")
)

setMethod(
  f = "initialize",
  signature = "Interface",
  definition = function(.Object, pep, se) {
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
    message("")
    message("Project:")
    show(.Object@pep)
    message("")
    message("SummarizedExperiment:")
    show(.Object@se)
    invisible(NULL)
  }
)


setGeneric("getColData", function(.Object, ...)
  standardGeneric("getColData"))

#' @export
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
