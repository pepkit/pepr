#' Create an absolute path from a primary target and a parent candidate.
#
#' @param perhapsRelative Path to primary target directory.
#' @param parent a path to parent folder to use if target isn't absolute.
#' @return Target itself if already absolute, else target nested within parent.
.makeAbsPath = function(perhapsRelative, parent) {
  if (!.isDefined(perhapsRelative)) return(perhapsRelative)
  perhapsRelative = .expandPath(perhapsRelative)
  if (.isAbsolute(perhapsRelative)) {
    abspath = perhapsRelative
  } else {
    abspath = file.path(normalizePath(parent), perhapsRelative)
  }
  if (!.isAbsolute(abspath)) 
    stop("Relative path", perhapsRelative, "and parent", parent ,
         "failed to create absolute path: ", abspath)
  return(abspath)
}

# Must test for is.null first, since is.na(NULL) returns a logical(0) which is
# not a boolean
.isDefined = function(var) {
  !(is.null(var) || is.na(var))
}

#' Determine whether a path is absolute.
#'
#' @param path The path to check for seeming absolute-ness.
#' @return Flag indicating whether the \code{path} appears to be absolute.
.isAbsolute = function(path) {
  if (!is.character(path)) stop("The path must be character")
  firstChar = substr(path, 1, 1)
  return(identical("/", firstChar) | identical("~", firstChar))
}

.safeFileExists = function(path) {
  ((!is.null(path)) && file.exists(path))
}

#' Listify data frame columns
#'
#' This function turns each data frame column into a list,
#' so that its cells can contain multiple elements
#' 
#' @param DF an object of class data.frame
#' @return an object of class data.frame
#'
#' @examples
#' dataFrame=mtcars
#' listifiedDataFrame=.listifyDF(dataFrame)
#' @export
.listifyDF = function(DF) {
  if(!is.data.frame(DF)) stop("The input object must be a data.frame.")
  colNames =  names(DF)
  for(iColumn in colNames){
    DF[[iColumn]] = as.list(DF[[iColumn]])
  }
  return(DF)
}


#' Handles the deprecation of the `sample_annotation` key in the `metadata` section of the config
#' 
#' If `sample_table` is not defined, the `sample_annotation` value is saved under `sample_table` key
#' The `sample_annotation` is then removed
#' Collect samples fulfilling the specified requirements
#' 
#' This funciton collects the samples from a \code{\link[data.table]{data.table-class}} object that
#' fulfill the requirements of an attribute \code{attr} specified with 
#' the \code{fun} argument
#' 
#' The anonymous function provided in the \code{func} argument has to return an integer that indicate the rows that the \code{action} should be performed on.
#' Core expressions which are most useful to implement the anonymous function are:
#' \itemize{
#' \item \code{\link[base]{which}} with inequality signs: \code{==,>,<}
#' \item \code{\link[base]{grep}}
#' }
#' 
#' @param samples an object of \code{\link[data.table]{data.table-class}} class
#' @param attr a string specifying a column in the \code{samples}
#' @param func an anonymous function, see Details for more information
#' @param action a string (either \code{include} or \code{exclude}) that specifies whether the function should select the row or exclude it.
#' 
#' @examples 
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_subprojects1", "project_config.yaml", package="pepr")
#' p = Project(projectConfig)
#' s = samples(p)
#' fetchSamples(s,attr = "sample_name", func=function(x){ which(x=="pig_0h") },action="include")
#' fetchSamples(s,attr = "sample_name", func=function(x){ which(x=="pig_0h") },action="exclude")
#' fetchSamples(s,attr = "sample_name", func=function(x){ grep("pig_",x) },action="include")
#' @export
fetchSamples = function(samples, attr=NULL, func=NULL, action="include") {
  if(!methods::is(samples, "data.table"))
    stop("'samples' argument has to be a data.table object, got: '",
         class(samples),"'")
  if(!action %in% c("include","exclude"))
    stop("'action' argument has to be either 'include' or 'exclude', got '",
         action,"'")
  attrNames = colnames(samples)
  if(!is.null(attr)){
    if (!attr %in% attrNames) 
      stop("The samples attribute '", attr,"' was not found.")
    if (!is.null(func)) {
      # use the anonymous function if provided
      if (is.function(func)) {
        rowIdx = tryCatch(
          expr = { 
            do.call(func,list(x=samples[[attr]]))
          }, error=function(e) {
            message("Error in your function: ")
            message(e)
          }, warning=function(w) {
            message("Warning in your function: ")
            message(w)
          }
        )
      } else {
        stop("The anonymous function you provided is invalid.")
      }
    }
  }
  if ((length(rowIdx) < 1) || (!methods::is(rowIdx,"integer"))) 
    stop("your function returned invalid indices: '", rowIdx,"'")
  # use action arg
  if (action=="include") {
    return(samples[rowIdx, ])
  } else {
    return(samples[!rowIdx, ])
  }
}

#' Similarily for `sample_subannotation` `subsample_table` pair
#' 
#' @param cfg list config
#' @return list config 
.handleSampleAnnotationDeprecation = function(cfg) {
  if (!is.null(cfg$metadata$sample_annotation)) {
    warning("'sample_annotation' key in the 'metadata' section of the config "
            ,"is deprecated. Use 'sample_table' instead.")
    if (is.null(cfg$metadata$sample_table)) {
      # save the sample_annotation under the sample_table key, delete the former
      cfg$metadata$sample_table = cfg$metadata$sample_annotation
    }
    cfg$metadata$sample_annotation = NULL
  }
  if (!is.null(cfg$metadata$sample_subannotation)) {
    warning("'sample_subannotation' key in the 'metadata' section of the config "
            ,"is deprecated. Use 'subsample_table' instead.")
    if (is.null(cfg$metadata$subsample_table)) {
      # save the sample_annotation under the sample_table key, delete the former
      cfg$metadata$subsample_table = cfg$metadata$sample_subannotation
    }
    cfg$metadata$sample_subannotation = NULL
  }
  return(cfg)
}

#' A designated place for any config transformations/sanity checks, like keys deprecation handling
#'
#' @param cfg config or a section of one to sanitize
#'
#' @return sanitized config
#' @export
.sanitizeConfig = function(cfg) {
  cfg = .handleSampleAnnotationDeprecation(cfg)
  if (!is.null(cfg$subprojects)) {
    for(i in seq_along(cfg$subprojects)) {
      cfg$subprojects[[i]] = 
        .handleSampleAnnotationDeprecation(cfg$subprojects[[i]])
    }
  }
  return(cfg)
}

#' Create a list of matched files in the system and unmatched regular expessions
#'
#' @param rgx string to expand in the system
#'
#' @return a list of all the elements after possible expansion
.matchesAndRegexes = function(rgx) {
  res = c()
  for(i in rgx){
    matched = Sys.glob(i)
    if(length(matched) < 1) {
      matched = i
    }
    res = c(res, matched)
  }
  return(list(res))
}
