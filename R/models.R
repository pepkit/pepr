#' Portable Encapsulated Project object
#'
#' Provides an in-memory representation and functions to access project
#' configuration and sample annotation values for a PEP.
#' 
#' Can be created with the constructor: \code{\link{Project}} 
#'
#' @slot file character vector path to config file on disk.
#' @slot samples a data table object holding the sample metadata
#' @slot config a list object holding contents of the config file
#'
#' @exportClass Project
setClass("Project",
         slots = c(
           file = "character",
           samples = "data.frame",
           config = "list"
         ))

#' The constructor of a class representing a Portable Encapsulated Project
#'
#' This is a helper that creates the project with empty samples and config slots
#'
#' @param file a character with project configuration yaml file
#' @param subproject a character with the subproject name to be activated
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_subprojects1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' @export Project
Project = function(file = character(),
                   # samples = list(),
                   # config = list(),
                   subproject = character()) {
  methods::new("Project", file = file, subproject = subproject)
}

#' Config objects are specialized list objects
#' 
#' Config objects are used with the \code{\link{Project-class}} objects
#' 
#'
#' @exportClass Config
setClass("Config", contains = "list")


# Override the standard generic show function for our config-style lists
setMethod(
  "show",
  signature = "Config",
  definition = function(object) {
    cat("PEP project object. Class:", class(object), fill = T)
    printNestedList(object)
    invisible(NULL)
  }
)


setMethod(
  "show",
  signature = "Project",
  definition = function(object) {
    cat("PEP project object. Class: ", class(object), fill = T)
    cat("  file: ", object@file, fill = T)
    cat("  samples: ", NROW(object@samples), fill = T)
    listSubprojects(object@config)
    invisible(NULL)
  }
)


setGeneric("config", function(object, ...)
  standardGeneric("config"))

#' View PEP config of the object of \code{\link{Project-class}} class
#'
#' This method can be used to view the config slot of
#' the \code{\link{Project-class}} class
#'
#' @param object an object of \code{\link{Project-class}} class
#'
#' @return a list with the config file
#'
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_subprojects1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' config(p)
#'
#' @export
setMethod(
  "config",
  signature = "Project",
  definition = function(object) {
    object@config
  }
)


setGeneric("samples", function(object, ...)
  standardGeneric("samples"))

#' View samples in the objects of \code{\link{Project-class}} class
#'
#' This method can be used to view the samples slot
#' of the \code{\link{Project-class}} class
#'
#' @param object an object of \code{\link{Project-class}} class
#'
#' @return a data.table with the config file
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_subprojects1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' samples(p)
#'
#' @export
setMethod(
  "samples",
  signature = "Project",
  definition = function(object) {
    object@samples
  }
)

setMethod("initialize", "Project", function(.Object, ...) {
  .Object = methods::callNextMethod(.Object)  # calls generic initialize
  ellipsis <- list(...)
  if (length(ellipsis$file) != 0) {
    # check if file path provided
    .Object@file = ellipsis$file
    .Object@config = loadConfig(ellipsis$file)
    if (length(ellipsis$subproject) != 0) {
      # check if subproject provided
      .Object = activateSubproject(.Object, ellipsis$subproject)
    } else{
      .Object = .loadSampleAnnotation(.Object)
      .Object = .loadSampleSubannotation(.Object)
      .Object = .applyConstantAttributes(.Object)
      .Object = .implyAttributes(.Object)
      .Object = .deriveAttributes(.Object)
    }
  }
  return(.Object)
})

setGeneric(name = "getSubsample", function(.Object, sampleName, subsampleName)
  standardGeneric("getSubsample"))

#' This method extracts the subsample from the \code{\link{Project-class}} object
#'
#' @param .Object An object of Project class
#'
#' @param sampleName character the name of the sample
#' @param subsampleName character the name of the subsample
#'
#' @return data.table one row data table with the subsample associated metadata
#' @examples
#' projectConfig = system.file(
#' "extdata",
#' "example_peps-master",
#' "example_subannotation1",
#' "project_config.yaml",
#' package = "pepr"
#' )
#' p = Project(projectConfig)
#' sampleName = "frog_1"
#' subsampleName = "sub_a"
#' getSubsample(p, sampleName, subsampleName)
#' @export
setMethod(
  f = "getSubsample",
  signature(
    .Object = "Project",
    sampleName = "character",
    subsampleName = "character"
  ),
  definition = function(.Object, sampleName, subsampleName) {
    if (is.null(.Object@samples$subsample_name))
      stop(
        "There is no subsample_name attribute in the subannotation
        table, therefore this method cannot be called."
      )
    sampleNames = unlist(.Object@samples$sample_name)
    rowNumber = which(sampleNames == sampleName)
    if (length(rowNumber) == 0)
      stop("Such sample name does not exist.")
    subsampleNames = .Object@samples$subsample_name[[rowNumber]]
    sampleNumber = which(subsampleNames == subsampleName)
    if (length(sampleNumber) == 0)
      stop("Such sample and sub sample name combination does not exist.")
    result = .Object@samples[1, ]
    for (iColumn in names(result)) {
      if (length(.Object@samples[[iColumn]][[rowNumber]]) > 1) {
        result[[iColumn]] = 
          .Object@samples[[iColumn]][[rowNumber]][[sampleNumber]]
      } else{
        result[[iColumn]] = .Object@samples[[iColumn]][[rowNumber]][[1]]
      }
    }
    return(result)
  }
)


.deriveAttributes = function(.Object) {
  # Backwards compatibility 
  # after change of derived columns to derived attributes
  if (is.null(.Object@config$derived_attributes)) {
    if (is.null(.Object@config$derived_columns)) {
      # If no derived columns and attributes found - return the unchanged object
      return(.Object)
    } else{
      # If the old naming scheme is used - copy the derived columns
      .Object@config$derived_attributes = .Object@config$derived_columns
      .Object@config[[which(names(.Object@config) == "derived_columns")]] = NULL
    }
  }
  # Set default derived columns - deprecated, to be deleted
  dc = unique(append(.Object@config$derived_attributes, "data_source"))
  .Object@config$derived_attributes = dc
  
  # Convert samples table into list of individual samples for processing
  cfg = .Object@config
  tempSamples = .Object@samples
  tempSamples[is.na(tempSamples)] = ""
  numSamples = nrow(tempSamples)
  if (numSamples == 0) {
    return(.Object)
  }
  
  listOfSamples = split(tempSamples, seq(numSamples))
  exclude = c()
  # Process derived attributes
  for (column in cfg$derived_attributes) {
    for (iSamp in seq_along(listOfSamples)) {
      samp = listOfSamples[[iSamp]]
      sampDataSource = unlist(samp[[column]])
      if (is.null(sampDataSource)) {
        # This sample lacks this derived attribute
        next
      }
      regex = cfg$data_sources[[sampDataSource]]
      if (!is.null(regex)) {
        samp[[column]] = list(strformat(regex, as.list(samp), exclude))
      }
      listOfSamples[[iSamp]] = samp
    }
    exclude = append(exclude, column)
  }
  
  # Reformat listOfSamples to a table
  .Object@samples = do.call(rbind, listOfSamples)
  .Object
}

.implyAttributes = function(.Object) {
  if (is.null(.Object@config$implied_attributes)) {
    # Backwards compatibility 
    # after change of implied columns to implied attributes
    if (is.null(.Object@config$implied_columns)) {
      # if the implied_attributes and implied_columns in project's config are
      # NULL, there is nothing that can be done
      return(.Object)
    } else{
      .Object@config$implied_attributes = .Object@config$implied_columns
      .Object@config[[which(names(.Object@config) == "implied_columns")]] = NULL
    }
  }
  if (is.list(.Object@config$implied_attributes)) {
    # the implied_attributes in project's config is a list,
    # so the columns can be implied
    samplesDims = dim(.Object@samples)
    primaryColumn = names(.Object@config$implied_attributes)
    for (iColumn in primaryColumn) {
      primaryKeys = names(.Object@config$implied_attributes[[iColumn]])
      for (iKey in primaryKeys) {
        newValues = names(.Object@config$implied_attributes[[iColumn]][[iKey]])
        for (iValue in newValues) {
          samplesColumns = names(.Object@samples)
          if (any(samplesColumns == iValue)) {
            # The implied column has been already added, populating
            .Object@samples[[iValue]][which(.Object@samples[[iColumn]] == iKey)] = 
              .Object@config$implied_attributes[[iColumn]][[iKey]][[iValue]]
          } else{
            # The implied column is missing, adding column and populating
            toBeAdded = data.frame(rep("", samplesDims[1]), stringsAsFactors =
                                     FALSE)
            names(toBeAdded) = iValue
            toBeAdded[which(.Object@samples[[iColumn]] == iKey), 1] = 
              .Object@config$implied_attributes[[iColumn]][[iKey]][[iValue]]
            .Object@samples = cbind(.Object@samples, toBeAdded)
          }
        }
      }
    }
  } else{
    # the implied_attributes in project's config is neither NULL nor list
    cat("The implied_attributes key-value pairs in project config are invalid!",
        fill = T)
  }
  return(.Object)
}

.loadSampleAnnotation = function(.Object) {
  # Can use fread if data.table is installed, otherwise use read.table
  if (requireNamespace("data.table")) {
    sampleReadFunc = data.table::fread
  } else {
    sampleReadFunc = utils::read.table
  }
  sampleAnnotationPath = .Object@config$metadata$sample_annotation
  if (.safeFileExists(sampleAnnotationPath)) {
    samples = sampleReadFunc(sampleAnnotationPath)
  } else{
    cat("No sample annotation file:", sampleAnnotationPath, fill = T)
    stop()
    samples = data.frame()
  }
  .Object@samples = samples
  return(.Object)
}

.loadSampleSubannotation = function(.Object) {
  # Extracting needed slots
  sampleSubannotationPath = .Object@config$metadata$sample_subannotation
  samples = .Object@samples
  samples = listifyDF(samples)
  #Reading sample subannonataion table, just like in annotation table
  if (requireNamespace("data.table")) {
    sampleSubReadFunc = data.table::fread
  } else {
    sampleSubReadFunc = utils::read.table
  }
  if (.safeFileExists(sampleSubannotationPath)) {
    samplesSubannotation = sampleSubReadFunc(sampleSubannotationPath)
  } else{
    samplesSubannotation = data.frame()
  }
  subNames = unique(samplesSubannotation$sample_name)
  rowNum = nrow(samples)
  # Creating a list to be populated in the loop and inserted
  # into the samples data.frame as a column. This way the "cells"
  # in the samples table can consist of multiple elements
  colList = vector("list", rowNum)
  for (iName in subNames) {
    whichNames = which(samplesSubannotation$sample_name == iName)
    subTable = samplesSubannotation[whichNames, ]
    dropCol = which(names(samplesSubannotation[whichNames, ]) == "sample_name")
    subTable = subset(subTable, select = -dropCol)
    for (iColumn in seq_len(ncol(subTable))) {
      colName = names(subset(subTable, select = iColumn))
      if (!any(names(samples) == colName)) {
        # The column doesn't exist, creating
        samples[, colName] = NULL
      } else{
        # colList=as.list(unname(samples[, ..colName]))[[1]]
        colList = samples[[colName]]
      }
      # The column exists
      whichColSamples = which(names(samples) == colName)
      whichRowSamples = which(samples$sample_name == iName)
      # Inserting element(s) into the list
      colList[[whichRowSamples]] = subTable[[colName]]
      # Inserting the list as a column in the data.frame
      samples[[colName]] = colList
    }
  }
  samples[is.na(samples)] = ""
  .Object@samples = samples
  return(.Object)
}

.applyConstantAttributes <- function(.Object) {
  # Extracting needed slots
  constants = .Object@config$constants
  if (is.list(constants)) {
    # get names
    constantsNames = names(constants)
    # get a copy of samples to get the dimensions
    samplesDF = .Object@samples
    colLen = dim(samplesDF)[1]
    for (iConst in seq_along(constants)) {
      # create a data.table - one column and glue
      # it with the current samples data.table
      constantCol = data.table::data.table(rep(constants[[iConst]], colLen))
      names(constantCol) = constantsNames[iConst]
      .Object@samples = cbind(.Object@samples, constantCol)
    }
  }
  return(.Object)
}

setGeneric("activateSubproject", function(.Object, sp, ...)
  standardGeneric("activateSubproject"))

#' Activate other subproject in objects of \code{\link{Project-class}} class
#'
#' This method switches the between the subprojects
#' within the \code{\link{Project-class}} object
#'
#' To check what are the subproject names
#' call \code{listSubprojects(config(p))}, where \code{p} is the object
#' of \code{\link{Project-class}} class
#'
#' @param .Object an object of class \code{\link{Project-class}}
#' @param sp character with the subproject name
#' @examples
#'
#' @export
setMethod(
  f = "activateSubproject",
  signature = signature(.Object = "Project", sp = "character"),
  definition = function(.Object, sp, ...) {
    .Object@config = .updateSubconfig(.Object@config, sp)
    
    # Ensure that metadata paths are absolute and return the config.
    # This used to be all metadata columns; now it's just: results_subdir
    mdn = names(.Object@config$metadata)
    
    .Object@config$metadata = 
      makeMetadataSectionAbsolute(.Object@config,
                                  parent = dirname(.Object@file))
    
    .Object = .loadSampleAnnotation(.Object)
    .Object = .loadSampleSubannotation(.Object)
    .Object = .applyConstantAttributes(.Object)
    .Object = .implyAttributes(.Object)
    .Object = .deriveAttributes(.Object)
    .Object
  }
)




#' Print a nested list
#'
#' Prints a nested list in a way that looks nice
#'
#' Useful for displaying the config of a PEP
#'
#' @param lst list object to print
#'
#' @examples
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_basic",
#' "project_config.yaml",
#' package = "pepr")
#' p = Project(file = projectConfig)
#' printNestedList(config(p))
#' @export
printNestedList = function(lst, level = 0) {
  if (!is.list(lst))
    stop("The input is not a list, cannot be displayed.")
  for (itemName in names(lst)) {
    item = lst[[itemName]]
    if (class(item) == "list") {
      cat(rep(" ", level), paste0(itemName, ":"), fill = T)
      printNestedList(item, level + 2)
    } else {
      if (is.null(item))
        item = "null"
      cat(rep(" ", level), paste0(itemName, ":"), item, fill = T)
    }
  }
}
