# constant variables declarations
CFG_SAMPLE_TABLE_KEY = "sample_table"
CFG_SUBSAMPLE_TABLE_KEY = "subsample_table"
REQ_ABS = c(CFG_SAMPLE_TABLE_KEY, CFG_SUBSAMPLE_TABLE_KEY)
CFG_VERSION_KEY = "pep_version"
CFG_MODIFIERS_KEY = "sample_modifiers"
CFG_APPEND_KEY = "append"
CFG_IMPLY_KEY = "imply"
CFG_DERIVE_KEY = "derive"
CFG_IMPLY_THEN_KEY = "then"
CFG_IMPLY_IF_KEY = "if"
CFG_DERIVE_ATTRS_KEY = "attributes"
CFG_DERIVE_SOURCES_KEY = "sources"
CFG_IMPORTS_KEY = "imports"

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
Project = function(file = NULL, amendments = NULL) {
  methods::new("Project", file = file, amendments = amendments)
}

#' Config objects are specialized list objects
#'
#' Config objects are used with the \code{\link{Project-class}} objects
#'
#' @exportClass Config
setClass("Config", contains = "list")

# Override the standard generic show function for our config-style lists
setMethod(
  "show",
  signature = "Config", 
  definition = function(object) {
    cat("Config object. Class:", class(object), fill = T)
    .printNestedList(object)
    invisible(NULL)
  }
)

#' Make sections absolute in the config
#' 
#' @export
setGeneric("makeSectionsAbsolute", function(object, sections, cfgPath)
  standardGeneric("makeSectionsAbsolute"))

setMethod(
  "makeSectionsAbsolute", 
  signature = signature(
    object="Config", 
    sections="character", 
    cfgPath="character"
    ), 
  definition = function(object, sections, cfgPath) {
    # Enable creation of absolute path using given parent folder path.
    absViaParent = pryr::partial(.makeAbsPath, parent=dirname(cfgPath))
    for(section in sections){
      if (section %in% names(object)){
        absSectionVals = c()
        for (iSection in object[section]){
          absSection = absViaParent(iSection)
          absSectionVals = append(absSectionVals, absSection)
        }
        object[section] = absSectionVals
      }
    }
  return(object)
  }
)

#' Check config spec version and reformat if needed
#'
#' @param object an object of \code{\link{Config-class}} 
#' 
#' @return an object of \code{\link{Config-class}} 
setGeneric(".reformat", function(object)
  standardGeneric(".reformat"))

setMethod(
  ".reformat", 
  signature = "Config", 
  definition = function(object) {
    if (CFG_VERSION_KEY %in% names(object)){
      split = str_split(object[[CFG_VERSION_KEY]],"\\.")[[1]]
      if (length(split) < 3) stop("PEP version string is not tripartite")
      majorVer = as.numeric(split[1])
      if (majorVer < 2){
        if (CFG_MODIFIERS_KEY %in% names(object)){
          stop("Project configuration file subscribes to specification 
               >= 2.0.0, since ",CFG_MODIFIERS_KEY," section is defined. Set ",
               CFG_VERSION_KEY, " to 2.0.0 in your config")
        } else{
          stop("Config file reformatting is not supported. 
               Reformat the config manually.")
        }
      }
    } else{
      stop("Config file is missing ", CFG_VERSION_KEY, " key. 
           Add it to the config manually.")
    }
    return(object)
  }
)

setGeneric("checkSection", function(object, sectionNames)
  standardGeneric("checkSection"))

#' Check for existence of a section in the Project config
#' 
#' This function checks for the section/nested sections in the config YAML file. Returns \code{TRUE} if it exist(s) or \code{FALSE} otherwise.
#' 
#' Element indices can be used instead of the actual names, see \code{Examples}.
#' 
#' @param object object of \code{\link[pepr]{Config-class}}
#' @param sectionNames the name of the section or names of the nested sections to look for
#' 
#' @return a logical indicating whether the section exists
#' 
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_subprojects1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' checkSection(config(p),sectionNames = c("subprojects","newLib","metadata"))
#' checkSection(config(p),sectionNames = c("subprojects",1,"metadata"))
#' @export
setMethod(
  "checkSection",
  signature = "Config",
  definition = function(object, sectionNames) {
    # try co convert the section name to numeric, return original name if 
    # not possible this enables the outer method to check the sections 
    # existance by index and by name at the same time
    tryToNum = function(x){
        convertedX = suppressWarnings(as.numeric(x))
        ifelse(!is.na(convertedX), convertedX, x)
    }
    testList = object
    counter = 1
    while (!is.na(sectionNames[counter])) {
      item = tryToNum(sectionNames[counter])
      if((!is.list(testList)) || is.null(testList[[item]])){
        return(FALSE)
      }
      testList = testList[[item]]
      counter = counter + 1
    }
    return(TRUE)
  }
)

setMethod(
  "show",
  signature = "Project",
  definition = function(object) {
    cat("PEP project object. Class: ", class(object), fill = T)
    cat("  file: ", object@file, fill = T)
    cat("  samples: ", NROW(object@samples), fill = T)
    if (length(object@config) != 0) {
      .listAmendments(object@config, style="cat")
    }
    invisible(NULL)
  }
)


#' View PEP config of the object of \code{\link{Project-class}}
#'
#' This method can be used to view the config slot of
#' the \code{\link{Project-class}} class
#'
#' @param object an object of \code{\link{Project-class}}
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
setGeneric("config", function(object)
  standardGeneric("config"))

setMethod(
  "config",
  signature = "Project",
  definition = function(object) {
    object@config
  }
)


#' View samples in the objects of \code{\link{Project-class}} 
#'
#' This method can be used to view the samples slot
#' of the \code{\link{Project-class}} class
#'
#' @param object an object of \code{\link{Project-class}} 
#'
#' @return a data.table with the with metadata about samples
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_subprojects1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' sampleTable(p)
#'
#' @export
setGeneric("sampleTable", function(object)
    standardGeneric("sampleTable"))

setMethod(
  "sampleTable",
  signature = "Project",
  definition = function(object) {
    object@samples
  }
)

setMethod("initialize", "Config", function(.Object, data) {
  .Object = methods::callNextMethod(.Object, data)  # calls list initialize
  .Object = .reformat(.Object)
  return(.Object)
})

setMethod("initialize", "Project", function(.Object, ...) {
  .Object = methods::callNextMethod(.Object)  # calls generic initialize
  ellipsis <- list(...)
  if (!is.null(ellipsis$file)) {
    # check if file path provided
    .Object@file = ellipsis$file
    message("Loading config file: ", ellipsis$file)
    cfg_data = .loadConfig(filename = ellipsis$file, amendments = ellipsis$amendments)
    .Object@config = methods::new("Config",data=cfg_data)
    # list available amendments
    .listAmendments(.Object@config)
    .Object@config = makeSectionsAbsolute(.Object@config, REQ_ABS, .Object@file)
    .Object = .loadSampleAnnotation(.Object)
    .Object = .modifySamples(.Object)
  }
  return(.Object)
})


#' Perform all the sample addribute modifications
#'
#' @param object an object of \code{\link{Project-class}} 
#'
#' @return
setGeneric(".modifySamples", function(object)
  standardGeneric(".modifySamples"))

setMethod(
  ".modifySamples",
  signature = "Project",
  definition = function(object) {
    if (!CFG_MODIFIERS_KEY %in% names(config(object))) return(object)
    object = .appendAttrs(object)
    object = .implyAttrs(object)
    object = .mergeAttrs(object)
    object = .deriveAttrs(object)
    return(object)
  }
)


#' This method extracts the sample from the \code{\link{Project-class}} object
#'
#' @param .Object An object of Project class
#'
#' @param sampleName character the name of the sample
#'
#' @return data.table one row data table with the sample associated metadata
#' @examples
#' projectConfig = system.file(
#' "extdata",
#' "example_peps-master",
#' "example_basic",
#' "project_config.yaml",
#' package = "pepr"
#' )
#' p = Project(projectConfig)
#' sampleName = "frog_1"
#' getSample(p, sampleName)
#' @export
setGeneric(name = "getSample", function(.Object, sampleName)
  standardGeneric("getSample"))

setMethod(
  f = "getSample",
  signature(.Object = "Project",
            sampleName = "character"),
  definition = function(.Object, sampleName) {
    sampleNames = unlist(.Object@samples$sample_name)
    rowNumber = which(sampleNames == sampleName)
    if (length(rowNumber) == 0)
      stop("Such sample name does not exist.")
    result = sampleTable(.Object)[rowNumber, ]
    return(result)
  }
)


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
#' "example_subtable1",
#' "project_config.yaml",
#' package = "pepr"
#' )
#' p = Project(projectConfig)
#' sampleName = "frog_1"
#' subsampleName = "sub_a"
#' getSubsample(p, sampleName, subsampleName)
#' @export
setGeneric(name = "getSubsample", function(.Object, sampleName, subsampleName)
  standardGeneric("getSubsample"))

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
        "There is no subsample_name attribute in the subannotation", 
        " table, therefore this method cannot be called."
      )
    sampleNames = unlist(.Object@samples$sample_name)
    rowNumber = which(sampleNames == sampleName)
    if (length(rowNumber) == 0)
      stop("Such sample name does not exist.")
    subsampleNames = .Object@samples$subsample_name[[rowNumber]]
    sampleNumber = which(subsampleNames == subsampleName)
    if (length(sampleNumber) == 0)
      stop("Such sample and sub sample name combination does not exist.")
    result = .Object@samples[1,]
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

#' Lists subprojects in a \code{\link{Project-class}} object
#'
#' Lists available subprojects within a \code{\link{Project-class}} object.
#'
#' The subprojects can be activated by passing their names to the  \code{\link{activateAmendments}} method
#'
#' @param .Object an object of \code{\link{Project-class}}
#' @return names of the available subprojects
#' @examples 
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_subprojects1",
#' "project_config.yaml",
#' package = "pepr")
#' p = Project(file = projectConfig)
#' availSubprojects = listAmendments(p)
#' activateAmendments(p,availSubprojects[1])
#' @export
setGeneric("listAmendments", function(.Object)
  standardGeneric("listAmendments"))

setMethod(
  f = "listAmendments",
  signature = signature(.Object = "Project"),
  definition = function(.Object) {
    config = config(.Object)
    .listAmendments(cfg = config, style="message")
  }
)


#' Activate other subproject in objects of \code{\link{Project-class}}
#'
#' This method switches between the subprojects
#' within the \code{\link{Project-class}} object
#'
#' To check what are the subproject names
#' call \code{listAmendments(p)}, where \code{p} is the object
#' of \code{\link{Project-class}} class
#'
#' @param .Object an object of class \code{\link{Project-class}}
#' @param amendments character with the subproject name
#' 
#' @examples 
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_subprojects1",
#' "project_config.yaml",
#' package = "pepr")
#' p = Project(file = projectConfig)
#' availSubprojects = listAmendments(p)
#' activateAmendments(p,availSubprojects[1])
#' @export
setGeneric("activateAmendments", function(.Object, amendments)
  standardGeneric("activateAmendments"))


setMethod(
  f = "activateAmendments",
  signature = signature(.Object="Project", amendments="character"),
  definition = function(.Object, amendments) {
    # .Object@config = .sanitizeConfig(.Object@config)
    # .Object@config = .updateSubconfig(.Object@config, amendments)
    .Object@config = .applyAmendments(.Object@config, amendments)
    .Object@config = makeSectionsAbsolute(
      .Object@config, 
      c(CFG_SAMPLE_TABLE_KEY, CFG_SUBSAMPLE_TABLE_KEY), 
      .Object@file
    )
    .Object = .loadSampleAnnotation(.Object)
    .Object = .modifySamples(.Object)
    return(.Object)
  }
)

#' Append constant attributes across all the samples
#'
#' @param .Object an object of \code{\link{Project-class}} 
#'
#' @return an object of \code{\link{Project-class}} 
#'
#' @examples
.appendAttrs <- function(.Object) {
  modifiers = config(.Object)[[CFG_MODIFIERS_KEY]]
  if (!CFG_APPEND_KEY %in% names(modifiers)) return(.Object)
  constants = modifiers[[CFG_APPEND_KEY]]
  if (is.list(constants)) {
    # get names
    constantsNames = names(constants)
    # get a copy of samples to get the dimensions
    colLen = dim(sampleTable(.Object))[1]
    for (iConst in seq_along(constants)) {
      # create a one column data.table and glue appand it with to the 
      # current samples data.table
      constantCol = data.table::data.table(rep(constants[[iConst]], colLen))
      names(constantCol) = constantsNames[iConst]
      .Object@samples = cbind(.Object@samples, constantCol)
    }
  }
  return(.Object)
}

#' Imply attributes
#'
#' @param .Object an object of \code{\link{Project-class}} 
#'
#' @return an object of \code{\link{Project-class}} 
#'
#' @examples
.implyAttrs = function(.Object) {
  modifiers = config(.Object)[[CFG_MODIFIERS_KEY]]
  if (!CFG_IMPLY_KEY %in% names(modifiers)) return(.Object)
  implications = modifiers[[CFG_IMPLY_KEY]]
  for (implication in implications) {
    if (!(CFG_IMPLY_IF_KEY %in% names(implication) && CFG_IMPLY_THEN_KEY %in% names(implication)))
      stop(CFG_IMPLY_KEY, " section is not formatted properly")
    implierAttrs = names(implication[[CFG_IMPLY_IF_KEY]])
    implierVals = as.character(implication[[CFG_IMPLY_IF_KEY]])
    impliedAttrs = names(implication[[CFG_IMPLY_THEN_KEY]])
    impliedVals = as.character(implication[[CFG_IMPLY_THEN_KEY]])
    attrs = colnames(.Object@samples)
    if (!all(implierAttrs %in% attrs)) next
    hitIds = list()
    for (i in seq_along(implierAttrs)) {
      hitIds[[i]] = which(.Object@samples[,implierAttrs[i]] == implierVals[i])
      if (length(hitIds) < 1) break
    }
    qualIds = Reduce(intersect, hitIds)
    if (length(qualIds) < 1) next
    for (i in seq_along(impliedAttrs)){
      if (!impliedAttrs[i] %in% attrs) .Object@samples[,impliedAttrs[i]] = ""
      .Object@samples[qualIds, impliedAttrs[i]] = impliedVals[i]
    }
  }
  return(.Object)
}

#' Derive attributes
#'
#' @param .Object an object of \code{\link{Project-class}} 
#'
#' @return an object of \code{\link{Project-class}} 
#'
.deriveAttrs = function(.Object) {
  parentDir = dirname(.Object@file)
  modifiers = config(.Object)[[CFG_MODIFIERS_KEY]]
  if (!CFG_DERIVE_KEY %in% names(modifiers)) return(.Object)
  derivations = modifiers[[CFG_DERIVE_KEY]]
  if (!all(c(CFG_DERIVE_ATTRS_KEY, CFG_DERIVE_SOURCES_KEY) %in% names(derivations)))
    stop(CFG_DERIVE_KEY, " section is not formatted properly")
  for (derivedAttr in derivations[[CFG_DERIVE_ATTRS_KEY]]) {
    derivedSamplesVals = .Object@samples[,derivedAttr]
    for (derivedSource in names(derivations[[CFG_DERIVE_SOURCES_KEY]])){
      hitIds = which(derivedSamplesVals == derivedSource)
      if (length(hitIds) < 1) next
      for (hitId in hitIds){
        rgx = derivations[[CFG_DERIVE_SOURCES_KEY]][[derivedSource]]
        res = .matchesAndRegexes(.strformat(rgx, as.list(sampleTable(.Object)[hitId,]), parentDir))  
        .Object@samples[hitId,derivedAttr] = list(res)
      }
    }
  }
  return(.Object)
}


#' Read sample annotation from disk
#'
#' @param .Object an object of \code{\link{Project-class}} 
#'
#' @return an object of \code{\link{Project-class}} 
#'
.loadSampleAnnotation = function(.Object) {
  # Can use fread if data.table is installed, otherwise use read.table
  if (requireNamespace("data.table")) {
    sampleReadFunc = data.table::fread
  } else {
    sampleReadFunc = utils::read.table
  }
  cfg = config(.Object)
  if (!CFG_SAMPLE_TABLE_KEY %in% names(cfg)) return(.Object)
  sampleAnnotationPath = cfg[[CFG_SAMPLE_TABLE_KEY]]
  if(.safeFileExists(sampleAnnotationPath)){
    samples = sampleReadFunc(sampleAnnotationPath)
  } else{
    warning("The sample_table does not exist: ", sampleAnnotationPath)
    return(.Object)
  }
  .Object@samples = samples
  return(.Object)
}

#' Merge samples defined in sample table with ones in subsample table
#'
#' @param .Object an object of \code{\link{Project-class}} 
#'
#' @return an object of \code{\link{Project-class}} 
#'
#' @examples
.mergeAttrs = function(.Object){
  cfg = config(.Object)
  if (!CFG_SUBSAMPLE_TABLE_KEY %in% names(cfg)) return(.Object)
  sampleSubannotationPath = cfg[[CFG_SUBSAMPLE_TABLE_KEY]]
  samples = sampleTable(.Object)
  samples = .listifyDF(samples)
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
    subTable = samplesSubannotation[whichNames,]
    dropCol = which(names(samplesSubannotation[whichNames,]) == "sample_name")
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
      if(length(whichRowSamples) < 1){
        warning("No samples named '", iName, "'")
      } else {
        # Inserting element(s) into the list
        colList[[whichRowSamples]] = subTable[[colName]]
        # Inserting the list as a column in the data.frame
        samples[[colName]] = colList
      }
    }
  }
  samples[is.na(samples)] = ""
  .Object@samples = samples
  return(.Object)
}

#' Print a nested list
#'
#' Prints a nested list in a way that looks nice
#'
#' Useful for displaying the config of a PEP
#'
#' @param lst list object to print
#' @param level the indentation level
#'
#' @examples
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_basic",
#' "project_config.yaml",
#' package = "pepr")
#' p = Project(file = projectConfig)
#' .printNestedList(config(p),level=2)
#' @export
.printNestedList = function(lst, level = 0) {
  if (!is.list(lst))
    stop("The input is not a list, cannot be displayed.")
  for (itemName in names(lst)) {
    item = lst[[itemName]]
    if (class(item) == "list") {
      cat(rep(" ", level), paste0(itemName, ":"), fill = T)
      .printNestedList(item, level + 2)
    } else {
      if (is.null(item))
        item = "null"
      cat(rep(" ", level), paste0(itemName, ":"), item, fill = T)
    }
  }
}
