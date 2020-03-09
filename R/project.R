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


setMethod("initialize", "Project", function(.Object, ...) {
    .Object = methods::callNextMethod(.Object)  # calls generic initialize
    ellipsis <- list(...)
    if (!is.null(ellipsis$file)) {
        # check if file path provided
        .Object@file = .makeAbsPath(ellipsis$file, parent = getwd())
        # instantiate config object and stick it in the config slot
        .Object@config = Config(ellipsis$file, ellipsis$amendments)
        .Object = .loadSampleAnnotation(.Object)
        .Object = .modifySamples(.Object)
    }
    return(.Object)
})


#' The constructor of a class representing a Portable Encapsulated Project
#'
#' This is a helper that creates the project with empty samples and config slots
#'
#' @param file a character with project configuration yaml file
#' @param amendments a character with the amendments names to be activated
#' @examples
#' projectConfig = system.file("extdata", "example_peps-cfg2",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' @export Project
Project = function(file = NULL, amendments = NULL) {
    methods::new("Project", file = file, amendments = amendments)
}


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
        object = .appendAttrs(object)
        object = .duplicateAttrs(object)
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
#' "example_peps-cfg2",
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
#' "example_peps-cfg2",
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


#' Lists amendments in a \code{\link{Project-class}} object
#'
#' Lists available amendments within a \code{\link{Project-class}} object.
#'
#' The amendments can be activated by passing their names to the  \code{\link{activateAmendments}} method
#'
#' @param .Object an object of \code{\link{Project-class}}
#' @return names of the available amendments
#' @examples 
#' projectConfig = system.file("extdata",
#' "example_peps-cfg2",
#' "example_amendments1",
#' "project_config.yaml",
#' package = "pepr")
#' p = Project(file = projectConfig)
#' availAmendemtns = listAmendments(p)
#' activateAmendments(p,availAmendemtns[1])
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


#' Activate other amendments in objects of \code{\link{Project-class}}
#'
#' This method switches between the amendments
#' within the \code{\link{Project-class}} object
#'
#' To check what are the amendments names
#' call \code{listAmendments(p)}, where \code{p} is the object
#' of \code{\link{Project-class}} class
#'
#' @param .Object an object of class \code{\link{Project-class}}
#' @param amendments character with the amendment name
#' 
#' @examples 
#' projectConfig = system.file("extdata",
#' "example_peps-cfg2",
#' "example_amendments1",
#' "project_config.yaml",
#' package = "pepr")
#' p = Project(file = projectConfig)
#' availAmendments = listAmendments(p)
#' activateAmendments(p,availAmendments[1])
#' @export
setGeneric("activateAmendments", function(.Object, amendments)
    standardGeneric("activateAmendments"))

setMethod(
    f = "activateAmendments",
    signature = signature(.Object="Project", amendments="character"),
    definition = function(.Object, amendments) {
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


#' View samples in the objects of \code{\link{Project-class}} 
#'
#' This method can be used to view the samples slot
#' of the \code{\link{Project-class}} class
#'
#' @param object an object of \code{\link{Project-class}} 
#'
#' @return a data.table with the with metadata about samples
#' @examples
#' projectConfig = system.file("extdata", "example_peps-cfg2",
#' "example_amendments1", "project_config.yaml", package="pepr")
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

# sample modifiers --------------------------------------------------------



#' Append constant attributes across all the samples
#'
#' @param .Object an object of \code{\link{Project-class}} 
#'
#' @return an object of \code{\link{Project-class}} 
.appendAttrs <- function(.Object) {
    if (!CFG_MODIFIERS_KEY %in% names(config(.Object))) return(.Object)
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

#' Duplicate a selected attribute across all the samples
#'
#' @param .Object an object of \code{\link{Project-class}} 
#'
#' @return an object of \code{\link{Project-class}} 
.duplicateAttrs <- function(.Object) {
    if (!CFG_MODIFIERS_KEY %in% names(config(.Object))) return(.Object)
    modifiers = config(.Object)[[CFG_MODIFIERS_KEY]]
    if (!CFG_DUPLICATE_KEY %in% names(modifiers)) return(.Object)
    duplicated = modifiers[[CFG_DUPLICATE_KEY]]
    for(oriAttrName in names(duplicated)){
        .Object@samples[,duplicated[[oriAttrName]]] = .Object@samples[,oriAttrName]
    }
    return(.Object)
}

#' Imply attributes
#'
#' @param .Object an object of \code{\link{Project-class}} 
#'
#' @return an object of \code{\link{Project-class}} 
.implyAttrs = function(.Object) {
    if (!CFG_MODIFIERS_KEY %in% names(config(.Object))) return(.Object)
    modifiers = config(.Object)[[CFG_MODIFIERS_KEY]]
    if (!CFG_IMPLY_KEY %in% names(modifiers)) return(.Object)
    implications = modifiers[[CFG_IMPLY_KEY]]
    for (implication in implications) {
        if (!(CFG_IMPLY_IF_KEY %in% names(implication) && CFG_IMPLY_THEN_KEY %in% names(implication)))
            stop(CFG_IMPLY_KEY, " section is not formatted properly")
        implierAttrs = names(implication[[CFG_IMPLY_IF_KEY]])
        implierVals = implication[[CFG_IMPLY_IF_KEY]]
        impliedAttrs = names(implication[[CFG_IMPLY_THEN_KEY]])
        impliedVals = as.character(implication[[CFG_IMPLY_THEN_KEY]])
        attrs = colnames(.Object@samples)
        if (!all(implierAttrs %in% attrs)) next
        allHitIds = list()
        for (i in seq_along(implierAttrs)) {
            hitIds = list()
            implierStrings = as.character(unlist(implierVals[i]))
            for(j in seq_along(implierStrings)){
                hitIds[[j]] = which(.Object@samples[,implierAttrs[i]] == implierStrings[j])    
            }
            allHitIds[[i]] = Reduce(union, hitIds)
            if (length(allHitIds[[i]]) < 1) break
        }
        qualIds = Reduce(intersect, allHitIds)
        if (length(qualIds) < 1) next
        for (i in seq_along(impliedAttrs)){
            if (!impliedAttrs[i] %in% attrs) 
                .Object@samples[,impliedAttrs[i]] = ""
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
.deriveAttrs = function(.Object) {
    if (!CFG_MODIFIERS_KEY %in% names(config(.Object))) return(.Object)
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