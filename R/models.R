#' Portable Encapsulated Project object
#'
#' Provides an in-memory representation and functions to access project
#' configuration and sample annotation values for a PEP.
#'
#' @slot file character vector path to config file on disk.
#' @slot samples a data table object holding the sample metadata
#' @slot config a list object holding contents of the config file
#'
#' @exportClass Project
setClass("Project", representation(
		file="character",
		samples="data.frame",
		config="list")
)

#' A class representing a Portable Encapsulated Project
#' This is a helper that creates the project with empty samples and config slots
#' @param file project configuration yaml file
#' @param samples a data table object holding the sample metadata
#' @param config a list object holding contents of the config file
#' @export Project
Project = function(file, samples=list(), config=list()) {
	new("Project", file=file)
}

setMethod("show",
	signature = "Project",
	definition = function(object) {
		message("PEP project object. Class: ", class(object))
		message("  file: ", object@file)
		message("  samples: ", NROW(object@samples))
		listSubprojects(object@config)
		invisible(NULL)
	}
)


setGeneric("config", function(object, ...) standardGeneric("config"))

#' @export
setMethod("config",
	signature = "Project",
	definition = function(object) {
		printNestedList(object@config)
		invisible(object@config)
	}
)

setGeneric("samples", function(object, ...) standardGeneric("samples"))

#' @export
setMethod("samples",
	signature = "Project",
	definition = function(object) {
		print(object@samples)
		invisible(object@samples)
	}
)

setMethod("initialize", "Project", function(.Object, sp=NULL, ...) {
	.Object = callNextMethod()  # calls generic initialize
	.Object@config = loadConfig(.Object@file, sp)
	.Object@samples = .loadSampleAnnotation(.Object@config$metadata$sample_annotation)
	.Object = .deriveColumns(.Object)
	.Object
})

.deriveColumns = function(.Object) {
	# Set default derived columns
	dc = as.list(unique(append(.Object@config$derived_columns, "data_source")))
	.Object@config$derived_columns = dc

	# Convert samples table into list of individual samples for processing
	cfg = .Object@config
	tempSamples = .Object@samples
	numSamples = nrow(tempSamples)
	if (numSamples == 0) {
		return(.Object)
	}

	listOfSamples = split(tempSamples, seq(numSamples))

	# Process derived columns
	for (column in cfg$derived_columns) {
		for (iSamp in seq_along(listOfSamples)) {
			samp = listOfSamples[[iSamp]]
			regex = cfg$data_sources[[ samp[[column]] ]]
			if (! is.null(regex) ) {
				samp[[column]] = strformat(regex, as.list(samp))
			}
			listOfSamples[[iSamp]] = samp
		}
	}

	# Reformat listOfSamples to a table
	.Object@samples = do.call(rbind, listOfSamples)
	.Object
}


.loadSampleAnnotation = function(sampleAnnotationPath) {
	# Can use fread if data.table is installed, otherwise use read.table
	if (requireNamespace("data.table")) {
		sampleReadFunc = data.table::fread
	} else {
		sampleReadFunc = read.table
	}

	if (.safeFileExists(sampleAnnotationPath)) {
		samples = sampleReadFunc(sampleAnnotationPath)
	} else{
		message("No sample annotation file:", sampleAnnotationPath)
		samples = data.frame()
	}
	return(samples)
}

#' @export
activateSubproject = function(.Object, sp, ...) {

	.Object@config = .updateSubconfig(.Object@config, sp)

	# Ensure that metadata paths are absolute and return the config.
	# This used to be all metadata columns; now it's just: results_subdir
	mdn = names(.Object@config$metadata)

	.Object@config$metadata = makeMetadataSectionAbsolute(.Object@config, parent=dirname(.Object@file))

	.Object@samples = .loadSampleAnnotation(.Object@config$metadata$sample_annotation)
	.Object = .deriveColumns(.Object)
	.Object
}

#' Prints a nested list in a way that looks nice
#' 
#' @param lst list object to print
#' @export
printNestedList = function(lst, level=0) {
	for(itemName in names(lst)) {
		item = lst[[itemName]]
		if (class(item) == "list") {
			message(rep(" ", level), itemName, ":")
			printNestedList(item, level+2)
		} else {
			if (is.null(item)) item = "null"
			message(rep(" ", level), itemName, ": ", item)
		}
	}
}
