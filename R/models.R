#' Portable Encapsulated Project object
#'
#' Provides an in-memory representation and functions to access project
#' configuration and sample annotation values for a PEP.
#'
#' @slot file A logical keeping track of something.
#' @slot samples An integer specifying something else.
#' @slot config A data.frame holding some data.
#'
#' @exportClass Project
setClass("Project", representation(
		file = "character",
		samples="list",
		config="list"))


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
	invisible(NULL)}
	)

setGeneric("config", function(object, ...) standardGeneric("config"))

#' @export
setMethod("config",
	signature = "Project",
	definition = function(object) {
		printNestedList(object@config)
		invisible(object@config)
	})

setGeneric("samples", function(object, ...) standardGeneric("samples"))

#' @export
setMethod("samples",
	signature = "Project",
	definition = function(object) {
		print(object@samples)
		invisible(object@samples)
	})

setMethod("initialize", "Project", function(.Object, ...) {
	.Object = callNextMethod()  # calls generic initialize

	.Object@config = loadConfig(.Object@file)

	# Can use fread if data.table is installed, otherwise use read.table
	if (requireNamespace("data.table")) {
		sampleReadFunc = data.table::fread
	} else {
		sampleReadFunc = read.table
	}
	.Object@samples = sampleReadFunc(.Object@config$metadata$sample_annotation)

	# Set default derived columns
	dc = as.list(unique(append(.Object@config$derived_columns, "data_source")))
	.Object@config$derived_columns = dc

	# Convert samples table into list of individual samples for processing
	cfg = .Object@config
	tempSamples = .Object@samples
	listOfSamples = split(tempSamples, seq(nrow(tempSamples)))

	# Process derived columns
	for (column in cfg$derived_columns) {
		for (iSamp in seq_along(listOfSamples)) {
			samp = listOfSamples[[iSamp]]
			regex = cfg$data_sources[[ samp[[column]] ]]
			samp[[column]] = fmt(regex, as.list(samp))
			listOfSamples[[iSamp]] = samp
		}
	}

	# Reformat listOfSamples to a table
	.Object@samples = do.call(rbind, listOfSamples)

	.Object
	})


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
