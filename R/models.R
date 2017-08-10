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
	})

setGeneric("samples", function(object, ...) standardGeneric("samples"))

#' @export
setMethod("samples",
	signature = "Project",
	definition = function(object) {
		return(object@samples)
	})

setMethod("initialize", "Project", function(.Object, ...) {
	.Object = callNextMethod()  # calls generic initialize
	.Object@config = loadConfig(.Object@file)
	if (requireNamespace("data.table")) {
		sampleReadFunc = data.table::fread
	} else {
		sampleReadFunc = read.table
	}
	.Object@samples = sampleReadFunc(.Object@config$metadata$sample_annotation)
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
