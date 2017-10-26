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
	if (requireNamespace("data.table")) {
		sampleReadFunc = data.table::fread
	} else {
		sampleReadFunc = read.table
	}
	.Object@samples = sampleReadFunc(.Object@config$metadata$sample_annotation)

	# Set default derived columns
	.Object@config$derived_columns = as.list(unique(append(.Object@config$derived_columns, "data_source")))
	cfg = .Object@config

	# Now we should process derived columns
	s = .Object@samples
	l <- split(s, seq(nrow(s)))
	for (column in cfg$derived_columns) {
		for (iSamp in seq_along(l)) {
			samp = l[[iSamp]]
			regex = cfg$data_sources[[ samp[[column]] ]]
			samp[[column]] = fmt(regex, as.list(samp))
			l[[iSamp]] = samp
		}
	}
	s2 = do.call(rbind,l)
	.Object@samples = s2

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
