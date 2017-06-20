# required libraries (to add to deps in package):
library(data.table)


#' used for my internal project naming scheme. 
#' returns a config file at a default location,
#' given a project name.
#' TODO: move to project.init
getConfigFile = function(project) {
	if (is.null(project)) { 
		projectDir = options("PROJECT.DIR")
	} else {
		projectDir = paste0(Sys.getenv("CODEBASE"), project)
	}
	# If no file is specified, try these default locations
	yamls = list("metadata/config.yaml",
						"metadata/project_config.yaml",
						paste0("metadata/", project, ".yaml"))
	cfg = NULL
	if (! is.null(filename)) {
		yamls = c(filename, yamls)
	}

	for (yfile in yamls) {
		if ( ! pathIsAbs(yfile) ) {
			cfgFile = file.path(projectDir, yfile)
		} else {
			cfgFile = yfile
		}
		if (file.exists(cfgFile)) {
			break
		}
	}
	return(cfgFile)
}
	

#' Loads a project_config.yaml file
#'
#' @param project A project (use default config file names for this project)
#' @param sp Subproject to activate
#' @param file file path to config file, allows you to specify an exact file.
#' @export
loadConfig = function(filename=NULL, project=NULL, sp=NULL) {
	if ( ! requireNamespace("yaml", quietly=TRUE)) {
		warning("Package yaml is required to load yaml config files.")
		return
	}

	cfg = yaml::yaml.load_file(filename)

	if (is.null(cfg)) {
		message("No config file found.")
		return
	}
	message("Loaded config file: ", filename)
	
	if (! is.null(sp)) {
		# Update with subproject variables
		spc = cfg$subprojects[[sp]]
		if (is.null(spc)) {
			message("Subproject not found: ", sp)
			return
		}
		cfg = modifyList(cfg, cfg$subprojects[[sp]])
		message("Loading subproject: ", sp)
	}
	# Show available subprojects
	sps = names(cfg$subprojects)
	if (length(sps) > 1) { 
		message("Available subprojects: ", paste0(sps, collapse=","))
	}

	# Make metadata absolute
	# This used to be all metadata columns; now it's just: results_subdir
	mdn = names(cfg$metadata)
	for (n in mdn) {
		if ( !pathIsAbs(cfg$metadata[n]) ) { 
			cfg$metadata[n] = file.path(dirname(filename), cfg$metadata[n])
		}
	}

	return(cfg)
}

#' is a path absolute?
pathIsAbs = function(path) {
	return(substr(path, 1, 1) == "/")
}


Project = setClass("Project",
	slots = c(
		file = "character",
		samples="list",
		config="list"))

setMethod("show",
	signature = "Project",
	definition = function(object) {
	message("An object of class ", class(object))
	message("  file: ", object@file)
	message("  samples: ", NROW(object@samples))
	invisible(NULL)}
	)

setGeneric("config", function(object, ...) standardGeneric("config"))

setMethod("config",
	signature = "Project",
	definition = function(object) {
		printNestedList(object@config)
	})

setMethod("initialize", "Project", function(.Object, ...) {
	.Object = callNextMethod()  # calls generic initialize
	.Object@config = loadConfig(.Object@file)
	.Object@samples = fread(.Object@config$metadata$sample_annotation)
	.Object
	})


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
