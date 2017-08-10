#' Loads a yaml config file
#' @param project A project (use default config file names for this project)
#' @param sp Subproject to activate
#' @param file file path to config file, allows you to specify an exact file.
#' @export
loadConfigNew = function(project=NULL, sp=NULL, filename=NULL, usesPathsSection=FALSE) {
	# Derive project folder from environment variables and project name.
	if (is.null(project)) { 
		projectDir = options("PROJECT.DIR")
	} else {
		codepath = Sys.getenv("CODE")
		projectDir = if (is.null(codepath) | identical("", codepath)) project else file.path(codepath, project)
	}

	# Load the project configuration file.
	cfgFile = findConfigFile(projectFolder = projectDir,
		nameConfigFile = filename, projectName = project)
	if (!.isDefined(cfgFile)) {
		message("No config file found.")
		return()
	}
	cfg = yaml::yaml.load_file(cfgFile)
	message("Loaded config file: ", cfgFile)

	# Update based on subproject if one is specified.
	if (!is.null(sp)) {
		if (is.null(cfg$subprojects[[sp]])) {
			message("Subproject not found: ", sp)
			return()
		}
		cfg = modifyList(cfg, cfg$subprojects[[sp]])
		message("Loading subproject: ", sp)
	}
	
	# Show available subprojects.
	if (length(names(cfg$subprojects)) > 1) {
		message("Available subprojects: ", paste0(names(cfg$subprojects), collapse=","))
	}


	# Ensure that metadata (paths) are absolute and return the config.
	cfg$metadata = makeMetadataSectionAbsolute(cfg,
		usesPathsSection=usesPathsSection, parent=dirname(cfgFile))
	return(cfg)
}




#' Alias for backwards compatibility
#'
#' @export
load.config = loadConfig


#' Mapper of organism name to genomic assembly name
#'
#' \code{assemblyByOrganism} uses the data in a project config object to 
#' map organism name to genomic assembly name, supporting both the direct 
#' mapping within a \code{genome} section, or an encoding of this data in 
#' an \code{implied_columns} section.
#'
#' @param config A project configuration object (parsed from a file with 
#'        \code{yaml::yaml.load_file}).
#' @return Mapping (as \code{list}) in which each name is an organism, and 
#'         each list element is the corresponding genomic assembly name.
#' @seealso \url{http://looper.readthedocs.io/en/latest/implied-columns.html}
#' @export
assemblyByOrganism = function(config) {
		# Basic case, in which the project config directly maps organism name 
	# to genomic assembly name
	if (!is.null(config$genome)) { return(config$genome) }
	
	# If neither direct mapping nor column implications, we can't do anything.
	organismImplications = config$implied_columns$organism
	if (is.null(organismImplications)) {
		warning("Project config lacks 'genome' and 'organism' section within 
			'implied_columns' section, so derivation of genome assembly mapping 
			is not possible.")
		return(NULL)
	}
	
	# Build up the organism-to-assembly mapping, skipping each organism 
	# for which such a mapping isn't defined.
	assemblies = list()
	for (organismName in names(organismImplications)) {
		assembly = organismImplications[[organismName]][["genome"]]
		if (is.null(assembly)) {
			warning(sprintf("No 'genome' for '%s'", organismName))
		} else {
			assemblies[[organismName]] = assembly
		}
	}
	
	return(assemblies)
}


expandPath = function(path) {
	# Handle null/empty input.
	if (!.isDefined(path)) { return(path) }

	# Helper functions
	chopPath = function(p) { if (p == dirname(p)) p else c(chopPath(dirname(p)), basename(p)) }
	expand = function(pathPart) { if (startsWith(pathPart, "$")) system(sprintf("echo %s", pathPart), intern = TRUE) else pathPart }

	# Split path; short-circuit return or ensure no reference to this folder.
	parts = chopPath(path)
	if (length(parts) < 2) { return(parts) }
	if (identical(".", parts[1])) { parts = parts[2:length(parts)] }

	# Expand any environment variables and return the complete path.
	fullPath = do.call(file.path, lapply(parts, expand))
	return(fullPath)
}


fileExists = function(fpath) { file_test("-f", fpath) }


findConfigFile = function(projectFolder, nameConfigFile = NULL, 
							projectName = NULL) {

	# First, form the relative filepaths to consider as config file candidates.
	filenames = c("config.yaml", "project_config.yaml", "pconfig.yaml")    # Defaults
	if (!is.null(projectName)) {
		# Project-named config takes last priority.
		filenames = c(filenames, sprintf("%s.yaml", projectName))
	}
	# Explicitly specified config file name takes first priority.
	if (!is.null(nameConfigFile)) { filenames = c(nameConfigFile, filenames) }

	# A project's configuration file is in its metadata folder.
	candidates = sapply( filenames,
		function(filename) { file.path("metadata", filename) } )

	# Within current project directory, find the first configuration
	# file that exists from among a pool of config file names.
	ensureAbsolute = pryr::partial(.makeAbsPath, parent = projectFolder)
	cfgFile = firstExtantFile(files = candidates, modify = ensureAbsolute)
	return(cfgFile)
}


firstExtantFile = function(files, modify = identity) {
	# Find the first extant file from a sequence of candidates.
	#
	# Args:
	#   files: The sequence file names or paths to consider.
	#   parent: Path to the folder to which each element considered 
	#           should be joined if the element isn't absolute path.
	#   modify: Function with which to modify each element before 
	#           checking existence.
	#
	# Returns:
	#   (Absolute) path to the first element that exists. NA if 
	#   no element considered resolves to valid filesystem location.
	modified = sapply(files, modify)
	return(modified[which(sapply(modified, fileExists))[1]])
}


makeMetadataSectionAbsolute = function(config, usesPathsSection, parent) {
	# Enable creation of absolute path using given parent folder path.
	absViaParent = pryr::partial(.makeAbsPath, parent = parent)

	# For earlier project config file layout, handling each metadata
	# item in the same way, deriving absolute path from parent, was valid.
	if (usesPathsSection) { return(lapply(config$metadata, absViaParent)) }

	# With newer project config file layout,
	# certain metadata members are handled differently.
	absoluteMetadata = list()

	# Process each metadata item, handling each value according to attribute name.
	for (metadataAttribute in names(config$metadata)) {
		value = config$metadata[[metadataAttribute]]

		if (metadataAttribute %in% kRelativeSections) {
			if (metadataAttribute == kOldPipelinesSection) {
				warning(sprintf(
					"Config contains old pipeline location specification section: '%s'", 
					kOldPipelinesSection))
			}
			value = expandPath(value)
			if (!.isAbsolute(value)) {
				value = file.path(expandPath(config$metadata[["output_dir"]]), value)
			}
		}
		else { value = absViaParent(value) }    # No special handling

		# Check for and warn about nonexistent path before setting value.
		if (!(fileExists(value) | dir.exists(value))) {
			warning(sprintf("Value for '%s' doesn't exist: '%s'", metadataAttribute, value))
		}
		absoluteMetadata[[metadataAttribute]] = value
	}

	return(absoluteMetadata)
}








# older versions:



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

#' used for my internal project naming scheme. 
#' returns a config file at a default location,
#' given a project name.
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
	


#' is a path absolute?
pathIsAbs = function(path) {
	return(substr(path, 1, 1) == "/")
}




