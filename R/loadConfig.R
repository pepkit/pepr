#' Loads a project_config.yaml file
#'
#' @param sp Subproject to activate
#' @param filename file path to config file
#' @export
loadConfig = function(filename=NULL, sp=NULL) {
  
  if (!file.exists(filename)) {
    message("No config file found: ", filename)
    return()
  }
  
  cfg = new("Config", yaml::yaml.load_file(filename))
  
  if (is.null(cfg)) {
    message("Config file not loaded.")
    return()
  }
  
  message("Loaded config file: ", filename)
  
  # Show available subprojects
  listSubprojects(cfg)
  
  # Update based on subproject if one is specified.
  cfg = .updateSubconfig(cfg, sp)
  
  # Ensure that metadata paths are absolute and return the config.
  # This used to be all metadata columns; now it's just: results_subdir
  mdn = names(cfg$metadata)
  
  cfg$metadata = makeMetadataSectionAbsolute(cfg, parent=dirname(filename))
  
  # Infer default project name
  
  if (is.null(cfg$name)) {
    # Default project name is the name of the folder containing the config file
    maybeProjectName = basename(dirname(normalizePath(filename)))
    if (maybeProjectName == "metadata") {
      # Unless it's in a 'metadata' folder, then it's the name of the folder
      # one level up
      maybeProjectName = basename(dirname(dirname(normalizePath(filename))))
    }
    cfg$name = maybeProjectName
  }
  
  return(cfg)
}

.updateSubconfig = function(cfg, sp=NULL) {
  if (! is.null(sp)) {
    if (is.null(cfg$subprojects[[sp]])) {
      message("Subproject not found: ", sp)
      return()
    }
    cfg = modifyList(cfg, cfg$subprojects[[sp]])
    message("Loading subproject: ", sp)
  }
  return(cfg)
}


#' Lists subprojects in a config file 
#' 
#' Lists subprojects in an R list representation of a yaml config file read by
#' pepr
#'
#' @param cfg Configuration section of a project
#' @export
listSubprojects = function(cfg) {
  # Show available subprojects
  if (length(names(cfg$subprojects)) > 1) {
    message("  subprojects: ", paste0(names(cfg$subprojects), collapse=","))
  }
  invisible(names(cfg$subprojects))
}


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

#' Implementation of python's expandpath
#' @param path file path to expand
#' @export
expandPath = function(path) {
  # Handle null/empty input.
  if (!.isDefined(path)) { return(path) }
  
  # Helper functions
  chopPath = function(p) { 
    if (p == dirname(p)) p else c(chopPath(dirname(p)), basename(p)) 
  }
  expand = function(pathPart) { 
    if (startsWith(pathPart, "$")) {
      return(system(sprintf("echo %s", pathPart), intern = TRUE))
    } else {
      return(pathPart)
    }
  }
  
  # Split path; short-circuit return or ensure no reference to this folder.
  parts = chopPath(path)
  if (length(parts) < 2) { return(parts) }
  if (identical(".", parts[1])) { parts = parts[2:length(parts)] }
  
  # Expand any environment variables and return the complete path.
  fullPath = do.call(file.path, lapply(parts, expand))
  return(fullPath)
}

#' Format a string like python's format function
#' 
#' Given a string with environment variables (encoded like ${VAR}) and other
#' variables (encoded like {VAR}), this function will substitute both of these
#' and return the formatted string, like the python string format(). Other
#' variables are populated from a list of arguments

#' @param string String with variables encoded
#' @param args named list of arguments to use to populate the string
#' @export
#' @examples
#' strformat("{VAR1}{VAR2}_file", list(VAR1="hi", VAR2="hello"))

#' Format a string like python's format function
#' 
#' Given a string with environment variables (encoded like ${VAR}) and other
#' variables (encoded like {VAR}), this function will substitute both of these
#' and return the formatted string, like the python string format(). Other
#' variables are populated from a list of arguments

#' @param string String with variables encoded
#' @param args named list of arguments to use to populate the string
#' @param exclude character vector of args that should be excluded from the interpolation. The elements in the vector should match the names of the elements in the args list
#' @export
#' @examples
#' strformat("{VAR1}{VAR2}_file", list(VAR1="hi", VAR2="hello"))
strformat = function(string, args, exclude) {
  result=c()
  x = pepr:::expandPath(string)
  # str_interp requires variables encoded like ${var}, so we substitute
  # the {var} syntax here.
  x = stringr::str_replace_all(x, "\\{", "${")
  argsUnlisted=lapply(args, unlist)
  argsLengths=lapply(argsUnlisted, length)
  if(any(argsLengths>1)){
    pluralID=which(argsLengths>1)
    # Remove the previously interpolated, thus plural elements from another round of interpolation
    if(any(names(pluralID) %in% exclude)){
      pluralID=pluralID[-which(names(pluralID) %in% exclude)]
    }
    for(iPlural in unlist(argsUnlisted[pluralID])){
      argsUnlisted[[pluralID]] = iPlural
      result=append(result,stringr::str_interp(x, argsUnlisted))
    }
    return(result)
  }else{
    return(stringr::str_interp(x, argsUnlisted))
  }
}

makeMetadataSectionAbsolute = function(config, parent) {
  # Enable creation of absolute path using given parent folder path.
  absViaParent = pryr::partial(.makeAbsPath, parent = parent)
  
  # With newer project config file layout,
  # certain metadata members are handled differently.
  absoluteMetadata = list()
  
  # Process each metadata item, handling each value according to attribute name.
  for (metadataAttribute in names(config$metadata)) {
    value = config$metadata[[metadataAttribute]]
    
    if (metadataAttribute %in% kRelativeToOutputDirMetadataSections) {
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
    if (!(!.isDefined(value) || file.exists(value) || dir.exists(value))) {
      warning(sprintf("Value for '%s' doesn't exist: '%s'", metadataAttribute, value))
    }
    absoluteMetadata[[metadataAttribute]] = value
  }
  
  return(absoluteMetadata)
}

