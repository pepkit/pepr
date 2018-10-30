#' Load the config of a PEP
#'
#' Loads a \code{project_config.yaml} file
#'
#' @param sp Subproject to activate
#' @param filename file path to config file
#' @export
loadConfig = function(filename = NULL, sp = NULL) {
  if (!file.exists(filename)) {
    stop("No config file found")
  }
  config_file = yaml::yaml.load_file(filename)
  if (!is.list(config_file))
    stop("The config file has to be a YAML formatted file.
         See: http://yaml.org/start.html")
  cfg = new("Config", config_file)
  
  if (is.null(cfg)) {
    cat("Config file not loaded.", fill = T)
    return()
  }
  
  cat("Loaded config file: ", filename, fill = T)
  
  # Show available subprojects
  listSubprojects(cfg)
  
  # Update based on subproject if one is specified.
  cfg = .updateSubconfig(cfg, sp)
  
  # Ensure that metadata paths are absolute and return the config.
  # This used to be all metadata columns; now it's just: results_subdir
  mdn = names(cfg$metadata)
  
  cfg$metadata = makeMetadataSectionAbsolute(cfg, parent = dirname(filename))
  
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

.updateSubconfig = function(cfg, sp = NULL) {
  if (!is.null(sp)) {
    if (is.null(cfg$subprojects[[sp]])) {
      cat("Subproject not found: ", sp, fill = T)
      return()
    }
    cfg = utils::modifyList(cfg, cfg$subprojects[[sp]])
    cat("Loading subproject: ", sp, fill = T)
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
  if (length(names(cfg$subprojects)) > 0) {
    # If there are any show a cat and return if needed
    cat("  subprojects: ", paste0(names(cfg$subprojects), 
                                  collapse = ","), fill = T)
    invisible(names(cfg$subprojects))
  } else{
    # Otherwise return NULL for testing purposes
    invisible(NULL)
  }
}


#' Expand system path
#'
#' This function expands system paths (the non-absolute paths become absolute) 
#' and replaces the environment variables (e.g, \code{${HOME}}) 
#' with their values.
#'
#' Most importantly strings that are not system paths are returned untouched
#'
#' @param path file path to expand. Potentially any string
#' @return Expanded path or untouched string
#'
#' @examples
#'
#' string = "https://www.r-project.org/"
#' expandPath(string)
#' path = "$HOME/my/path/string.txt"
#' expandPath(path)
#' @export

expandPath = function(path) {
  # helper function
  removeNonWords = function(str) {
    # can be used to get rid of the non-word chars in the env vars strings
    strsplit(gsub("[^[:alnum:] ]", "", str), " +")[[1]]
  }
  
  # helper function
  replaceEnvVars = function(path, matches){
    # the core of the expandPath function
    parts = unlist(regmatches(x = path, matches, invert = F))
    replacements = c()
    for (i in seq_along(attr(matches[[1]], "match.length"))) {
      # get the values of the env vars
      replacements[i] = Sys.getenv(removeNonWords(parts[i]))
    }
    # replace env vars with their system values
    regmatches(x = path, matches, invert = F) = replacements
    # if UNIX, make sure the root's in the path
    if (.Platform$OS.type == "unix") {
      if (!startsWith(path, "/")) {
        path = paste0("/", path)
      }
      # prevent double slashes
      path = gsub("//", "/", path)
    }
  }
  
  # handle null/empty input.
  if (!.isDefined(path)) {
    return(path)
  }
  
  # if it's a path, make it absolute
  path = path.expand(path)
  # search for env vars, both bracketed and not 
  matchesBracket = gregexpr("\\$\\{\\w+\\}", path, perl = T)
  matches = gregexpr("\\$\\w+", path, perl = T)
  
  # perform two rounds of env var replacement
  # this way both bracketed and not bracketed ones will be replaced
  if(all(attr(matchesBracket[[1]], "match.length") != -1)) path = replaceEnvVars(path, matchesBracket)
  if(all(attr(matches[[1]], "match.length") != -1)) path = replaceEnvVars(path, matches)
  
  return(path)
}

#' Format a string like python's format function
#'
#' Given a string with environment variables (encoded like \code{${VAR}}), and
#' other variables (encoded like \code{{VAR}}) this function will substitute
#' both of these and return the formatted string, like the python string
#' \code{format()}. Other variables are populated from a list of arguments.
#' Additionally, if the string is a non-absolute path, it will be expanded.

#' @param string String with variables encoded
#' @param args named list of arguments to use to populate the string
#' @param exclude character vector of args that should be excluded from 
#' the interpolation. The elements in the vector should match the names of the
#' elements in the args list
#' @export
#' @examples
#' strformat("~/{VAR1}{VAR2}_file", list(VAR1="hi", VAR2="hello"))
strformat = function(string, args, exclude) {
  result = c()
  x = expandPath(string)
  # str_interp requires variables encoded like ${var}, so we substitute
  # the {var} syntax here.
  x = stringr::str_replace_all(x, "\\{", "${")
  argsUnlisted = lapply(args, unlist)
  argsLengths = lapply(argsUnlisted, length)
  if (any(argsLengths > 1)) {
    pluralID = which(argsLengths > 1)
    # Remove the previously interpolated, 
    # thus plural elements from another round of interpolation
    if (any(names(pluralID) %in% exclude)) {
      pluralID = pluralID[-which(names(pluralID) %in% exclude)]
    }
    for (iPlural in unlist(argsUnlisted[pluralID])) {
      argsUnlisted[[pluralID]] = iPlural
      result = append(result, stringr::str_interp(x, argsUnlisted))
    }
    return(result)
  } else{
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
        warning(
          sprintf(
            "Config contains old pipeline location specification section: '%s'",
            kOldPipelinesSection
          )
        )
      }
      value = expandPath(value)
      if (!.isAbsolute(value)) {
        value = file.path(expandPath(config$metadata[["output_dir"]]), value)
      }
    }
    else {
      value = absViaParent(value)
    }    # No special handling
    
    # Check for and warn about nonexistent path before setting value.
    if (!(!.isDefined(value) ||
          file.exists(value) || dir.exists(value))) {
      warning(sprintf(
        "Value for '%s' doesn't exist: '%s'",
        metadataAttribute,
        value
      ))
    }
    absoluteMetadata[[metadataAttribute]] = value
  }
  
  return(absoluteMetadata)
}
