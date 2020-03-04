#' Load the config of a PEP
#'
#' Loads a PEP config file
#'
#' @param amendments amendments to activate
#' @param filename file path to config file
#' 
#' @seealso \url{https://pepkit.github.io/}
.loadConfig = function(filename=NULL, amendments=NULL) {
  if (!file.exists(filename)) {
    stop("No config file found")
  }
  # Initialize config object
  cfg = methods::new("Config", filename)
  message("Loaded config file: ", filename)
  # Show available subprojects
  .listAmendments(cfg)
  # Update based on amendments if any specified
  cfg = .activateAmendments(cfg, amendments)
  # make bioconductor$readFunPath value absolute, used in BiocProject
  if(!is.null(cfg$bioconductor$readFunPath)){
    path = gsub("\\./","",cfg$bioconductor$readFunPath)
    cfg$bioconductor$readFunPath = .makeAbsPath(path, parent=dirname(filename))
  }
  cfg$name = .inferProjectName(cfg, filename)
  return(cfg)
}

.inferProjectName = function(cfg, filename){
  if (!is.null(cfg$name)) return(cfg$name)
  return(basename(dirname(normalizePath(filename))))
}

.activateAmendments = function(cfg, amendments=NULL) {
  if (!is.null(amendments)) {
    for (amendment in amendments){
      if (is.null(cfg$amendments[[amendment]])) {
        warning("Amendment not found: ", amendment)
        message("Amendment was not activated")
        return(cfg)
      }
      cfg = utils::modifyList(cfg, cfg$amendments[[amendment]])
      message("Activating amendment: ", amendment)
    }
  }
  return(cfg)
}

.listAmendments = function(cfg, style="message") {
  # this function can be used in object show method, where cat is preferred 
  # or for user information when the Project is created, where message
  # is preferred
  if(!style == "message"){
    printFun = pryr::partial(cat, fill=T)
  }else{
    printFun = message
  }
  # make sure the extracted config is of proper class
  if(!methods::is(cfg,"Config")) 
    stop("The Project object does not contain a vaild config")
  
  if (length(names(cfg$amendments)) > 0) {
    # If there are any show a cat and return if needed
    printFun("  amendments: ", paste0(names(cfg$amendments), collapse=","))
    invisible(names(cfg$amendments))
  } else{
    # Otherwise return NULL for testing purposes
    NULL
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
#' .expandPath(string)
#' path = "$HOME/my/path/string.txt"
#' .expandPath(path)
#' @export
.expandPath = function(path) {
  # helper function
  removeNonWords = function(str) {
    # can be used to get rid of the non-word chars in the env vars strings
    strsplit(gsub("[^[:alnum:] ]", "", str), " +")[[1]]
  }
  
  # helper function
  replaceEnvVars = function(path, matches){
    # the core of the expandPath function
    parts = unlist(regmatches(x=path, matches, invert=F))
    replacements = c()
    for (i in seq_along(attr(matches[[1]], "match.length"))) {
      # get the values of the env vars
      replacements[i] = Sys.getenv(removeNonWords(parts[i]))
      if(any(replacements == "")){
        missingEnvVar = which(replacements == "")
        warning(
          paste0("The environment variable '",parts[missingEnvVar],
                 "' was not found. Created object might be invalid.")
          )
      }
    }
    # replace env vars with their system values
    regmatches(x=path, matches, invert=F) = replacements
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
  path = normalizePath(path.expand(path),mustWork = FALSE)
  # search for env vars, both bracketed and not 
  matchesBracket = gregexpr("\\$\\{\\w+\\}", path, perl=T)
  matches = gregexpr("\\$\\w+", path, perl=T)
  
  # perform two rounds of env var replacement
  # this way both bracketed and not bracketed ones will be replaced
  if(all(attr(matchesBracket[[1]], "match.length") != -1)) path = replaceEnvVars(path, matchesBracket)
  if(all(attr(matches[[1]], "match.length") != -1)) path = replaceEnvVars(path, matches)
  return(path)
}

#' Format a string like python's format method
#'
#' Given a string with environment variables (encoded like \code{${VAR}} or \code{$VAR}), and
#' other variables (encoded like \code{{VAR}}) this function will substitute
#' both of these and return the formatted string, like the Python
#' \code{str.format()} method. Other variables are populated from a list of arguments.
#' Additionally, if the string is a non-absolute path, it will be expanded.

#' @param string String with variables encoded
#' @param args named list of arguments to use to populate the string
#' @param exclude character vector of args that should be excluded from 
#' the interpolation. The elements in the vector should match the names of the
#' elements in the \code{args} list
#' @param parent a directory that will be used to make the path absolute
#' @export
#' @examples
#' .strformat("~/{VAR1}{VAR2}_file", list(VAR1="hi", VAR2="hello"))
#' .strformat("$HOME/{VAR1}{VAR2}_file", list(VAR1="hi", VAR2="hello"))
.strformat = function(string, args, exclude=NULL, parent=NULL) {
  result = c()
  # if parent provided, make the path absolute and expand it.
  #  Otherwise, just expand it
  x = ifelse(is.null(parent),.expandPath(string),.makeAbsPath(string, parent))
  # str_interp requires variables encoded like ${var}, so we substitute
  # the {var} syntax here.
  x = stringr::str_replace_all(x, "\\{", "${")
  argsUnlisted = lapply(args, unlist)
  argsLengths = lapply(argsUnlisted, length)
  if (any(argsLengths > 1)) {
    pluralID = which(argsLengths > 1)
     attrCount = sapply(argsUnlisted, length)[pluralID]
     nrows = unique(attrCount)
     if(length(nrows) > 1) {
      stop("If including multiple attributes with multiple values, the number of values in each attribute must be identical.")
     }
     for (r in seq_len(nrows)) {
        argsUnlistedCopy = argsUnlisted
        for (i in seq_along(pluralID)) {
          argsUnlistedCopy[[pluralID[[i]]]] = argsUnlisted[[pluralID[[i]]]][r]
        }
        result = append(result, stringr::str_interp(x, argsUnlistedCopy))
      }
    return(result)
  } else {
    return(stringr::str_interp(x, argsUnlisted))
  }
}
