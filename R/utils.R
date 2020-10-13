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
      undefinedID = which(replacements == "")
      if(length(undefinedID) > 0){
        warning(
          paste0("The environment variable '", parts[undefinedID],
                 "' was not found. Created object might be invalid.")
        )
        replacements[undefinedID] = parts[undefinedID]
      }
    }
    # replace env vars with their system values
    regmatches(x=path, matches, invert=F) = replacements
    # if UNIX, make sure the root's in the path
    if (.Platform$OS.type == "unix") {
      if (!startsWith(path, "/") && length(undefinedID) == 0) {
        path = paste0("/", path)
      }
    }
    # prevent double slashes
    path = gsub("//", "/", path)
  }
  
  # handle null/empty input.
  if (!.isDefined(path) || !is.character(path)) {
    return(path)
  }
  
  # if it's a path, make it absolute
  path = path.expand(path)
  # search for env vars, both bracketed and not 
  matchesBracket = gregexpr("\\$\\{\\w+\\}", path, perl=T)
  matches = gregexpr("\\$\\w+", path, perl=T)
  
  # perform two rounds of env var replacement
  # this way both bracketed and not bracketed ones will be replaced
  if(all(attr(matchesBracket[[1]], "match.length") != -1)) 
    path = replaceEnvVars(path, matchesBracket)
  if(all(attr(matches[[1]], "match.length") != -1)) 
    path = replaceEnvVars(path, matches)
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
#' @param parent a directory that will be used to make the path absolute
#' @export
#' @examples
#' .strformat("~/{VAR1}{VAR2}_file", list(VAR1="hi", VAR2="hello"))
#' .strformat("$HOME/{VAR1}{VAR2}_file", list(VAR1="hi", VAR2="hello"))
.strformat = function(string, args, parent=NULL) {
  result = c()
  # if parent provided, make the path absolute and expand it.
  #  Otherwise, just expand it
  x = .expandPath(string)
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


#' Create an absolute path from a primary target and a parent candidate.
#
#' @param perhapsRelative Path to primary target directory.
#' @param parent a path to parent folder to use if target isn't absolute.
#' 
#' @export 
#' @return Target itself if already absolute, else target nested within parent.
.makeAbsPath = function(perhapsRelative, parent) {
  res = c()
  for(pR in perhapsRelative){
    if (!.isDefined(pR)) return(pR)
    pR = .expandPath(pR)
    if (.isAbsolute(pR)) {
      abspath = pR
    } else {
      abspath = file.path(path.expand(parent), pR)
    }
    if (!.isAbsolute(abspath)) 
      stop("Relative path ", pR, " and parent ", parent ,
           " failed to create absolute path: ", abspath)
    res = append(res, abspath)
  }
  return(res)
}

# Must test for is.null first, since is.na(NULL) returns a logical(0) which is
# not a boolean
.isDefined = function(var) {
  !(is.null(var) || is.na(var))
}

#' Determine whether a path is absolute.
#'
#' @param path The path to check for seeming absolute-ness.
#' @return Flag indicating whether the \code{path} appears to be absolute.
.isAbsolute = function(path) {
  if (!is.character(path)) stop("The path must be character")
  return(grepl("^(/|[A-Za-z]:|\\\\|~)", path))
}

.safeFileExists = function(path) {
  ((!is.null(path)) && file.exists(path))
}

#' Listify data frame columns
#'
#' This function turns each data frame column into a list,
#' so that its cells can contain multiple elements
#' 
#' @param DF an object of class data.frame
#' @return an object of class data.frame
#'
#' @examples
#' dataFrame=mtcars
#' listifiedDataFrame=.listifyDF(dataFrame)
#' @export
.listifyDF = function(DF) {
  if(!is.data.frame(DF)) stop("The input object must be a data.frame.")
  colNames =  names(DF)
  for(iColumn in colNames){
    DF[[iColumn]] = as.list(DF[[iColumn]])
  }
  return(DF)
}


#' Collect samples fulfilling the specified requirements
#' 
#' This funciton collects the samples from a \code{\link[data.table]{data.table-class}} object that
#' fulfill the requirements of an attribute \code{attr} specified with 
#' the \code{fun} argument
#' 
#' The anonymous function provided in the \code{func} argument has to return an integer that indicate the rows that the \code{action} should be performed on.
#' Core expressions which are most useful to implement the anonymous function are:
#' \itemize{
#' \item \code{\link[base]{which}} with inequality signs: \code{==,>,<}
#' \item \code{\link[base]{grep}}
#' }
#' 
#' @param samples an object of \code{\link[data.table]{data.table-class}} class
#' @param attr a string specifying a column in the \code{samples}
#' @param func an anonymous function, see Details for more information
#' @param action a string (either \code{include} or \code{exclude}) that specifies whether the function should select the row or exclude it.
#' @importFrom methods is
#' @examples 
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' p = Project(projectConfig)
#' s = sampleTable(p)
#' fetchSamples(s,attr = "sample_name", func=function(x){ which(x=="pig_0h") },action="include")
#' fetchSamples(s,attr = "sample_name", func=function(x){ which(x=="pig_0h") },action="exclude")
#' fetchSamples(s,attr = "sample_name", func=function(x){ grep("pig_",x) },action="include")
#' @export
fetchSamples = function(samples, attr=NULL, func=NULL, action="include") {
  if(!methods::is(samples, "data.table"))
    stop("'samples' argument has to be a data.table object, got: '",
         class(samples),"'")
  if(!action %in% c("include","exclude"))
    stop("'action' argument has to be either 'include' or 'exclude', got '",
         action,"'")
  attrNames = colnames(samples)
  if(!is.null(attr)){
    if (!attr %in% attrNames) 
      stop("The samples attribute '", attr,"' was not found.")
    if (!is.null(func)) {
      # use the anonymous function if provided
      if (is.function(func)) {
        rowIdx = tryCatch(
          expr = { 
            do.call(func,list(x=samples[[attr]]))
          }, error=function(e) {
            message("Error in your function: ")
            message(e)
          }, warning=function(w) {
            message("Warning in your function: ")
            message(w)
          }
        )
      } else {
        stop("The anonymous function you provided is invalid.")
      }
    }
  }
  if ((length(rowIdx) < 1) || (!methods::is(rowIdx,"integer"))) 
    stop("your function returned invalid indices: '", rowIdx,"'")
  # use action arg
  if (action=="include") {
    return(samples[rowIdx, ])
  } else {
    return(samples[!rowIdx, ])
  }
}


#' Create a list of matched files in the system and unmatched regular expessions
#'
#' @param rgx string to expand in the system
#'
#' @return a list of all the elements after possible expansion
.matchesAndRegexes = function(rgx) {
  res = c()
  for(i in rgx){
    matched = Sys.glob(i)
    if(length(matched) < 1) {
      matched = i
    }
    res = c(res, matched)
  }
  return(list(res))
}


#' Print a nested list
#'
#' Prints a nested list in a way that looks nice
#'
#' Useful for displaying the config of a PEP
#'
#' @param lst list object to print
#' @param level the indentation level
#'
#' @examples
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_basic",
#' "project_config.yaml",
#' package = "pepr")
#' p = Project(file = projectConfig)
#' .printNestedList(config(p),level=2)
#' @export
.printNestedList = function(lst, level = 0) {
  if (!is.list(lst))
    stop("The input is not a list, cannot be displayed.")
  ns = names(lst)
  for (i in seq_along(lst)) {
    item = lst[[i]]
    itemName = ns[i]
    if (class(item) == "list") {
      if(!is.null(itemName))
        cat(rep(" ", level), paste0(itemName, ":"), fill = T)
      .printNestedList(item, level + 2)
    } else {
      if (is.null(item))
        item = "null"
      cat(rep(" ", level), paste0(itemName, ":"), item, fill = T)
    }
  }
}

#' Check for a section existence in a nested list
#'
#' @param object list to inspect
#' @param sectionNames vector or characters with sectio names to check for 
#'
#' @return logical indicating whether the sections where found in the list
#' @export
#'
#' @examples
#' l = list(a=list(b="test"))
#' .checkSection(l,c("a","b"))
#' .checkSection(l,c("c","b"))
.checkSection = function(object, sectionNames) {
  tryToNum = function(x){
    convertedX = suppressWarnings(as.numeric(x))
    ifelse(!is.na(convertedX), convertedX, x)
  }
  testList = object
  counter = 1
  while (!is.na(sectionNames[counter])) {
    item = tryToNum(sectionNames[counter])
    if((!is.list(testList)) || is.null(testList[[item]])){
      return(FALSE)
    }
    testList = testList[[item]]
    counter = counter + 1
  }
  return(TRUE)
}

