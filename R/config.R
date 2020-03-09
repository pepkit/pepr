setMethod("initialize", "Config", function(.Object, data) {
  .Object = methods::callNextMethod(.Object, data)  # calls list initialize
  .Object = .reformat(.Object)
  return(.Object)
})


#'  The constructor of a class representing PEP config
#'
#' @param file a character with project configuration yaml file
#' @param amendments a character with the amendments names to be activated
#'
#' @return
#' @examples
#' projectConfig = system.file("extdata", "example_peps-cfg2",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' c=Config(projectConfig)
#' @export
Config = function(file, amendments = NULL){
  message("Loading config file: ", file)
  cfg_data = .loadConfig(filename=file, amendments=amendments)
  config = methods::new("Config",data=cfg_data)
  config = makeSectionsAbsolute(config, REQ_ABS, file)
  .listAmendments(config)
  return(config)
}


#' Config objects are specialized list objects
#'
#' Config objects are used with the \code{\link{Project-class}} objects
#'
#' @exportClass Config
setClass("Config", contains = "list")

# Override the standard generic show function for our config-style lists
setMethod(
  "show",
  signature = "Config", 
  definition = function(object) {
    cat("Config object. Class:", class(object), fill = T)
    .printNestedList(object)
    invisible(NULL)
  }
)


#' Make sections absolute in the config
#' 
#' @export
setGeneric("makeSectionsAbsolute", function(object, sections, cfgPath)
  standardGeneric("makeSectionsAbsolute"))

setMethod(
  "makeSectionsAbsolute", 
  signature = signature(
    object="Config", 
    sections="character", 
    cfgPath="character"
    ), 
  definition = function(object, sections, cfgPath) {
    # Enable creation of absolute path using given parent folder path.
    absViaParent = pryr::partial(.makeAbsPath, parent=dirname(cfgPath))
    for(section in sections){
      if (section %in% names(object)){
        absSectionVals = c()
        for (iSection in object[section]){
          absSection = absViaParent(iSection)
          absSectionVals = append(absSectionVals, absSection)
        }
        object[section] = absSectionVals
      }
    }
  return(object)
  }
)


#' Check config spec version and reformat if needed
#'
#' @param object an object of \code{\link{Config-class}} 
#' 
#' @return an object of \code{\link{Config-class}} 
setGeneric(".reformat", function(object)
  standardGeneric(".reformat"))

setMethod(
  ".reformat", 
  signature = "Config", 
  definition = function(object) {
    if (CFG_VERSION_KEY %in% names(object)){
      split = str_split(object[[CFG_VERSION_KEY]],"\\.")[[1]]
      if (length(split) < 3) stop("PEP version string is not tripartite")
      majorVer = as.numeric(split[1])
      if (majorVer < 2){
        if (CFG_MODIFIERS_KEY %in% names(object)){
          stop("Project configuration file subscribes to specification 
               >= 2.0.0, since ",CFG_MODIFIERS_KEY," section is defined. Set ",
               CFG_VERSION_KEY, " to 2.0.0 in your config")
        } else{
          stop("Config file reformatting is not supported. 
               Reformat the config manually.")
        }
      }
    } else{
      stop("Config file is missing ", CFG_VERSION_KEY, " key. 
           Add it to the config manually.")
    }
    return(object)
  }
)


setGeneric("checkSection", function(object, sectionNames)
  standardGeneric("checkSection"))

#' Check for existence of a section in the Project config
#' 
#' This function checks for the section/nested sections in the config YAML file.
#'  Returns \code{TRUE} if it exist(s) or \code{FALSE} otherwise.
#' 
#' Element indices can be used instead of the actual names, see \code{Examples}.
#' 
#' @param object object of \code{\link[pepr]{Config-class}}
#' @param sectionNames the name of the section or names of the 
#'        nested sections to look for
#' 
#' @return a logical indicating whether the section exists
#' 
#' @examples
#' projectConfig = system.file("extdata", "example_peps-cfg2",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' checkSection(config(p),sectionNames = c("amendments","newLib"))
#' checkSection(config(p),sectionNames = c("amendments",1))
#' @export
setMethod(
  "checkSection",
  signature = "Config",
  definition = function(object, sectionNames) {
    # try co convert the section name to numeric, return original name if 
    # not possible this enables the outer method to check the sections 
    # existance by index and by name at the same time
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
)


#' View PEP config of the object of \code{\link{Project-class}}
#'
#' This method can be used to view the config slot of
#' the \code{\link{Project-class}} class
#'
#' @param object an object of \code{\link{Project-class}}
#'
#' @return a list with the config file
#'
#' @examples
#' projectConfig = system.file("extdata", "example_peps-cfg2",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' config(p)
#'
#' @export
setGeneric("config", function(object)
  standardGeneric("config"))

setMethod(
  "config",
  signature = "Project",
  definition = function(object) {
    object@config
  }
)

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
    stop("Config file found: ", filename)
  }
  # Initialize config object
  cfg_data = yaml::yaml.load_file(filename)
  if (!is.list(cfg_data))
    stop("The config file has to be a YAML formatted file.
         See: http://yaml.org/start.html")
  # Update based on imports inm the config file
  cfg_data = .applyImports(cfg_data, filename)
  # Update based on amendments if any specified
  cfg_data = .applyAmendments(cfg_data, amendments)
  # make bioconductor$readFunPath value absolute, used in BiocProject
  if(!is.null(cfg_data$bioconductor$readFunPath)){
    path = gsub("\\./","",cfg_data$bioconductor$readFunPath)
    cfg_data$bioconductor$readFunPath = 
      .makeAbsPath(path, parent=dirname(filename))
  }
  cfg_data$name = .inferProjectName(cfg_data, filename)
  return(cfg_data)
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
  if (length(names(cfg$amendments)) > 0) {
    # If there are any show a cat and return if needed
    printFun("  amendments: ", paste0(names(cfg$amendments), collapse=","))
    invisible(names(cfg$amendments))
  } else{
    # Otherwise return NULL for testing purposes
    NULL
  }
}

#' Apply amendments
#' 
#' Overwrite and/or add Project attributes from the amendments section
#'
#' @param cfg config
#' @param amendments 
#'
#' @return
.applyAmendments = function(cfg, amendments=NULL) {
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

#' Function for recursive config data imports
#'
#' @param cfg_data config data, possibly including imports statement
#' @param filename path to the file to get the imports for
#'
#' @return config data enriched in imported sections, if imports existed in the
#'  input
.applyImports = function(cfg_data, filename){
  if (!CFG_IMPORTS_KEY %in% names(cfg_data))
    return(cfg_data)
  for(externalPath in cfg_data[[CFG_IMPORTS_KEY]]){
    externalPath=.makeAbsPath(externalPath, parent = dirname(filename))
    extCfg = .loadConfig(filename = externalPath)  
    cfg_data = utils::modifyList(cfg_data, extCfg)
    message("  Loaded external config file: ", externalPath)
  }
  return(cfg_data)
}

#' Infer project name
#' 
#' Based on dedicated config section or PEP enclosing dir
#'
#' @param cfg config data
#' @param filename path to the config file
#'
#' @return
.inferProjectName = function(cfg, filename){
  if (!is.null(cfg$name)) return(cfg$name)
  return(basename(dirname(normalizePath(filename))))
}