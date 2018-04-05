
#' Create an absolute path from a primary target and a parent candidate.
#
#' @param perhapsRelative: Path to primary target directory.
#' @param  parent: Path to parent folder to use if target isn't absolute.
#
#' @return	Target itself if already absolute, else target nested within parent.
.makeAbsPath = function(perhapsRelative, parent) {
	if (.isAbsolute(perhapsRelative)) {
		abspath = perhapsRelative
	} else {
		abspath = file.path(normalizePath(parent), perhapsRelative)
	}
	if (!.isAbsolute(abspath)) {
		errmsg = sprintf("Relative path '%s' and parent '%s' failed to create
			absolute path: '%s'", perhapsRelative, parent, abspath)
		stop(errmsg)
	}
	return(abspath)
}

.isDefined = function(var) { ! (is.na(var) | is.null(var)) }

# Filesystem utilities

#' Determine whether a path is absolute.
#'
#' @param path The path to check for seeming absolute-ness.
#' @return Flag indicating whether the \code{path} appears to be absolute.
#' @family path operations
.isAbsolute = function(path) {
	firstChar = substr(path, 1, 1)
	return(identical("/", firstChar) | identical("~", firstChar))
}

.safeFileExists = function(path) {
	( (! is.null(path)) && file.exists(path) )
}