#' Print a message (info) to the user, checking option if package should be verbose or not
#'
#' @param ... message components
#'
#' @noRd
.TFL_message <- function(...) {
	if (getOption("TFLMonolixSuite.verbose", FALSE)) {
		message(...)
	}
}


# Function to load specified project, which will do nothing in the case that the project
# is already loaded
#

#' Check if project is loaded, and loads project if not
#'
#' Function to load specified project, which will do nothing in the case that the project
#' is already loaded. Will switch software for lixoftConnectors if necessary.
#'
#' @param projectFile Project file
#'
#'@noRd
.ensureProjectLoaded <- function(projectFile = NULL) {

	# suppress lixoftConnector messages (i.e., ERROR, WARNING, and INFO) in the case of connectors not initialized,
	# no project loaded, etc., the functions return NULL in these cases
	current_prj_info  <- suppressMessages(lixoftConnectors::getProjectSettings())
	connector_state <- suppressMessages(lixoftConnectors::getLixoftConnectorsState())

	if (is.null(projectFile)) {
		# CASE 1: No project argument →  use current

		if (is.null(current_prj_info) || is.null(current_prj_info$project)) {
			stop("No project is currently loaded and no project was provided.",
					 call. = FALSE)
		}
		.TFL_message("No project specified, using project already loaded: ",
						current_prj_info$project)
	}
	else {
		# CASE 2: Project argument provided

		project_software <- .getSoftwareFromProjectFile(projectFile)
		if (project_software == "unknown") {
			stop("Unknown file type: ", projectFile)
		}

		# Get information on currently loaded project, if any
		# Note: only Monolix provides getProjectInfo()$project, so the only one we can verify which project loaded
		if (is.null(current_prj_info) || is.null(current_prj_info$project) || project_software != "monolix") {
			same_project <- FALSE

			if (is.null(connector_state)) {
				# connectors not initialized
				same_software <- FALSE
			} else{
				current_software <- connector_state$software
				same_software <- project_software == current_software
			}
		}
		else {
			# Project already loaded
			project_path <- normalizePath(projectFile, winslash = "/", mustWork = FALSE)
			current_project <- normalizePath(current_prj_info$project, winslash = "/", mustWork = FALSE)
			current_software <- connector_state$software

			same_project <- project_path == current_project
			same_software <- project_software == current_software
		}

		# Load project only if it differs or none loaded
		if (!same_project) {
			if (!same_software) {
				lixoftConnectors::initializeLixoftConnectors(software = project_software, force = TRUE)
			}
			.TFL_message("Loading new project: ", projectFile)
			lixoftConnectors::loadProject(projectFile = projectFile)
		}
		else {
			.TFL_message("Project already loaded: ", current_project)
		}
	}
}

#' Gets the software corresponding to the project file extension
#'
#' Note this only includes the three applications supported by the lixoftConnectors
#'
#' @param projectFile Project file
#'
#' @returns "pkanalix", "monolix", or "simulx" (or "unknown" for unrecognized extension)
#' @noRd
.getSoftwareFromProjectFile = function(projectFile)
{
	ext <- tolower(tools::file_ext(projectFile))
	software <- switch(ext,
										 pkx = "pkanalix",
										 mlxtran = "monolix",
										 smlx = "simulx",
										 "unknown")
	return(software)
}

#' Gets the extension corresponding to the specified software application
#'
#' @param software Software application
#'
#' @returns File extension (or "unknown" for unrecognized application)
#' @noRd
.getExtentionFromSoftware = function(software)
{
	software <- tolower(software)
	ext <- switch(software,
								pkanalix = "pkx",
								monolix = "mlxtran",
								simulx = "smlx",
								sycomore = "syc",
								"unknown")
	return(ext)
}


#' Checks the project file matches the specified software
#'
#' Checks that project file is for the specified software application via the file extension.
#' If the project file is NULL, checks that there is a currently loaded project of the
#' specified software application.
#'
#' @param projectFile Project file
#' @param software Software application required (multiple can be specified)
#'
#'@noRd
.checkProjectFile = function(projectFile, software)
{
	# if project file is NULL, check that currently loaded project is correct software
	if (is.null(projectFile)) {
		# check if currently loaded project is the specified software application
		current_prj_info  <- tryCatch(lixoftConnectors::getProjectSettings(), error = function(e) NULL)

		if (is.null(current_prj_info) || is.null(current_prj_info$project)) {
			stop("No project is currently loaded and no project was provided.",
					 call. = FALSE)
		}
		else {
			connector_state <- tryCatch(lixoftConnectors::getLixoftConnectorsState(), error = function(e) NULL)

			if (!(connector_state$software %in% software)) {
				stop("This function is only available for ", software,
						 " projects,  while the current project uses ", connector_state$software, ".",
						 call. = FALSE)
			}
		}
	}
	# otherwise verify file extension of to be loaded project
	else {
		ext <- sapply(software, .getExtentionFromSoftware)
		if (!any(endsWith(projectFile, ext)))
			stop("This function is only available for ", software, " projects, while the specified project is ", ext, ".",
					 call. = FALSE)
	}
}

#' Rounds number to specified number of digits
#'
#' Two rounding methods are supported: "monolix" which mirrors the behavior of MonolixSuite
#' applications where all digits before the decimal are considered significant, and only digits
#' after the decimal are rounded, and "signif" where the number of significant digits are specified.
#'
#' For the two rounding methods, the result is returned as character, while for "none" the numeric result
#' is retained.
#'
#' @param number number to round
#' @param digits number of digits to round to
#' @param rounding_method "monolix" (default), "signif", or "none"
#'
#' @returns rounded number
#' @noRd
.round_num = function(number, digits, rounding_method = c("monolix", "signif", "none")) {
	rounding_method <- match.arg(rounding_method)

	if (rounding_method == "none") {
		return(number)
	}

	if (!is.numeric(digits)) {
		stop("digits argument must be numeric", call. = FALSE)
	}
	if (is.numeric(number)) {

		if (rounding_method == "monolix") {
			decimal <- number - as.integer(number)
			rr <- as.integer(number) + signif(decimal, digits = digits)
		} else if (rounding_method == "signif") {
			rr <- signif(number, digits = digits)
		}
		rr <- as.character(rr)

	} else {
		rr <- number
	}
	return(rr)
}
