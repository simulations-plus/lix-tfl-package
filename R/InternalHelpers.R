# Function to load specified project, which will do nothing in the case that the project
# is already loaded
#

#' Check if project is loaded, and loads project if not
#'
#' Function to load specified project, which will do nothing in the case that the project
#' is already loaded. Will switch software for lixoftConnectors if necessary.
#'
#' @param project Project file
#'
.ensureProjectLoaded <- function(projectFile = NULL) {

	# Retrieve current project and connector state
	current_prj_info  <- tryCatch(lixoftConnectors::getProjectSettings(), error = function(e) NULL)
	connector_state <- tryCatch(lixoftConnectors::getLixoftConnectorsState(), error = function(e) NULL)

	if (is.null(projectFile)) {
		# CASE 1: No project argument →  use current

		if (is.null(current_prj_info) || is.null(current_prj_info$project)) {
			stop("No project is currently loaded and no project path was provided.")
		}
		message("No project specified, using project already loaded: ",
						current_prj_info$project)
	}
	else {
		# CASE 2: Project argument provided

		project_software <- .getSoftwareFromProjectFile(project_path)

		# Get information on currently loaded project, if any
		if (is.null(current_prj_info) || is.null(current_prj_info$project)) {
			same_project <- FALSE

			if (is.null(connector_state)) {
				# connectors not initialized
				same_software <- FALSE
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
			message("Loading new project: ", projectFile)
			lixoftConnectors::loadProject(projectFile = projectFile)
		}
		else {
			message("Project already loaded: ", current_project)
		}
	}
}

#' Gets the software corresponding to the project file extension
#'
#' @param projectFile Project file
#'
#' @returns "pkanalix", "monolix", or "simulx"
.getSoftwareFromProjectFile = function(projectFile)
{
	ext <- tolower(tools::file_ext(projectFile))
	software <- switch(ext,
										 pkx = "pkanalix",
										 mlxtran = "mlxtran",
										 smlx = "simulx",
										 "unknown")
	return(software)
}


#' Checks for project file
#'
#' Verifies project file is properly formatted and exists. Optionally verifies the file extension.
#'
#' @param projectFile Project file
#' @param ext [optional] File extension required
#'
.checkProjectFile = function(projectFile, ext = ".mlxtran")
{
	if (!is.character(projectFile))
		stop("The argument 'project_file' must be a string.", call. = FALSE)

	if (!endsWith(projectFile, ext))
		stop(paste0("The file extension of the argument 'projectFile' must be ", ext),
				 call. = FALSE)

	if (!file.exists(projectFile))
		stop(paste0("Could not find project file ", path.expand(projectFile)), call. = FALSE)
}

#' Rounds number to specified number of digits
#'
#' Two rounding methods are supported: "monolix" which mirrors the behavior of MonolixSuite
#' applications where all digits before the decimal are considered significant, and only digits
#' after the decimal are rounded, and "signif" where the number of significant digits are specified.
#'
#' @param number number to round
#' @param digits number of digits to round to
#' @param rounding_method "monolix" (default) or "signif"
#'
#' @returns rounded number
.round_num = function(number, digits, rounding_method = c("monolix", "signif")) {
	rounding_method <- match.arg(rounding_method)

	if (is.numeric(number)) {

		if (rounding_method == "monolix") {
			digits = digits + ifelse(number < 1, abs(floor(log10(abs(number))) + 1), 0)
			rr <- round(number, digits = digits)
		} else if (rounding_method == "signif") {
			rr <- signif(number, digits = digits)
		}
		rr <- as.character(rr)

	} else {
		rr <- number
	}
	return(rr)
}
