# quiets NOTE from R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c("."))

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
	# Note: directory and seed are only project settings common to all 3 software
	current_prj_info  <- suppressMessages(lixoftConnectors::getProjectSettings())
	connector_state <- suppressMessages(lixoftConnectors::getLixoftConnectorsState())

	if (is.null(projectFile)) {
		# CASE 1: No project argument →  use current

		if (is.null(current_prj_info) || is.null(current_prj_info$directory)) {
			stop("No project is currently loaded and no project was provided.",
					 call. = FALSE)
		}
		.TFL_message("No project specified, using project results from: ",
						current_prj_info$directory)
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

.checkFlextable <- function() {
	if (!requireNamespace("flextable", quietly = TRUE))
	{
		stop("The flextable package is necessary for this function. Please install it from CRAN.",
				 call. = FALSE)
	}
}

# TODO: decide if we want to check for this in .ensureProjectLoaded (that is zip loaded and not connectors)
# problem is that we would not necessarily be able to detect all problem cases (e.g. both already attached but in wrong order)
.checkZipLoadedOrAttached <- function() {
	session = sessionInfo()
	zipLoaded = FALSE

	attached = sapply(session$otherPkgs, function(x) x$Package)
	loaded = sapply(session$loadedOnly, function(x) x$Package)

	if ("zip" %in% attached || "zip" %in% loaded) {
		zipLoaded = TRUE
	}
	return(zipLoaded)
}

#' Rounds numbers to specified number of digits
#'
#' Two rounding methods are supported: "monolix" which mirrors the behavior of MonolixSuite
#' applications where all digits before the decimal are considered significant, and only digits
#' after the decimal are rounded, and "signif" where the number of significant digits are specified.
#' To retain the original values and not round, use "none".
#'
#' @param to_round number, vector or data frame to round
#' @param digits number of digits to round to
#' @exclude names of columns to exclude
#' @param rounding_method "monolix" (default), "signif", or "none"
#'
#' @returns rounded number
#' @noRd
.round_num = function(to_round, digits, exclude = character(0), rounding_method = c("monolix", "signif", "none")) {
	rounding_method <- match.arg(rounding_method)

	if (rounding_method == "none") {
		return(to_round)
	}

	if (is.data.frame(to_round)) {
		rr <- to_round %>%
			dplyr::mutate(dplyr::across(tidyselect::where(is.double) & !any_of(exclude),
																	.fns = ~.round_num(.x, digits, rounding_method = rounding_method)))
	}
	else if (is.numeric(to_round)) {

		if (rounding_method == "monolix") {
			decimal <- to_round - as.integer(to_round)
			rr <- as.integer(to_round) + signif(decimal, digits = digits)
		} else if (rounding_method == "signif") {
			rr <- signif(to_round, digits = digits)
		}
	}
	else {
		rr <- to_round
	}
	return(rr)
}

#' Combines two metrics into a single column, rounding them first
#'
#' @param value1 main metric
#' @param value2 metric in parentheses
#' @param digits number of digits to round to
#' @exclude names of columns to exclude
#' @param rounding_method "monolix" (default), "signif", or "none"
#'
#' @returns "value1 (value2)"
.combineMetrics <- function(value1, value2,
														digits, rounding_method = c("monolix", "signif", "none")) {
	paste0(.round_num(value1, digits, rounding_method = rounding_method),
				 " (", .round_num(value2, digits, rounding_method = rounding_method), ")" )
}

#' PrettyLabels
#'
#' Converts statistical parameter column names to human-readable, formatted labels
#' for presentation in tables and reports. Maps common statistical abbreviations
#' to their full, properly formatted names.
#'
#' @param df A data frame containing statistical columns to be relabeled.
#'
#' @return A data frame with column names converted to pretty, human-readable labels.
.prettyLabels <- function(df) {
	stat_labels <- c(
		"parameter"           = "Parameter",
		"mean"                = "Mean",
		"sd"                  = "SD",
		"se"                  = "SE",
		"CV"                  = "CV (%)",
		"Min"                 = "Minimum",
		"Max"                 = "Maximum",
		"median"              = "Median",
		"geoMean"             = "Geometric\nMean",
		"GeoMean"             = "Geometric\nMean",
		"geoSD"               = "Geometric\nSD",
		"geoCV"               = "Geometric\nCV (%)",
		"GeomCV"              = "Geometric\nMean (CV %)",
		"GeomSD"              = "Geometric\nMean (SD)",
		"MeanSD"              = "Arithmetric\nMean (SD)",
		"harmMean"            = "Harmonic\nMean",
		"P05"                 = "5th Percentile",
		"P95"                 = "95th Percentile",
		"Ntot"                = "N Total",
		"Nobs"                = "N", # "N Observed"
		"Nmiss"               = "N Missing",
		"p.value"             = "p-value",
		"Dosing.range"        = "Dosing Range",
		"Point.estimate"      = "Point Estimate",
		"Criteria.interval"   = "Criteria Interval",
		"Confidence.interval" = "Confidence Interval"
	)

	# Replace column names where matches are found
	col_names <- names(df)
	labelIdx <- match(col_names, names(stat_labels))
	col_names[!is.na(labelIdx)] <- stat_labels[stats::na.omit(labelIdx)]

	names(df) <- col_names
	return(df)
}
