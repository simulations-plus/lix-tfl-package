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

		if (is.null(current_prj_info) || is.null(current_prj_info$directory)) {
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
			dplyr::mutate(dplyr::across(tidyselect::where(is.double) & !dplyr::any_of(exclude),
																	.fns = ~.round_num(.x, digits, rounding_method = rounding_method))) %>%
			# Character-to-numeric conversion if needed
			dplyr::mutate(dplyr::across(
				tidyselect::where(is.character) & !dplyr::any_of(exclude),
				.fns = ~{
					num_converted <- suppressWarnings(as.numeric(.x))
					if (any(!is.na(num_converted))) {
						.round_num(num_converted, digits, rounding_method = rounding_method)
					} else {
						.x
					}
				}
			))
	}else if(is.numeric(to_round)) {
		# Track NaN positions BEFORE rounding
		nan_positions <- is.nan(to_round)

		if (rounding_method == "monolix") {
			# integer_part <- trunc(to_round)
			# decimal <- to_round - integer_part
			decimal <- to_round - as.integer(to_round)
			rr <- as.integer(to_round) + signif(decimal, digits = digits)
		} else if (rounding_method == "signif") {
			rr <- signif(to_round, digits = digits)
		}

		# Restore NaN values AFTER rounding
		rr[nan_positions] <- NaN
	}else{
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
	# Helper function to extract CI level from column name
	extract_ci_level <- function(col_name) {
		# Match pattern like IIV_CI_2.5_97.5 (with prefix)
		matches <- regmatches(col_name, regexec("_CI_([0-9.]+)_([0-9.]+)$", col_name))
		if (length(matches[[1]]) == 3) {
			lower_p <- as.numeric(matches[[1]][2])
			upper_p <- as.numeric(matches[[1]][3])
			ci_level <- round(upper_p - lower_p)
			return(ci_level)
		}
		# Match pattern like CI_2.5_97.5 (standalone, no prefix)
		matches <- regmatches(col_name, regexec("^CI_([0-9.]+)_([0-9.]+)$", col_name))
		if (length(matches[[1]]) == 3) {
			lower_p <- as.numeric(matches[[1]][2])
			upper_p <- as.numeric(matches[[1]][3])
			ci_level <- round(upper_p - lower_p)
			return(ci_level)
		}
		return(NULL)
	}

	# Static labels for exact matches
	stat_labels <- c(
		"parameter"           = "Parameter",
		"mean"                = "Mean",
		"sd"                  = "SD",
		"se"                  = "SE",
		"CV"                  = "CV (%)",
		"min"                 = "Minimum",
		"max"                 = "Maximum",
		"median"              = "Median",
		"geoMean"             = "Geometric\nMean",
		"GeoMean"             = "Geometric\nMean",
		"geoSD"               = "Geometric\nSD",
		"geoCV"               = "Geometric\nCV (%)",
		"GeomCV"              = "Geometric\nMean (CV %)",
		"GeomSD"              = "Geometric\nMean (SD)",
		"MeanSD"              = "Arithmetric\nMean (SD)",
		"MeanCV"              = "Arithmetric\nMean (CV %)",
		"harmMean"            = "Harmonic\nMean",
		"P05"                 = "5th Percentile",
		"P95"                 = "95th Percentile",
		"N"                   = "N",
		"Ntot"                = "N Total",
		"Nobs"                = "n",
		"Nmiss"               = "N Missing",
		"p.value"             = "p-value",
		"RSE"                 = "RSE (%)",
		"CI"                  = "Confidence \nInterval",
		"FE_Estimate"         = "Fixed Effect \nEstimate",
		"FE_RSE"              = "RSE (%)",
		"IIV_SD"              = "IIV\nSD",
		"IIV_CV"              = "IIV\nCV (%)",
		"IIV_RSE"             = "IIV\nRSE (%)",
		"IOV_SD"              = "IOV\nSD",
		"IOV_CV"              = "IOV\nCV (%)",
		"IOV_RSE"             = "IOV\nRSE (%)",
		"Dosing.range"        = "Dosing Range",
		"Point.estimate"      = "Point Estimate",
		"Criteria.interval"   = "Criteria\nInterval",
		"Ratio"               = "Geometric Mean\nRatio (%)",
		"Parent"              = "Parent Run",
		"Path"                = "Project Path",
		"LLlin"               = "LL\n(Linearization)",
		"LLis"                = "LL\n(Imp. Sampling)",
		"BICclin"             = "BICc\n(Linearization)",
		"BICcis"              = "BICc\n(Imp. Sampling)",
		"dLLlin"              = "ΔLL\n(Linearization)",
		"dLLis"               = "ΔLL\n(Imp. Sampling)",
		"dBICclin"            = "ΔBICc\n(Linearization)",
		"dBICcis"             = "ΔBICc\n(Imp. Sampling)")

	# Process each column name
	col_names <- names(df)
	new_names <- sapply(col_names, function(col) {
		# First check for dynamic CI columns (pattern matching for any confidence level)
		# Matches both prefixed (e.g., IIV_CI_2.5_97.5) and standalone (e.g., CI_2.5_97.5)
		if (grepl("(^|_)CI_[0-9.]+_[0-9.]+$", col)) {
			ci_level <- extract_ci_level(col)

			if (!is.null(ci_level)) {
				# Determine prefix and format accordingly (check most specific first)
				if (grepl("^Boot_IOV_CI", col)) {
					return(paste0("Bootstrap IOV\n", ci_level, "% CI"))
				} else if (grepl("^Boot_IIV_CI", col)) {
					return(paste0("Bootstrap IIV\n", ci_level, "% CI"))
				} else if (grepl("^Boot_CI", col)) {
					return(paste0("Bootstrap\n", ci_level, "% CI"))
				} else if (grepl("^IOV_CI", col)) {
					return(paste0("IOV\n", ci_level, "% CI"))
				} else if (grepl("^IIV_CI", col)) {
					return(paste0("IIV\n", ci_level, "% CI"))
				} else if (grepl("^FE_CI", col)) {
					return(paste0(ci_level, "% CI"))
				} else if (grepl("^CI_", col)) {
					return(paste0(ci_level, "% CI"))
				}
			}
		}

		# Handle Bootstrap IOV columns (non-CI)
		if (grepl("^Boot_IOV_", col)) {
			suffix <- sub("^Boot_IOV_", "", col)
			if (suffix == "Median") return("Bootstrap IOV\nMedian")
			if (suffix == "RSE") return("Bootstrap IOV\nRSE (%)")
			return(paste0("Bootstrap IOV\n", suffix))
		}

		# Handle Bootstrap IIV columns (non-CI)
		if (grepl("^Boot_IIV_", col)) {
			suffix <- sub("^Boot_IIV_", "", col)
			if (suffix == "Median") return("Bootstrap IIV\nMedian")
			if (suffix == "RSE") return("Bootstrap IIV\nRSE (%)")
			return(paste0("Bootstrap IIV\n", suffix))
		}

		# Handle Bootstrap FE columns (non-CI)
		if (grepl("^Boot_", col)) {
			suffix <- sub("^Boot_", "", col)
			if (suffix == "Median") return("Bootstrap\nMedian")
			if (suffix == "RSE") return("Bootstrap\nRSE (%)")
			return(paste0("Bootstrap\n", suffix))
		}

		# Check static labels for exact match
		if (col %in% names(stat_labels)) {
			return(unname(stat_labels[col]))
		}

		# Default: keep original name
		return(col)
	}, USE.NAMES = FALSE)

	names(df) <- new_names
	return(df)
}

#' Filter run history table to show only selected criteria columns
#'
#' @description
#' Removes LL or BICc columns (including deltas) based on user selection.
#' - filterCriteria = "LL": removes BICclin, BICcis, dBICclin, dBICcis
#' - filterCriteria = "BICc": removes LLlin, LLis, dLLlin, dLLis
#' - filterCriteria = "none": keeps all columns (default)
#'
#' @param df Data frame with run history
#' @param filterCriteria Which criteria to filter: "none", "LL", or "BICc"
#' @return Data frame with filtered columns
#' @keywords internal
.filterCriteriaColumns <- function(df, filterCriteria = c("none", "LL", "BICc")) {
	filterCriteria <- match.arg(filterCriteria)

	if (filterCriteria == "none") {
		return(df)
	}

	# Define columns to remove (base + delta columns)
	if (filterCriteria == "LL") {
		# Keep LL columns, remove all BICc columns
		cols_to_remove <- c("BICclin", "BICcis", "dBICclin", "dBICcis")
	} else {
		# Keep BICc columns, remove all LL columns
		cols_to_remove <- c("LLlin", "LLis", "dLLlin", "dLLis")
	}

	# Remove only columns that exist
	cols_to_remove <- intersect(cols_to_remove, names(df))
	df <- df[, !names(df) %in% cols_to_remove, drop = FALSE]

	return(df)
}

#' Validate sorting and filtering criteria compatibility
#'
#' @description
#' Ensures that sortByCriteria and filterCriteria are compatible:
#' - If sorting by a criteria, you can only filter by "none" or that same criteria
#' - Cannot sort by LL but filter to show only BICc (and vice versa)
#'
#' @param sortByCriteria Sorting criteria: "none", "LL", or "BICc"
#' @param filterCriteria Filter criteria: "none", "LL", or "BICc"
#' @return NULL (invisibly) if valid, otherwise stops with error
#' @keywords internal
.validateCriteriaArgs <- function(sortByCriteria, filterCriteria) {
	# If not sorting, any filterCriteria is valid
	if (sortByCriteria == "none") {
		return(invisible(NULL))
	}

	# If sorting by a criteria, filterCriteria must be "none" or match sortByCriteria
	if (filterCriteria != "none" && filterCriteria != sortByCriteria) {
		stop(sprintf(
			"Incompatible arguments: sortByCriteria = '%s' but filterCriteria = '%s'.\n%s",
			sortByCriteria, filterCriteria,
			"When sorting by a criteria, filterCriteria must be 'none' or match the sorting criteria."
		), call. = FALSE)
	}

	return(invisible(NULL))
}

#' Detect which estimation method to use for sorting
#'
#' @description
#' When both linearization and importance sampling results are available,
#' determines which method to use based on the parent/root project.
#'
#' @param df Data frame with run history
#' @param col_lin Column name for linearization values
#' @param col_is Column name for importance sampling values
#' @return Column name to use for sorting
#' @keywords internal
.detectSortingMethod <- function(df, col_lin, col_is) {
	# Find root project(s) - those without a parent
	root_rows <- which(is.na(df$Parent) | df$Parent == "")


	if (length(root_rows) == 0) {
		# No root found, prefer importance sampling if available
		if (any(!is.na(df[[col_is]]))) return(col_is)
		return(col_lin)
	}

	# Check which method the root project used (prefer IS over lin)
	root_is <- df[[col_is]][root_rows[1]]
	root_lin <- df[[col_lin]][root_rows[1]]

	if (!is.na(root_is)) {
		return(col_is)
	} else if (!is.na(root_lin)) {
		return(col_lin)
	} else {
		# Root has neither - check what's available globally
		if (any(!is.na(df[[col_is]]))) return(col_is)
		return(col_lin)
	}
}

#' Sort run history table by model selection criteria
#'
#' @description
#' Sorts the run history table by LL (log-likelihood/OFV) or BICc.
#' Automatically detects whether linearization or importance sampling
#' results are available and chooses appropriately.
#'
#' @param df Data frame with run history (must contain LL/BICc columns)
#' @param sortByCriteria Sorting criteria: "none", "LL", or "BICc"
#' @return Sorted data frame (ascending order - best model first)
#' @keywords internal
.sortRunHistory <- function(df, sortByCriteria = c("none", "LL", "BICc")) {
	sortByCriteria <- match.arg(sortByCriteria)

	if (sortByCriteria == "none") {
		return(df)
	}

	# Map criteria to column names
	if (sortByCriteria == "LL") {
		col_lin <- "LLlin"
		col_is  <- "LLis"
	} else {
		col_lin <- "BICclin"
		col_is  <- "BICcis"
	}

	# Check column availability
	has_lin <- col_lin %in% names(df) && any(!is.na(df[[col_lin]]))
	has_is  <- col_is %in% names(df) && any(!is.na(df[[col_is]]))

	if (!has_lin && !has_is) {
		warning(sprintf("No %s results available for sorting. Returning unsorted.", sortByCriteria),
						call. = FALSE)
		return(df)
	}

	# Determine which column to sort by
	if (has_lin && has_is) {
		# Both methods present - detect based on parent/root project
		sort_col <- .detectSortingMethod(df, col_lin, col_is)
	} else if (has_is) {
		sort_col <- col_is
	} else {
		sort_col <- col_lin
	}

	# Sort ascending (lowest = best for both OFV and BICc)
	df <- df[order(df[[sort_col]], na.last = TRUE), ]
	rownames(df) <- NULL

	return(df)
}


################################################################################
#' .applyCustomAliasesUnits
#'
#' Software-independent helper to apply custom aliases and/or units to a parameter
#' column in a dataframe. This function does NOT fetch aliases/units from any
#' project - it only applies the custom mappings provided.
#'
#' @param df A dataframe containing a parameter column.
#' @param param_col Character string specifying the name of the parameter column.
#'   Default is "parameter".
#' @param aliases_df Optional dataframe with columns "parameter" and "aliases".
#'   If provided, parameter values are replaced with their aliases.
#' @param units_df Optional dataframe with columns "parameter" and "units".
#'   If provided, units are appended to parameter names in parentheses.
#' @param orig_param_col Optional character string specifying a column containing
#'   original parameter names for unit lookup (useful when aliases have already
#'   been applied). If NULL, uses param_col for lookup.
#'
#' @return A dataframe with the parameter column updated with aliases and/or units.
#'
#' @details
#' The function applies transformations in order:
#' 1. Aliases are applied first (parameter values are replaced)
#' 2. Units are appended second (in parentheses after the parameter name)
#'
#' If both aliases and units are provided, the final format is: "alias (unit)"
#'
#' @noRd
.applyCustomAliasesUnits <- function(df,
																		 param_col      = "parameter",
																		 aliases_df     = NULL,
																		 units_df       = NULL,
																		 orig_param_col = NULL) {

	# Nothing to do if both are NULL
	if (is.null(aliases_df) && is.null(units_df)) {
		return(df)
	}

	# Determine lookup column for units (before aliases change param_col values)
	if (is.null(orig_param_col)) {
		# Store original parameter values for unit lookup
		orig_params <- df[[param_col]]
	} else {
		orig_params <- df[[orig_param_col]]
	}

	# Apply aliases (replace parameter values)
	if (!is.null(aliases_df)) {
		alias_map <- setNames(aliases_df$aliases, aliases_df$parameter)
		# Apply mapping, keeping original value if no alias defined
		df[[param_col]] <- sapply(df[[param_col]], function(p) {
			if (p %in% names(alias_map) && !is.na(alias_map[p])) {
				alias_map[p]
			} else {
				p
			}
		}, USE.NAMES = FALSE)
	}

	# Apply units (append to parameter names)
	if (!is.null(units_df)) {
		units_map <- setNames(units_df$units, units_df$parameter)
		# Append units using original parameter names for lookup
		df[[param_col]] <- sapply(seq_len(nrow(df)), function(i) {
			orig_p <- orig_params[i]
			current_p <- df[[param_col]][i]
			unit <- units_map[orig_p]

			if (!is.na(unit) && nchar(trimws(unit)) > 0) {
				paste0(current_p, " (", unit, ")")
			} else {
				current_p
			}
		}, USE.NAMES = FALSE)
	}

	# Remove orig_param_col if it was provided and exists
	if (!is.null(orig_param_col) && orig_param_col %in% names(df) && orig_param_col != param_col) {
		df[[orig_param_col]] <- NULL
	}

	return(df)
}

#' Get residual error parameter names from the currently loaded Monolix project
#'
#' Returns the names of all residual error parameters across continuous outputs,
#' including autocorrelation parameters (r, r1, r2, ...) for outputs with
#' autocorrelation enabled. Returns an empty character vector for projects with
#' no continuous observations (e.g. pure RTTE models).
#'
#' @noRd
.getResidualErrorParameters <- function() {
	obs_model <- try(lixoftConnectors::getContinuousObservationModel(), silent = TRUE)
	if (inherits(obs_model, "try-error") || is.null(obs_model$parameters)) {
		return(character(0))
	}

	res_err_params <- unlist(obs_model$parameters, use.names = FALSE)

	# Add the autocorrelation parameter r<suffix> for each output where
	# autocorrelation is enabled. The suffix matches that of the output's existing
	# residual error parameters (e.g. "a1" -> "r1", "a" -> "r").
	if (!is.null(obs_model$autocorrelation)) {
		autocorr_outputs <- names(obs_model$autocorrelation)[
			vapply(obs_model$autocorrelation, isTRUE, logical(1))
		]
		for (output_name in autocorr_outputs) {
			output_params <- obs_model$parameters[[output_name]]
			if (length(output_params) > 0) {
				suffix <- sub("^[a-zA-Z]+", "", output_params[[1]])
				res_err_params <- c(res_err_params, paste0("r", suffix))
			}
		}
	}

	res_err_params
}
