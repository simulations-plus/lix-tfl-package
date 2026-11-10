################################################################################
#-------------------Quarto Report: Non Compartmental analysis------------------#
#----------------------------Internal Helpers----------------------------------#
################################################################################

# Internal Helper Functions
# These functions are prefixed with a dot (.) and are used internally by
# the public PKanalix helper functions.

##################################################################################
#' ConvertHtmlAliasesForFlextable
#'
#' @description
#' Transforms HTML-formatted parameter aliases (containing <sub>, <sup> tags) into
#' flextable-compatible placeholder format. This function is part of the table
#' formatting pipeline and maintains separation between data helpers and presentation
#' logic.
#'
#' @details
#' This function converts HTML markup to neutral placeholders that can be decoded
#' later by flextable rendering functions. The conversion happens in two stages:
#'
#' 1. HTML tags → Placeholders: <sub>text</sub> becomes [[sub:text]]
#' 2. Cleanup: Any remaining HTML tags are stripped
#'
#' **Design Philosophy:**
#' - Table helpers should be presentation-agnostic (don't know about flextable)
#' - Placeholders serve as an intermediate format between HTML and flextable
#' - Actual rendering (as_sub(), as_sup()) happens in styleTFL()
#'
#' @param df A data frame containing an "aliases" column with HTML-formatted text.
#'   Typically obtained from getNCAParameterStatistics()$information
#'
#' @return A data frame identical to input but with the "aliases" column transformed:
#'   - HTML subscript tags converted to [[sub:...]] placeholders
#'   - All other HTML tags removed
#'   - Plain text preserved as-is
#'
#' applied i helper function .addUnitsAliases()
#'
.convertHtmlAliasesForFlextable <- function(df) {
	convert_sub_and_sup <- function(alias) {
		# Convert <sub>text</sub> to [[sub:text]] placeholder
		alias_clean <- stringr::str_replace_all(alias, "<sub>(.*?)</sub>", "[[sub:\\1]]")
		# Remove any remaining HTML tags
		stringr::str_remove_all(alias_clean, "<[^>]+>")
	}

	df %>%
		dplyr::mutate(!!"aliases" := vapply(.data[["aliases"]], convert_sub_and_sup, character(1)))
}


##################################################################################
#  .normalizeAliasMapping()
#' Normalize alias/unit mapping input to a standardized dataframe format
#'
#' Accepts either a named vector or a 2-column dataframe and converts it to
#' a standardized dataframe with columns "parameter" and the specified value column.
#'
#' @param mapping A named vector (e.g., c(Cmax = "C~max~")) or a 2-column dataframe
#'   where the first column contains parameter names and the second contains values.
#' @param value_col Character string specifying the name of the value column in the
#'   output dataframe. Default is "aliases".
#'
#' @return A dataframe with columns "parameter" and \code{value_col}, or NULL if
#'   input is NULL.
#'
.normalizeAliasMapping <- function(mapping, value_col = "aliases") {
	# Return NULL if input is NULL
	if (is.null(mapping)) {
		return(NULL)
	}

	# Case 1: Named vector -> convert to dataframe
	if (is.vector(mapping) && !is.data.frame(mapping) && !is.null(names(mapping))) {
		result <- data.frame(
			parameter = names(mapping),
			value = unname(mapping),
			stringsAsFactors = FALSE
		)
		names(result) <- c("parameter", value_col)
		return(result)
	}

	# Case 2: Dataframe -> rename columns
	if (is.data.frame(mapping)) {
		if (ncol(mapping) < 2) {
			stop("customAliases/customUnits dataframe must have at least 2 columns.", call. = FALSE)
		}
		# Take first two columns and rename
		result <- mapping[, 1:2, drop = FALSE]
		names(result) <- c("parameter", value_col)
		# Ensure character type
		result$parameter <- as.character(result$parameter)
		result[[value_col]] <- as.character(result[[value_col]])
		return(result)
	}

	# Invalid input
	stop("customAliases/customUnits must be a named vector or a 2-column dataframe.", call. = FALSE)
}

###############################################################################
#' .formatUnits
#'
#' Transforms unit strings from PKanalix notation to publication-ready
#' display format. Converts denominator notation (e.g., `^-1`) to fraction
#' format and replaces multiplication separators with `*`.
#'
#' @param unit_string A character string representing a unit in PKanalix notation
#'   (e.g., "h.ng.mL^-1", "ng.mL^-1").
#'
#' @return A character string with units formatted for display:
#'   - Denominators (units with `^-1`) moved after `/`
#'   - Multiplication shown as `*`
#'   - Returns input unchanged if NA or empty
#'
#' @examples
#' .formatUnits("h.ng.mL^-1")
#' # Returns: "h*ng/mL"
#'
#' .formatUnits("ng.mL^-1")
#' # Returns: "ng/mL"
#'
#' .formatUnits("mL^-1")
#' # Returns: "1/mL"
#'
#' .formatUnits("h")
#' # Returns: "h"
#'
#' @seealso \code{\link{.addUnitsAliases}} which uses this helper internally
#'
.formatUnits <- function(unit_string) {
	if (is.na(unit_string) || unit_string == "") return(unit_string)

	# Split by any multiplication separator (., *, middle dot)
	units <- unlist(strsplit(unit_string, "[.\\*\u22c5]"))

	# Separate into numerator and denominator - order independent
	numerator <- c()
	denominator <- c()

	for (u in units) {
		if (grepl("\\^-1$", u)) {
			denominator <- c(denominator, sub("\\^-1$", "", u))
		} else {
			numerator <- c(numerator, u)
		}
	}

	# Build result: all numerators together, then / , then all denominators
	num_str <- if (length(numerator) == 0) "1" else paste(numerator, collapse = "*")

	if (length(denominator) > 0) {
		denom_str <- paste(denominator, collapse = "*")
		return(paste0(num_str, "/", denom_str))
	}

	return(num_str)
}

###############################################################################
#' .addUnitsAliases
#'
#' Enhances parameter data frames by adding units and aliases to create formatted
#' display names. Combines parameter names with their corresponding units and aliases
#' to generate publication-ready parameter labels with proper fallback handling.
#' @param df A data frame containing a 'parameter' column.
#' @param add_units Logical; if TRUE, adds units to display names.
#' @param add_aliases Logical; if TRUE, adds aliases to display names.
#' @param units_df Optional dataframe with columns "parameter" and "units".
#'   If provided, uses these instead of fetching from PKanalix project.
#' @param aliases_df Optional dataframe with columns "parameter" and "aliases".
#'   If provided, uses these instead of fetching from PKanalix project.
#'
#' @return A data frame with the 'parameter' column replaced by formatted display names.
.addUnitsAliases <- function(df, add_units = TRUE, add_aliases = TRUE, units_df = NULL, aliases_df = NULL) {

	# ---------------------------------------------------------------------------
	# UNITS: Use custom data or fetch from PKanalix
	# ---------------------------------------------------------------------------
	if (isTRUE(add_units)) {
		if (!is.null(units_df)) {
			# Use custom units (already normalized to parameter/units columns)
			# Apply .formatUnits() to ensure consistent formatting
			units_df <- units_df %>%
				dplyr::mutate(units = sapply(units, .formatUnits))
		} else {
			# Fetch from PKanalix project (existing behavior)
			allparams <- lixoftConnectors::getNCASettings()$computedncaparameters

			NCAParamSummary_info <- lixoftConnectors::getNCAParameterStatistics(parameters = allparams)$information %>%
				tibble::rownames_to_column(var = "parameter")

			if (isTRUE(lixoftConnectors::getDataSettings()$enableunits)) {
				units_df <- NCAParamSummary_info %>%
					dplyr::select(parameter, units) %>%
					dplyr::mutate(units = sapply(units, .formatUnits))
			} else {
				warning("Units were requested (units=TRUE) but are not enabled in the current project. Skipping unit retrieval.")
				units_df <- NULL
			}
		}
	} else {
		units_df <- NULL
	}

	# ---------------------------------------------------------------------------
	# ALIASES: Use custom data or fetch from PKanalix
	# ---------------------------------------------------------------------------
	if (isTRUE(add_aliases)) {
		if (!is.null(aliases_df)) {
			# Use custom aliases (already normalized to parameter/aliases columns)
			# Apply HTML conversion so users can use <sub>text</sub> syntax
			aliases_df <- .convertHtmlAliasesForFlextable(aliases_df)
		} else {
			# Fetch from PKanalix project (existing behavior)
			if (!exists("NCAParamSummary_info")) {
				allparams <- lixoftConnectors::getNCASettings()$computedncaparameters
				NCAParamSummary_info <- lixoftConnectors::getNCAParameterStatistics(parameters = allparams)$information %>%
					tibble::rownames_to_column(var = "parameter")
			}

			if ("aliases" %in% names(NCAParamSummary_info)) {
				aliases_df <- NCAParamSummary_info %>%
					dplyr::select(parameter, aliases) %>%
					.convertHtmlAliasesForFlextable()
			} else {
				warning("Aliases were requested (aliases=TRUE) but are not available in the current project. Skipping alias retrieval.")
				aliases_df <- NULL
			}
		}
	} else {
		aliases_df <- NULL
	}

	# ---------------------------------------------------------------------------
	# MERGE AND BUILD DISPLAY NAMES (unchanged from original)
	# ---------------------------------------------------------------------------

	original_order <- unique(df$parameter)

	if (!is.null(units_df)) {
		units_df <- units_df %>% dplyr::mutate(
			parameter = as.character(parameter),
			units     = as.character(units))
		df <- dplyr::left_join(df, units_df, by = "parameter")
	} else {
		df$units <- NA_character_
	}

	if (!is.null(aliases_df)) {
		aliases_df <- aliases_df %>% dplyr::mutate(
			parameter = as.character(parameter),
			aliases   = as.character(aliases))
		df <- dplyr::left_join(df, aliases_df, by = "parameter")
	} else {
		df$aliases <- NA_character_
	}

	df <- df %>%
		dplyr::mutate(
			parameter = as.character(parameter),
			aliases   = as.character(aliases),
			units     = as.character(units))

	df <- df %>%
		dplyr::mutate(display_name = dplyr::case_when(
			!is.na(aliases) & !is.na(units) & units != "" ~ paste0(aliases, " (", units, ")"),
			!is.na(aliases) ~ aliases,
			is.na(aliases) & !is.na(units) & units != "" ~ paste0(parameter, " (", units, ")"),
			TRUE ~ parameter))

	df <- df %>%
		dplyr::mutate(parameter = factor(parameter, levels = original_order)) %>%
		dplyr::arrange(parameter) %>%
		dplyr::relocate(display_name, .before = 1) %>%
		dplyr::select(-parameter, -units, -aliases) %>%
		dplyr::rename(parameter = display_name)

	return(df)
}

##################################################################################
#' .getDelim
#'
#' This helper function attempts to detect the delimiter used in a text file by reading
#' a small sample of rows and determining which of a specified set of delimiters yields the
#' highest number of columns. This function is useful for automatically identifying the correct
#' delimiter when reading data files with various formats.
#'
#' @param filename A string specifying the path to the file whose delimiter is to be detected.
#' @param seps A character vector of possible delimiters to test. The default is `c("\t", " ", ";", ",")`.
#' @param nrows An integer specifying the number of rows to read for testing the delimiter. Default is 50.
#'
#' @details
#' The function works by iteratively attempting to read a portion of the file (`nrows` rows) with
#' each delimiter specified in `seps`. It counts the number of columns produced by each delimiter
#' and selects the delimiter that yields the highest number of columns. This method assumes that
#' the correct delimiter will create more columns than incorrect ones.
#'
#' - **Max Columns Logic**: The function assumes that the delimiter yielding the maximum number
#'   of columns is the correct one. If the resulting column count is only 1 (indicating that the
#'   file could not be split into columns), the function outputs an error message and returns `NULL`.
#'
#' @return Returns the detected delimiter as a character if successful. If the file cannot be
#'         split into columns with any of the tested delimiters, returns `NULL` invisibly.
#'
#' @examples
#' # Example usage:
#' # sep <- .getDelim("datafile.txt")
#'
.getDelim <- function(filename, seps = c(",", "\t", ";"), nrows = 50) {
	nbCol <- NULL
	for (i in 1:length(seps)) {
		sep <- seps[i]
		df <- utils::read.csv(filename, sep = sep, nrows = nrows)
		nbCol <- c(nbCol, length(df))
	}
	iSep <- which(nbCol == max(nbCol))[1]
	sep <- seps[iSep]
	if (nbCol[iSep] == 1) {
		message(paste0("ERROR: Cannot read file ", filename, "."))
		return(invisible(NULL))
	}
	return(invisible(sep))
}


##################################################################################
#' summarizeStats
#'
#' Calculates comprehensive descriptive statistics for a numeric variable,
#' including arithmetic, geometric, and harmonic means, standard deviation,
#' coefficient of variation, quantiles, and percentiles. Supports grouped
#' summaries by one or more categorical variables.
#'
#' @param df A data frame containing the data to summarize.
#' @param group_vars Character vector of column names to group by. If `NULL`,
#'   statistics are calculated for the entire dataset without grouping.
#' @param value_col Character string specifying the name of the numeric column
#'   to summarize. Default is `"value"`.
#' @param id_col Character string specifying the name of the ID column for
#'   counting distinct subjects/individuals. If `NULL`, row count is used instead.
#'   Default is `NULL`.
#'
#' @details
#' This function computes a comprehensive set of summary statistics:
#' \itemize{
#'   \item **N**: Number of distinct IDs (if `id_col` provided) or row count
#'   \item **Arithmetic statistics**: mean, SD, CV (coefficient of variation as percentage)
#'   \item **Quantiles**: min, P05 (5th percentile), Q1, median, Q3, P95 (95th percentile), max
#'   \item **Geometric statistics**: geoMean, geoSD, geoCV (calculated on log scale)
#'   \item **Harmonic mean**: harmMean
#' }
#'
#' **Geometric Statistics**: Only positive, finite values are used. The geometric mean
#' is calculated as `exp(mean(log(x)))`, geometric SD as `exp(sd(log(x)))`, and
#' geometric CV as `100 * sqrt(exp(sd(log(x))^2) - 1)`.
#'
#' **Harmonic Mean**: Only positive, finite values are used. Calculated as
#' `n / sum(1/x)` where n is the count of valid values.
#'
#' @return A data frame with one row per group (or one row total if ungrouped)
#'   containing all calculated statistics as columns.
#'
#' @keywords internal
#'
#' @examples
#' # df_long <- data.frame(parameter = rep(c("AUC", "Cmax"), each = 10),
#' #                       value = c(rnorm(10, 100, 20), rnorm(10, 50, 10)),
#' #                       id = rep(1:10, 2))
#' # .summarizeStats(df_long, group_vars = "parameter", value_col = "value", id_col = "id")
#'
.summarizeStats <- function(df,
														group_vars = NULL,
														value_col = "value",
														id_col = NULL) {
	# Ensure the value column exists
	if (!(value_col %in% names(df))) {
		stop("Value column '", value_col, "' not found in input data.")
	}

	df %>%
		dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
		dplyr::summarise(
			N = if (!is.null(id_col)) dplyr::n_distinct(.data[[id_col]]) else dplyr::n(),
			min = min(.data[[value_col]], na.rm = TRUE),
			P05 = stats::quantile(.data[[value_col]], 0.05, na.rm = TRUE, type = 5),
			Q1 = stats::quantile(.data[[value_col]], 0.25, na.rm = TRUE, type = 5),
			mean = mean(.data[[value_col]], na.rm = TRUE),
			SD = stats::sd(.data[[value_col]], na.rm = TRUE),
			CV = if (is.na(mean) || mean == 0) NA_real_ else 100 * SD / mean,
			median = stats::median(.data[[value_col]], na.rm = TRUE),
			Q3 = stats::quantile(.data[[value_col]], 0.75, na.rm = TRUE, type = 5),
			P95 = stats::quantile(.data[[value_col]], 0.95, na.rm = TRUE, type = 5),
			max = max(.data[[value_col]], na.rm = TRUE),

			# geometric mean / sd / cv
			geoMean = {
				mask <- is.finite(.data[[value_col]]) & .data[[value_col]] > 0
				if (sum(mask) > 0) exp(mean(log(.data[[value_col]][mask]), na.rm = TRUE)) else NA_real_
			},
			geoSD = {
				mask <- is.finite(.data[[value_col]]) & .data[[value_col]] > 0
				sdlog <- if (sum(mask) > 1) stats::sd(log(.data[[value_col]][mask]), na.rm = TRUE) else NA_real_
				if (is.na(sdlog)) NA_real_ else exp(sdlog)
			},
			geoCV = {
				mask <- is.finite(.data[[value_col]]) & .data[[value_col]] > 0
				sdlog <- if (sum(mask) > 1) stats::sd(log(.data[[value_col]][mask]), na.rm = TRUE) else NA_real_
				if (is.na(sdlog)) NA_real_ else 100 * sqrt(exp(sdlog^2) - 1)
			},

			# harmonic mean
			harmMean = {
				mask <- is.finite(.data[[value_col]]) & .data[[value_col]] > 0
				if (any(mask, na.rm = TRUE)) length(.data[[value_col]][mask]) / sum(1 / .data[[value_col]][mask]) else NA_real_
			},
			.groups = "drop"
		)
}

##################################################################################
#' Calculate Descriptive Statistics for Longitudinal Data
#'
#' @description
#' A simple internal helper that calculates descriptive statistics from
#' longitudinal data. Designed to be reusable across multiple table functions
#' (tableNCAConcentration, tableNCAParameters, tableNCARatios, tableContinuousCovariateSummary).
#'
#' This function calculates raw numeric statistics only. Rounding and combined
#' metrics (MeanSD, GeomCV, etc.) should be applied by the calling function.
#'
#' @param data Data frame in long format containing at minimum an ID column and a value column.
#' @param value_col Character string specifying the name of the value column to summarize.
#' @param id_col Character string specifying the name of the ID/subject column.
#'   When `NULL`, SE is not computed. Occasion-invariant deflation (if wanted) is
#'   done by the caller before calling this function.
#' @param group_by Character vector or NULL specifying column names to group by.
#'   Examples: `c("Time", "SEX")`, `c("Parameter")`, `NULL` for no grouping.
#'
#' @return A data frame with grouping columns (if any) and the following metrics:
#'   N (pooled population), Nobs (non-missing), min, P05, Q1, mean, SD, SE, CV,
#'   median, Q3, P95, max, geoMean, geoSD, geoCV, harmMean
#'
#' @keywords internal
#'
.calculateDescriptiveStats <- function(data,
																			 value_col,
																			 id_col,
																			 group_by = NULL) {


	# Validate inputs
	if (!(value_col %in% names(data))) {
		stop("Value column '", value_col, "' not found in input data.")
	}
	if (!is.null(id_col) && !(id_col %in% names(data))) {
		stop("ID column '", id_col, "' not found in input data.")
	}
	if (!is.null(group_by)) {
		missing_cols <- setdiff(group_by, names(data))
		if (length(missing_cols) > 0) {
			stop("Grouping columns not found in data: ", paste(missing_cols, collapse = ", "))
		}
	}

	# N = pooled population (NTOT); Nobs = non-missing (NOBS).
	stats <- data %>%
		dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) %>%
		dplyr::summarise(
			N    = dplyr::n(),
			Nobs = sum(!is.na(.data[[value_col]])),
			min = { v <- .data[[value_col]]; if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE) },
			P05 = stats::quantile(.data[[value_col]], 0.05, na.rm = TRUE, type = 5),
			Q1 = stats::quantile(.data[[value_col]], 0.25, na.rm = TRUE, type = 5),
			mean = mean(.data[[value_col]], na.rm = TRUE),
			SD = stats::sd(.data[[value_col]], na.rm = TRUE),
			CV = {
				m <- mean(.data[[value_col]], na.rm = TRUE)
				s <- stats::sd(.data[[value_col]], na.rm = TRUE)
				if (!is.na(m) && m != 0 && !is.na(s)) 100 * s / m else NA_real_
			},
			median = stats::median(.data[[value_col]], na.rm = TRUE),
			Q3 = stats::quantile(.data[[value_col]], 0.75, na.rm = TRUE, type = 5),
			P95 = stats::quantile(.data[[value_col]], 0.95, na.rm = TRUE, type = 5),
			max = { v <- .data[[value_col]]; if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) },
			geoMean = {
				vals <- .data[[value_col]][.data[[value_col]] > 0 & is.finite(.data[[value_col]])]
				if (length(vals) > 0) exp(mean(log(vals))) else NA_real_
			},
			geoSD = {
				vals <- .data[[value_col]][.data[[value_col]] > 0 & is.finite(.data[[value_col]])]
				if (length(vals) > 1) exp(stats::sd(log(vals))) else NA_real_
			},
			geoCV = {
				vals <- .data[[value_col]][.data[[value_col]] > 0 & is.finite(.data[[value_col]])]
				sdlog <- if (length(vals) > 1) stats::sd(log(vals)) else NA_real_
				if (!is.na(sdlog)) 100 * sqrt(exp(sdlog^2) - 1) else NA_real_
			},
			harmMean = {
				vals <- .data[[value_col]][.data[[value_col]] > 0 & is.finite(.data[[value_col]])]
				if (length(vals) > 0) length(vals) / sum(1 / vals) else NA_real_
			},
			.groups = "drop"
		) %>%
		as.data.frame()

	# SE from the non-missing count (NA when no ID column is given).
	out <- stats
	out$SE <- if (is.null(id_col)) {
		NA_real_
	} else {
		ifelse(!is.na(out$SD) & out$Nobs > 0, out$SD / sqrt(out$Nobs), NA_real_)
	}

	out %>%
		dplyr::relocate("SE", .after = "SD") %>%
		as.data.frame()
}

##################################################################################
#' combinedStats
#'
#' Creates combined display metrics by concatenating mean with SD and geometric
#' mean with geometric CV or SD. This formatting is commonly used in pharmacokinetic
#' summary tables to present central tendency and variability together.
#'
#' @param df A data frame containing columns named `mean`, `SD`, `geoMean`,
#'   `geoCV`, and `geoSD`. Typically the output from `.summarizeStats()`.
#'
#' @details
#' The function creates three new combined columns:
#' \itemize{
#'   \item **MeanSD**: Formatted as "mean (SD)"
#'   \item **GeomCV**: Formatted as "geoMean (geoCV)"
#'   \item **GeomSD**: Formatted as "geoMean (geoSD)"
#' }
#'
#' These combined metrics are useful for creating compact summary tables where
#' both central tendency and variability need to be displayed together.
#'
#' @return The input data frame with three additional columns: `MeanSD`, `GeomCV`,
#'   and `GeomSD`.
#'
#' @keywords internal
#'
#' @examples
#' # df <- data.frame(mean = 100, SD = 20, geoMean = 95, geoCV = 18, geoSD = 1.2)
#' # .combinedStats(df)
#' # Result: adds columns MeanSD = "100 (20)", GeomCV = "95 (18)", GeomSD = "95 (1.2)"
#'
.combinedStats <- function(df) {
	df %>%
		dplyr::mutate(
			MeanSD = paste0(.data$mean, " (", .data$SD, ")"),
			MeanCV = paste0(.data$mean, " (", .data$CV, ")"),
			GeomCV = paste0(.data$geoMean, " (", .data$geoCV, ")"),
			GeomSD = paste0(.data$geoMean, " (", .data$geoSD, ")"))
}

##################################################################################
#' fixNCAConcentrationsColumnSwap
#'
#' Corrects the column order issue in \code{getNCAConcentrations()} output where
#' covariate columns appear with swapped/reversed content. The function detects
#' when covariate column headers are in the correct order but their data content
#' is swapped, and restores the proper alignment between column
#' names and their corresponding data.
#'
#' @param df Data frame returned by \code{getNCAConcentrations(displayIndividualProfiles = TRUE)}
#'   that exhibits the column swapping issue.
#' @param originalCovariates Character vector of covariate names in their intended
#'   order, as originally passed to \code{setNCAResultsStratification(split = splitCovariates)}.
#'   Must match the order used when setting up the stratification.
#'
#' @details This function corrects a known bug where \code{getNCAConcentrations()}
#'   with \code{displayIndividualProfiles = TRUE} returns covariate columns with
#'   correct headers but swapped content. When the number of splitCovariates is even,
#'   all splitCovariates are affected in reverse order. When odd, the middle covariate
#'   remains correct while others are mirrored around it. The function identifies
#'   covariate columns by name, reverses their order, and applies the original
#'   intended column names to restore proper data alignment.
#'
#' @return A data frame with the same structure as the input but with covariate
#'   columns correctly ordered and named according to \code{originalCovariates}.
#'   Non-covariate columns (subject, time points, concentrations) remain unchanged.
#'
#' @examples
#' #  <- c("DOSE", "FOOD", "VISIT")
#' # setNCAResultsStratification(split = splitCovariates)
#' # df <- getNCAConcentrations(displayIndividualProfiles = TRUE)
#' # df_fixed <- fixNCAConcentrationsColumnSwap(df, splitCovariates)
.fixNCAConcentrationsColumnSwap <- function(df, originalCovariates) {
	# Get the number of splitCovariates
	n_cov <- length(originalCovariates)

	# If no splitCovariates or only one, no swapping occurs
	if (n_cov <= 1) {
		return(df)
	}

	# Find covariate columns in the dataframe
	cov_cols <- names(df)[names(df) %in% originalCovariates]

	# Find non-covariate columns
	non_cov_cols <- names(df)[!names(df) %in% originalCovariates]

	# Create the corrected column order
	# The splitCovariates appear to be reversed in the output
	corrected_cov_order <- rev(cov_cols)

	# Reorder the dataframe with corrected covariate order
	df_fixed <- df[, c(corrected_cov_order, non_cov_cols)]

	# Rename columns to match the original intended order
	names(df_fixed)[1:n_cov] <- originalCovariates

	return(df_fixed)
}

##################################################################################

#-------------------------------------------------------------------------------
#' Validate Analysis Results Availability
#'
#' Checks if required analysis results are available in the currently loaded project.
#' Throws an error with an informative message if results are missing.
#'
#' @param software Character string specifying the software. One of "monolix" or "pkanalix".
#' @param resultType Character string specifying the type of results to validate.
#'   For PKanalix: "nca" (NCA parameters/concentrations), "ncaRatio" (NCA ratios),
#'   "ca" (compartmental analysis), or "be" (bioequivalence).
#'   For Monolix: "pop" (population parameters).
#'
#' @return Invisible NULL if validation passes. Stops with an error if results are unavailable.
#'
#' @details
#' For BE and ratio analyses, also validates that prerequisite NCA results exist.
#' BE and ratio analyses require NCA to be run first.
#'
#' @keywords internal
#' @noRd
.validateProjectResultsAvailability <- function(software   = c("monolix", "pkanalix"),
																								resultType = c("pop", "rse", "bootstrap", "nca", "ncaRatio", "be", "ca"),
																								method     = "") {
	resultType <- match.arg(resultType)

	if (software == "pkanalix") {
		if (resultType %in% c("nca")) {
			result    <- tryCatch(lixoftConnectors::getNCAIndividualParameters()$parameters, error = function(e) NULL)
			error_msg <- "No NCA results available. Please run non-compartmental analysis first."

		} else if (resultType == "ncaRatio") {
			nca_result <- tryCatch(lixoftConnectors::getNCAIndividualParameters(), error = function(e) NULL)

			if (is.null(nca_result) || isFALSE(nca_result)) {
				stop("No NCA results available. Please run non-compartmental analysis (required for NCA ratios).", call. = FALSE)
			}
			result    <- tryCatch(lixoftConnectors::getNCAIndividualRatios(), error = function(e) NULL)
			error_msg <- "No NCA ratios available. Please configure NCA ratio module before running NCA."

		} else if (resultType == "ca") {
			result    <- tryCatch(lixoftConnectors::getCAIndividualParameters(), error = function(e) NULL)
			error_msg <- "No CA results available. Please run compartmental analysis first."

		} else if (resultType == "be") {
			# BE depends on NCA - check NCA first
			nca_result <- tryCatch(lixoftConnectors::getNCAIndividualParameters(), error = function(e) NULL)

			if (is.null(nca_result) || isFALSE(nca_result)) {
				stop("No NCA results available. Please run non-compartmental analysis (required for BE analysis).", call. = FALSE)
			}
			be_result <- tryCatch(lixoftConnectors::getBioequivalenceResults()$confidenceIntervals, error = function(e) NULL)

			if (is.null(be_result) || isFALSE(be_result)) {
				stop("No BE results available. Please run bioequivalence analysis first.", call. = FALSE)
			}
			return(invisible(NULL))
		}

	} else if (software == "monolix") {
		if (resultType == "pop") {
			result    <- tryCatch(lixoftConnectors::getEstimatedPopulationParameters(), error = function(e) NULL)
			error_msg <- "No population parameter estimates available. Please run SAEM first."

		} else if (resultType == "rse") {
			# Get all available SE results
			all_rse <- tryCatch(lixoftConnectors::getEstimatedStandardErrors(), error = function(e) NULL)

			# Case 1: No RSE results at all
			if (is.null(all_rse) || length(all_rse) == 0) {
				stop("No standard error estimates available. Please run standard error estimation first.", call. = FALSE)
			}

			# Case 2: Specific method requested
			if (method != "") {
				available_methods <- names(all_rse)
				if (!(method %in% available_methods)) {
					stop(paste0(
						"Standard errors for method '", method, "' not available. ",
						"Available methods: ", paste(available_methods, collapse = ", "), "."), call. = FALSE)
				}
			}
			return(invisible(NULL))

		} else if (resultType == "bootstrap") {
			result <- tryCatch(lixoftConnectors::getBootstrapResults()$populationSummary, error = function(e) NULL)
			error_msg <- "No bootstrap results available. Please run bootstrap analysis first."
		}
	}

	# Generic validation for non-rse cases
	if (is.null(result) || isFALSE(result)) {
		stop(error_msg, call. = FALSE)
	}

	return(invisible(NULL))
}


##################################################################################
#' Internal helper to validate and select metrics
#'
#' @param available_cols Character vector of all column names in the stats dataframe.
#' @param user_metrics Character vector of metrics requested by the user (can be NULL).
#' @param descriptors Character vector of column names that are descriptors (not metrics).
#'
#' @return A character vector of valid selected metrics.
#'
.validateAndSelectMetrics <- function(available_cols, user_metrics, descriptors) {
	# Determine what are actual metrics by excluding descriptors
	available_metrics_list <- setdiff(available_cols, descriptors)

	if (is.null(user_metrics)) {
		# User did not specify metrics -> Return ALL available
		message("No metrics specified. Using all available metrics.")
		return(available_metrics_list)
	} else {
		# User specified metrics
		missing_metrics <- setdiff(user_metrics, available_metrics_list)
		valid_metrics <- intersect(user_metrics, available_metrics_list)

		# Error if NO valid metrics found
		if (length(valid_metrics) == 0) {
			stop(paste0(
				"None of the requested metrics were found.\n",
				"Requested: ", paste(user_metrics, collapse = ", "), "\n",
				"Available metrics: ", paste(available_metrics_list, collapse = ", ")
			))
		}

		# Message if some metrics are missing or wrong
		if (length(missing_metrics) > 0) {
			message(paste0(
				"Ignoring unknown metrics: ", paste(missing_metrics, collapse = ", "),
				".\nAvailable metrics: ", paste(available_metrics_list, collapse = ", ")
			))
		}

		return(valid_metrics)
	}
}

##################################################################################
#' Validate selected splitCovariates for NCA tables and listings
#'
#' @description
#' Checks if the provided splitCovariates exist in the currently loaded PKanalix project
#' and are of valid types. For ratio tables, only categorical splitCovariates are allowed.
#' For other tables/listings, both categorical splitCovariates and occasions are allowed.
#'
#' @param splitCovariates Character vector of covariate names to validate, or NULL
#' @param ratio Logical; if TRUE, validates for ratio tables (only categorical splitCovariates allowed
#'   via getCovariateInformation()). If FALSE (default), allows both categorical splitCovariates
#'   and occasions (via getData()).
#'
#' @return Invisible NULL if validation passes. Stops with error if validation fails.
#'
#' @details
#' When ratio = FALSE:
#'   - Uses lixoftConnectors::getData()
#'   - Allows headerTypes: "catcov" (categorical covariate) or "occ" (occasion)
#'   - Used in: listingNCAConcentration, tableNCAConcentration, tableNCAParameters, etc.
#'
#' When ratio = TRUE:
#'   - Uses lixoftConnectors::getCovariateInformation()
#'   - Allows only categorical splitCovariates (type == "categorical")
#'   - Does NOT allow occasions
#'   - Used in: tableNCARatios, listingNCARatios
#'
#' @noRd

.validateCovariateSelection <- function(splitCovariates, ratio = FALSE) {
	# Skip validation if no splitCovariates provided
	if (is.null(splitCovariates)) {
		return(invisible(NULL))
	}

	if (ratio) {
		project_data   <- lixoftConnectors::getData()
		available_covs <- project_data$header[project_data$headerTypes %in% c("catcov", "stratcatcov")]
		cov_info       <- lixoftConnectors::getCovariateInformation()
		available_covs <- c(available_covs, cov_info$name[cov_info$type == "additionalstratificationcategorical"])
		context_msg    <- "splitCovariates in ratio results (categorical or stratification categorical; occasions not allowed for ratios)"

		.assertOccasionInvariantForRatio(splitCovariates, cov_info = cov_info, arg_name = "splitCovariates")
	} else {
		project_data   <- lixoftConnectors::getData()
		available_covs <- project_data$header[project_data$headerTypes %in% c("catcov", "occ", "stratcatcov")]
		cov_info       <- lixoftConnectors::getCovariateInformation()
		available_covs <- c(available_covs, cov_info$name[cov_info$type == "additionalstratificationcategorical"])
		context_msg    <- "splitCovariates (catcov, occ, or stratification categorical)"
	}

	# Check if provided splitCovariates are valid
	invalid_covs <- setdiff(splitCovariates, available_covs)

	if (length(invalid_covs) > 0) {
		stop(sprintf(
			"Invalid covariate%s: %s\nAvailable %s: %s",
			if (length(invalid_covs) > 1) "s" else "",
			paste(invalid_covs, collapse = ", "),
			context_msg,
			if (length(available_covs) > 0) paste(available_covs, collapse = ", ") else "none"
		), call. = FALSE)
	}

	return(invisible(NULL))
}

################################################################################
# Filter Helpers for PKanalix Tables and Listings
################################################################################

#' Validate Filter Arguments
#'
#' Internal helper to validate filterCovariates and filterIds arguments.
#' Checks that filter splitCovariates are categorical (catcov or occ) based on
#' the resultType. Does NOT validate that filter values exist in data
#' (that is handled by .applyFilters()).
#'
#' @param filterCovariates Named list where names are categorical covariate names
#'   and values are vectors of allowed values. Example:
#'   `list(DOSE = c("100mg", "200mg"), SEX = "Male")`. If NULL, no covariate
#'   filtering is applied.
#' @param filterIds Character or numeric vector of subject IDs to include.
#'   If NULL, no ID filtering is applied.
#' @param resultType Character string specifying the result type context.
#'   Determines which splitCovariates are valid for filtering:
#'   \itemize{
#'     \item "nca": NCA parameters and concentrations - uses catcov and occ from getData()
#'     \item "ncaRatio": NCA ratios - uses intersection of ratio columns and data variables
#'   }
#'
#' @return Returns invisible(NULL) on success. Stops with error if validation fails.
#'
#' @details
#' Validation checks performed:
#' \itemize{
#'   \item filterCovariates must be a named list (or NULL)
#'   \item Filter covariate names must be categorical splitCovariates (catcov or occ)
#'   \item filterIds must be a vector (or NULL)
#' }
#'
#' Note: This function does NOT validate that filter values exist in the data.
#' Invalid values will simply result in no rows matching (handled by .applyFilters()).
#'
#' @keywords internal
#'
.validateFilters <- function(filterCovariates = NULL,
                             filterIds        = NULL,
                             resultType       = c("nca", "ncaRatio", "covariateSummary")) {

	resultType <- match.arg(resultType)

	# Early return if no filters specified
	if (is.null(filterCovariates) && is.null(filterIds)) {
		return(invisible(NULL))
	}

	# --------------------------------------------------------------------------
	# Validate filterCovariates
	# --------------------------------------------------------------------------
	if (!is.null(filterCovariates)) {

		# Must be a named list
		if (!is.list(filterCovariates) || is.null(names(filterCovariates))) {
			stop("filterCovariates must be a named list. Example: list(DOSE = c('100mg', '200mg'), SEX = 'Male')",
					 call. = FALSE)
		}

		# Check for empty names
		if (any(names(filterCovariates) == "")) {
			stop("All elements in filterCovariates must be named.", call. = FALSE)
		}

		# Get available categorical splitCovariates based on resultType
		if (resultType == "ncaRatio") {
			project_data       <- lixoftConnectors::getData()
			available_cat_covs <- project_data$header[project_data$headerTypes %in% c("catcov", "stratcatcov")]
			cov_info_filt      <- lixoftConnectors::getCovariateInformation()
			available_cat_covs <- c(available_cat_covs, cov_info_filt$name[cov_info_filt$type == "additionalstratificationcategorical"])

			.assertOccasionInvariantForRatio(names(filterCovariates), cov_info = cov_info_filt, arg_name = "filterCovariates")
		} else if (resultType == "covariateSummary") {
			cov_info_filt      <- lixoftConnectors::getCovariateInformation()
			cat_types          <- c("categorical", "additionalstratificationcategorical", "stratificationcategorical")
			available_cat_covs <- cov_info_filt$name[cov_info_filt$type %in% cat_types]
			# allow occasion index columns (in $covariate but not in $name) as filter keys
			occ_cols           <- setdiff(names(cov_info_filt$covariate), c("id", cov_info_filt$name))
			available_cat_covs <- c(available_cat_covs, occ_cols)
		} else {
			project_data       <- lixoftConnectors::getData()
			available_cat_covs <- project_data$header[project_data$headerTypes %in% c("catcov", "occ", "stratcatcov")]
			cov_info_filt      <- lixoftConnectors::getCovariateInformation()
			available_cat_covs <- c(available_cat_covs,
															cov_info_filt$name[cov_info_filt$type == "additionalstratificationcategorical"])
		}

		# Check that filter splitCovariates are valid categorical splitCovariates
		filter_covs  <- names(filterCovariates)
		invalid_covs <- setdiff(filter_covs, available_cat_covs)

		if (length(invalid_covs) > 0) {
			# Check if they're continuous splitCovariates (more specific error message)
			if (resultType == "covariateSummary") {
				cov_info_filt <- lixoftConnectors::getCovariateInformation()
				cont_types    <- c("continuous",
													 "additionalstratificationcontinuous",
													 "stratificationcontinuous")
				cont_covs     <- cov_info_filt$name[cov_info_filt$type %in% cont_types]
			} else {
				project_data <- lixoftConnectors::getData()
				cont_covs    <- project_data$header[project_data$headerTypes == "contcov"]
			}
			invalid_cont <- intersect(invalid_covs, cont_covs)

			if (length(invalid_cont) > 0) {
				stop(sprintf(
					"filterCovariates can only filter categorical covariates.\n'%s' %s continuous covariate%s and cannot be filtered.\nAvailable categorical covariates: %s",
					paste(invalid_cont, collapse = "', '"),
					if (length(invalid_cont) > 1) "are" else "is a",
					if (length(invalid_cont) > 1) "s" else "",
					if (length(available_cat_covs) > 0) paste(available_cat_covs, collapse = ", ") else "none"), call. = FALSE)
			} else {
				stop(sprintf(
					"Invalid covariate%s in filterCovariates: %s\nAvailable categorical covariates: %s",
					if (length(invalid_covs) > 1) "s" else "",
					paste(invalid_covs, collapse = ", "),
					if (length(available_cat_covs) > 0) paste(available_cat_covs, collapse = ", ") else "none"), call. = FALSE)
			}
		}
	}

	# --------------------------------------------------------------------------
	# Validate filterIds
	# --------------------------------------------------------------------------
	if (!is.null(filterIds)) {

		# Must be a vector (character or numeric)
		if (!is.vector(filterIds) || is.list(filterIds)) {
			stop("filterIds must be a character or numeric vector of IDs.", call. = FALSE)
		}

		# Check it's not empty
		if (length(filterIds) == 0) {
			stop("filterIds cannot be an empty vector.", call. = FALSE)
		}
	}

	return(invisible(NULL))
}


################################################################################
#' Apply Filters to Data
#'
#' Internal helper to apply covariate and ID filters to a dataframe.
#'
#' @param data Data frame to filter.
#' @param filterCovariates Named list where names are categorical covariate names
#'   and values are vectors of allowed values. Example:
#'   `list(DOSE = c("100mg", "200mg"), SEX = "Male")`. If NULL, no covariate
#'   filtering is applied.
#' @param filterIds Character or numeric vector of subject IDs to include.
#'   If NULL, no ID filtering is applied.
#' @param id_col Character string specifying the name of the ID column.
#'   Default is "id".
#'
#' @return Filtered data frame. Warns if the result is empty.
#'
#' @details
#' Filters are applied in the following order:
#' \enumerate{
#'   \item ID filter (if specified)
#'   \item Covariate filters (if specified) - applied sequentially
#' }
#'
#' The function assumes filters have been validated by `.validateFilters()`.
#' If filter values don't exist in the data, those conditions simply won't
#' match any rows (no error is thrown).
#'
#' @keywords internal
#'
.applyFilters <- function(data,
                          filterCovariates = NULL,
                          filterIds        = NULL,
                          id_col           = "id") {

	# Early return if no filters specified
	if (is.null(filterCovariates) && is.null(filterIds)) {
		return(data)
	}

	# Store original row count for messaging
	n_original <- nrow(data)

	# --------------------------------------------------------------------------
	# Apply ID filter
	# --------------------------------------------------------------------------
	if (!is.null(filterIds)) {
		if (id_col %in% names(data)) {
			# Convert both to character for consistent comparison
			data_ids <- as.character(data[[id_col]])
			filter_ids <- as.character(filterIds)

			data <- data[data_ids %in% filter_ids, , drop = FALSE]
		} else {
			warning(sprintf("ID column '%s' not found in data. ID filter not applied.", id_col),
							call. = FALSE)
		}
	}

	# --------------------------------------------------------------------------
	# Apply covariate filters
	# --------------------------------------------------------------------------
	if (!is.null(filterCovariates)) {
		for (cov_name in names(filterCovariates)) {
			if (cov_name %in% names(data)) {
				filter_values <- filterCovariates[[cov_name]]

				# Convert both to character for consistent comparison
				data_values <- as.character(data[[cov_name]])
				filter_values_char <- as.character(filter_values)

				data <- data[data_values %in% filter_values_char, , drop = FALSE]
			} else {
				warning(sprintf("Covariate '%s' not found in data. Filter not applied for this covariate.",
												cov_name), call. = FALSE)
			}
		}
	}

	# --------------------------------------------------------------------------
	# Check for empty result
	# --------------------------------------------------------------------------
	n_filtered <- nrow(data)

	if (n_filtered == 0) {
		warning(sprintf(
			"Filter resulted in empty dataset (0 of %d rows remaining).\nCheck your filterCovariates and filterIds values.",
			n_original
		), call. = FALSE)
	}

	return(data)
}

##################################################################################
#' Internal helper to validate stratification arguments
#'
#' @param splitCovariates Vector of covariate names (or NULL).
#' @param stratifyColumns Boolean flag indicating if stratification is requested.
#' @param allowed_types Optional character vector of allowed `getCovariateInformation()$type`
#'   values (e.g. `c("categorical", "additionalstratificationcategorical", "stratificationcategorical")`).
#'   If `NULL` (default), only existence of `splitCovariates` is checked. If provided,
#'   each split covariate must additionally have a `type` in `allowed_types`.
#' @param allow_occasions Logical; if TRUE, occasion index columns (columns in
#'   `cov_info$covariate` that are not `id` and not in `cov_info$name`) are also
#'   accepted as split covariates, even though they are not in `allowed_types`.
#'
#' @return Boolean. Returns FALSE if splitCovariates are missing, otherwise returns original stratifyColumns.
#'
.validateStratificationArgs <- function(splitCovariates, stratifyColumns,
																				allowed_types = NULL, allow_occasions = FALSE) {
	# 1. Argument consistency check
	if (isTRUE(stratifyColumns) && is.null(splitCovariates)) {
		warning("stratifyColumns=TRUE requires covariates. Resetting stratifyColumns to FALSE.")
		return(FALSE)
	}
	# 2. Covariate existence check (if splitCovariates provided)
	if (!is.null(splitCovariates)) {
		cov_info                  <- lixoftConnectors::getCovariateInformation()
		available_splitCovariates <- setdiff(names(cov_info$covariate), "id")

		bad_splitCovariates <- setdiff(splitCovariates, available_splitCovariates)
		if (length(bad_splitCovariates) > 0) {
			stop(
				"Invalid covariate(s): ", paste(bad_splitCovariates, collapse = ", "), "\n",
				"Available splitCovariates: ", paste(available_splitCovariates, collapse = ", ")
			)
		}

		# 3. Optional type check
		if (!is.null(allowed_types)) {
			valid_covs <- cov_info$name[cov_info$type %in% allowed_types]
			if (isTRUE(allow_occasions)) {
				# occasion index columns: present in $covariate but not a covariate name
				occ_cols   <- setdiff(names(cov_info$covariate), c("id", cov_info$name))
				valid_covs <- c(valid_covs, occ_cols)
			}
			bad_type <- setdiff(splitCovariates, valid_covs)
			if (length(bad_type) > 0) {
				stop(
					"Covariate(s) with invalid type for stratification: ",
					paste(bad_type, collapse = ", "), "\n",
					"Allowed types: ", paste(allowed_types, collapse = ", "),
					if (isTRUE(allow_occasions)) " (plus occasion variables)" else "", "\n",
					"Available covariates of allowed type(s): ",
					if (length(valid_covs) > 0) paste(valid_covs, collapse = ", ") else "none",
					call. = FALSE
				)
			}
		}
	}
	return(stratifyColumns)
}

##################################################################################
#' .getOccasionInvariantCovariates()
#' Classify covariates as occasion-invariant or occasion-varying
#'
#' Inspects \code{getCovariateInformation()$covariate} (one row per subject and
#' occasion) and determines, for each requested covariate, whether it keeps a
#' single value within every subject (occasion-invariant, e.g. WEIGHT, BMI, a
#' treatment-sequence covariate) or changes between a subject's occasions
#' (occasion-varying, e.g. TRT or a per-period covariate).
#'
#' This distinction matters for NCA ratio tables: ratios are computed within a
#' subject across occasions, so only occasion-invariant covariates have an
#' unambiguous single value per subject and can be safely merged onto / used to
#' stratify or filter the ratio results. Occasion-varying covariates would both
#' duplicate rows on an id-only merge and have no well-defined value per ratio.
#'
#' @param cov_info Optional result of \code{lixoftConnectors::getCovariateInformation()}.
#'   Fetched automatically when \code{NULL}.
#' @param covariates Optional character vector restricting the covariates to
#'   classify. When \code{NULL}, all covariates in \code{cov_info$name} are used.
#' @param group_by Optional character vector of extra columns to add to the
#'   grouping key alongside `id`. NULL (default) = constancy within `id` only.
#' @return A list with two character vectors: \code{invariant} (constant within
#'   each subject) and \code{varying} (changes across a subject's occasions).
#'
#' @keywords internal
#' @noRd
.getOccasionInvariantCovariates <- function(cov_info = NULL, covariates = NULL, group_by = NULL) {
	if (is.null(cov_info)) {
		cov_info <- lixoftConnectors::getCovariateInformation()
	}

	cov_df <- cov_info$covariate

	candidates <- cov_info$name
	if (!is.null(covariates)) {
		candidates <- intersect(candidates, covariates)
	}
	candidates <- intersect(candidates, names(cov_df))

	# Grouping key: id, plus any extra columns (e.g. current splitCovariates).
	group_by <- intersect(group_by, names(cov_df))
	key_cols <- c("id", group_by)
	key      <- do.call(paste, c(lapply(key_cols, function(cc) as.character(cov_df[[cc]])),
															 sep = "\r"))

	invariant <- character(0)
	varying   <- character(0)

	for (nm in candidates) {
		# Count distinct (non-NA) values within each grouping key.
		n_distinct_per_key <- tapply(as.character(cov_df[[nm]]), key,
																 function(x) length(unique(x[!is.na(x)])))

		if (all(n_distinct_per_key <= 1, na.rm = TRUE)) {
			invariant <- c(invariant, nm)
		} else {
			varying <- c(varying, nm)
		}
	}

	list(invariant = invariant, varying = varying)
}

##################################################################################

#' .getCovariateOccasionDependency()
#' Find the occasion variable(s) a covariate varies along
#'
#' For each requested covariate, decide whether it is constant within every
#' (id + `group_by`) combination (delegated to `.getOccasionInvariantCovariates`).
#' If not, find which occasion index column(s) make it constant when added to the
#' grouping — i.e. the occasion variable(s) the user should add to
#' `splitCovariates` to obtain a well-defined table.
#'
#' Occasion index columns are taken to be the columns of `cov_info$covariate`
#' that are neither `id` nor a covariate name (`cov_info$name`).
#'
#' @param cov_info Optional `getCovariateInformation()` result (fetched when NULL).
#' @param covariates Character vector of covariate names to test. NULL = all.
#' @param group_by Character vector of columns already used to stratify
#'   (typically the current `splitCovariates`); occasions already listed here are
#'   not re-reported. Default NULL.
#'
#' @return Named list (one entry per varying covariate). Each entry is a
#'   character vector of occasion column(s) the covariate varies along given
#'   `group_by`. Covariates already constant within `id + group_by` are omitted.
#'
#' @keywords internal
#' @noRd
.getCovariateOccasionDependency <- function(cov_info = NULL, covariates = NULL, group_by = NULL) {
	if (is.null(cov_info)) {
		cov_info <- lixoftConnectors::getCovariateInformation()
	}

	occ_cols      <- setdiff(names(cov_info$covariate), c("id", cov_info$name))
	candidate_occ <- setdiff(occ_cols, group_by)

	# Which requested covariates still vary within (id + group_by)?
	varying <- .getOccasionInvariantCovariates(cov_info, covariates = covariates,
																						 group_by = group_by)$varying

	result <- list()
	for (nm in varying) {
		# Occasion column(s) that make `nm` constant within (id + group_by + occ).
		resolving <- character(0)
		for (occ in candidate_occ) {
			is_invariant <- nm %in% .getOccasionInvariantCovariates(
				cov_info, covariates = nm, group_by = c(group_by, occ))$invariant
			if (is_invariant) resolving <- c(resolving, occ)
		}
		# If no single occasion resolves it, the combination of all remaining
		# occasion columns is required.
		if (length(resolving) == 0 && length(candidate_occ) > 0) {
			resolving <- candidate_occ
		}
		result[[nm]] <- resolving
	}

	result
}

##################################################################################

#' .assertOccasionInvariantForRatio()
#' Assert that covariates are occasion-invariant for ratio results
#'
#' Stops with an informative error when any of the requested covariates changes
#' between a subject's occasions. Used by the ratio-table validation helpers so
#' that occasion-varying covariates cannot be used to stratify or filter NCA
#' ratio tables (where they have no single value per subject).
#'
#' @param covariates Character vector of covariate names (or NULL).
#' @param cov_info Optional \code{getCovariateInformation()} result (fetched when NULL).
#' @param arg_name Name of the calling argument, used in the error message.
#'
#' @return Invisible NULL when all covariates are occasion-invariant.
#'
#' @keywords internal
#' @noRd
.assertOccasionInvariantForRatio <- function(covariates, cov_info = NULL, arg_name = "splitCovariates") {
	if (is.null(covariates) || length(covariates) == 0) {
		return(invisible(NULL))
	}

	cls <- .getOccasionInvariantCovariates(cov_info = cov_info, covariates = covariates)

	if (length(cls$varying) > 0) {
		stop(sprintf(
			paste0(
				"Occasion-varying covariate(s) cannot be used for %s in ratio results: %s\n",
				"Use a covariate that is constant within each subject."),
			arg_name,
			paste(cls$varying, collapse = ", ")),
			call. = FALSE)
	}

	invisible(NULL)
}

##################################################################################
#' Internal helper to relabel split-covariate columns in non-stratified tables
#'
#' @description
#' When `stratifyColumns = FALSE`, the split covariates appear as literal columns
#' (left-side columns in wide format, leftmost columns in long format). This
#' helper renames those columns from their raw covariate names to the validated
#' `splitCovariateLabels`, so the same labels drive both the stratified header
#' text and the non-stratified column headers. Columns that are absent (e.g.
#' reshaped away) or whose label equals the raw name are skipped.
#'
#' @param df Data frame whose covariate columns should be relabeled.
#' @param splitCovariates Character vector of raw covariate column names.
#' @param splitCovariate_labels Character vector of labels aligned to
#'   `splitCovariates` (as returned by `.validateCovariateLabels`).
#'
#' @return `df` with covariate columns renamed where a distinct label is given.
#'
#' @keywords internal
#' @noRd
.applyCovariateColumnLabels <- function(df, splitCovariates, splitCovariate_labels) {
	if (is.null(splitCovariates) || is.null(splitCovariate_labels)) {
		return(df)
	}

	# dplyr::rename() wants new_name = old_name, so names() are the labels
	rename_map <- stats::setNames(splitCovariates, splitCovariate_labels)

	# keep only columns still present and whose label actually differs from the raw name
	rename_map <- rename_map[rename_map %in% names(df) & names(rename_map) != rename_map]

	if (length(rename_map) > 0) {
		df <- dplyr::rename(df, !!!rename_map)
	}

	return(df)
}


##################################################################################

#' Internal helper: numeric-aware sort key for a covariate column
#'
#' Categorical covariates are stored as character/factor, so plain sorting is
#' lexicographic ("10" < "2"). This returns a numeric key when every non-missing
#' value converts cleanly (numeric-coded modalities then sort 2 < 10), and keeps
#' the original values otherwise (mixed numeric/text, or genuinely categorical).
#'
#' @param x A covariate column (character, factor, or numeric).
#'
#' @return A numeric vector when `x` is wholly numeric-coded, else `x` unchanged.
#'
#' @keywords internal
#' @noRd
.covariateSortKey <- function(x) {
	num <- suppressWarnings(as.numeric(as.character(x)))
	if (all(is.na(x) | !is.na(num))) num else x
}


##################################################################################

#' Internal helper to validate covariate labels against splitCovariates
#'
#' @param splitCovariates Character vector of covariate names.
#' @param splitCovariate_labels Character vector of labels (can be NULL).
#'
#' @return A character vector of validated labels matching the length of splitCovariates.
#'
.validateCovariateLabels <- function(splitCovariates, splitCovariate_labels) {
	if (is.null(splitCovariates)) {
		return(NULL)
	}

	# If no labels provided, use variable names
	if (is.null(splitCovariate_labels)) {
		return(splitCovariates)
	}

	n_cov <- length(splitCovariates)
	n_lab <- length(splitCovariate_labels)

	if (n_lab < n_cov) {
		# Scenario: Fewer labels than splitCovariates -> Fill the rest with names
		# e.g. covs=c(A,B,C), labels=c("LabelA") -> result=c("LabelA", "B", "C")
		final_labels <- c(splitCovariate_labels, splitCovariates[(n_lab + 1):n_cov])
	} else if (n_lab > n_cov) {
		# Scenario: More labels than splitCovariates -> Truncate and Warn
		final_labels <- splitCovariate_labels[1:n_cov]
		warning(paste0("More labels provided (", n_lab, ") than splitCovariates (", n_cov, "). Extra labels ignored."))
	} else {
		# Scenario: Exact match
		final_labels <- splitCovariate_labels
	}

	return(final_labels)
}

##################################################################################
#' Validate provided NCA or ratio parameters against available parameters
#'
#' @description
#' Checks if the provided NCA parameters exist in the currently loaded PKanalix project.
#' For ratio tables, validates against ratio parameters in getNCAIndividualRatios().
#' For non-ratio tables, validates against computed NCA parameters from getNCASettings().
#' Stops if none of the requested parameters are found; if some are unknown, emits
#' a message naming them alongside the remaining available (not-yet-selected)
#' parameters, then continues with the valid ones.
#'
#' @param selectParameters Character vector of NCA parameter names to validate, or NULL
#' @param ratio Logical; if TRUE, validates for ratio tables (checks ratio parameters).
#'   If FALSE (default), validates against computed NCA parameters.
#'
#' @return Invisibly returns the character vector of available parameters (always
#'   computed, even when `selectParameters` is NULL, so callers can reuse it - e.g. to
#'   validate `roundByParameter`). Stops with an error if none of the requested
#'   parameters are found; emits a message listing unknown parameters and the
#'   remaining available ones, then continues if at least one is valid.
#'
#' @details
#' When ratio = FALSE:
#'   - Uses lixoftConnectors::getNCASettings()$computedncaparameters
#'   - Gets list of NCA parameters computed/enabled in the project
#'   - Used in: listingNCAParameters, tableNCAParameters, tableNCAConcentration
#'
#' When ratio = TRUE:
#'   - Uses lixoftConnectors::getNCAIndividualRatios()
#'   - Extracts ratio parameter columns (excludes id and data variables)
#'   - Used in: tableNCARatios, listingNCARatios
#'
#' Error handling:
#'   - Stops if NONE of the requested parameters are available
#'   - Messages if SOME parameters are unknown, listing them plus the remaining
#'     available parameters (continues with valid ones)
#'
#' @noRd
.validateNCAParametersSelection <- function(selectParameters, ratio = FALSE, be = FALSE) {
	# Always compute the available parameters (even when selectParameters is NULL) so the
	# list can be returned and reused by callers (e.g. to validate roundByParameter).
	if (be) {
		# For BE tables: get computed BE parameters
		be_settings      <- lixoftConnectors::getBioequivalenceSettings()
		available_params <- be_settings$computedbioequivalenceparameters$parameters
		context_msg      <- "BE parameters"
	} else if (ratio) {
		# For ratio tables: get available ratio parameters
		ratio_result <- lixoftConnectors::getNCAIndividualRatios()

		# Get all parameters columns except id and splitCovariates
		data_info  <- lixoftConnectors::getData()
		data_vars  <- data_info$header
		ratio_cols <- setdiff(names(ratio_result), "id")

		# Parameters are columns that only appear in ratio results, not in data
		available_params <- setdiff(ratio_cols, data_vars)
		context_msg      <- "ratio parameters"
	} else {
		# For non-ratio tables: get computed NCA parameters
		available_params <- lixoftConnectors::getNCASettings()$computedncaparameters
		context_msg      <- "NCA parameters"
	}

	# Only validate a selection when one was provided.
	if (!is.null(selectParameters)) {
		# Find intersection of requested and available
		valid_params   <- intersect(selectParameters, available_params)
		invalid_params <- setdiff(selectParameters, available_params)

		# Parameters available in the project that have not been (correctly) selected -> displayed as suggestions
		remaining_params <- setdiff(available_params, valid_params)

		# Stop if none of the requested parameters are available
		if (length(valid_params) == 0) {
			stop(sprintf(
				"None of the requested %s were found.\nRequested: %s\nAvailable: %s",
				context_msg,
				paste(selectParameters, collapse = ", "),
				if (length(available_params) > 0) paste(available_params, collapse = ", ") else "none"
			), call. = FALSE)
		}

		# Notify (immediately, via message) if some parameters are invalid --> continue with valid ones
		if (length(invalid_params) > 0) {
			message(sprintf(
				"Ignoring unknown %s: %s\nAvailable %s not selected: %s",
				context_msg,
				paste(invalid_params, collapse = ", "),
				context_msg,
				if (length(remaining_params) > 0) paste(remaining_params, collapse = ", ") else "none"
			))
		}
	}

	# Return the available parameters (invisibly) for reuse by callers.
	return(invisible(available_params))
}

################################################################################
#' Normalize a roundByParameter argument into a canonical data.frame
#'
#' Accepts the dual input of the `roundByParameter` argument (mirrors the
#' named-vector-or-data.frame idiom of [.normalizeAliasMapping()]) and returns a
#' canonical `data.frame(parameter, digits, method)` in which any unset digits or
#' method have been filled from the global defaults. Returns NULL when no
#' override was supplied.
#'
#' @param roundByParameter NULL, a named numeric vector of digits
#'   (`c(Cmax = 2, ...)`), or a data.frame with a `parameter` column plus
#'   `digits` and/or `method` columns.
#' @param globalDigits Fallback digits (the caller's `roundDigits`).
#' @param globalMethod Fallback method (the caller's `roundMethod`).
#'
#' @returns Canonical `data.frame(parameter, digits, method)`, or NULL.
#' @noRd
.normalizeRoundOverrides <- function(roundByParameter, globalDigits, globalMethod) {
	if (is.null(roundByParameter)) return(NULL)

	valid_methods <- c("monolixSuiteRounding", "significantFigures",
										 "significantFiguresPreserved", "none")

	if (is.numeric(roundByParameter) && !is.data.frame(roundByParameter)) {
		# --- Named numeric vector: digits only ---
		if (is.null(names(roundByParameter)) || any(names(roundByParameter) == "")) {
			stop("roundByParameter vector must be fully named, e.g. c(Cmax = 2, AUClast = 4).",
					 call. = FALSE)
		}
		out <- data.frame(
			parameter = names(roundByParameter),
			digits    = as.numeric(unname(roundByParameter)),
			method    = NA_character_,
			stringsAsFactors = FALSE)

	} else if (is.data.frame(roundByParameter)) {
		# --- data.frame: parameter + digits and/or method ---
		nms <- tolower(names(roundByParameter))
		if (!"parameter" %in% nms) {
			# be forgiving: treat the first column as the parameter column
			names(roundByParameter)[1] <- "parameter"
			nms <- tolower(names(roundByParameter))
		}
		get_col <- function(key) {
			idx <- which(nms == key)
			if (length(idx)) roundByParameter[[idx[1]]] else NULL
		}
		param <- as.character(get_col("parameter"))
		dig   <- get_col("digits")
		if (is.null(dig)) {
			dig <- NA_real_
		} else {
			dig_chr <- as.character(dig)
			dig     <- suppressWarnings(as.numeric(dig_chr))
			# Reject values that were supplied but failed to parse (vs. genuinely missing NA)
			bad_dig <- !is.na(dig_chr) & trimws(dig_chr) != "" & is.na(dig)
			if (any(bad_dig)) {
				stop("roundByParameter digits must be numeric; invalid value(s): ",
						 paste(unique(dig_chr[bad_dig]), collapse = ", "), call. = FALSE)
			}
		}
		meth  <- get_col("method")
		meth  <- if (is.null(meth)) NA_character_ else as.character(meth)

		out <- data.frame(
			parameter = param,
			digits    = rep_len(dig, length(param)),
			method    = rep_len(meth, length(param)),
			stringsAsFactors = FALSE)

	} else {
		stop("roundByParameter must be a named numeric vector or a data.frame ",
				 "with columns parameter/digits[/method].", call. = FALSE)
	}

	# Drop rows with empty parameter names
	out$parameter <- trimws(out$parameter)
	out <- out[!is.na(out$parameter) & out$parameter != "", , drop = FALSE]
	if (!nrow(out)) return(NULL)

	# Fill unset digits/method from the global defaults
	out$digits[is.na(out$digits)] <- globalDigits
	out$method[is.na(out$method) | out$method == ""] <- globalMethod

	# Validate digits
	if (any(out$digits < 0 | out$digits != round(out$digits))) {
		stop("roundByParameter digits must be non-negative integers.", call. = FALSE)
	}
	# Validate methods
	bad_m <- setdiff(unique(out$method), valid_methods)
	if (length(bad_m)) {
		stop("Invalid rounding method(s) in roundByParameter: ",
				 paste(bad_m, collapse = ", "),
				 "\nAllowed: ", paste(valid_methods, collapse = ", "), call. = FALSE)
	}

	# De-duplicate by parameter (last specification wins)
	out <- out[!duplicated(out$parameter, fromLast = TRUE), , drop = FALSE]
	rownames(out) <- NULL
	out
}

################################################################################
#' Validate roundByParameter keys against the available parameters
#'
#' Drops keys that are not available parameters (rather than only flagging them),
#' notifying via `message()` to match [.validateNCAParametersSelection()]. Never
#' stops -- a rounding typo must not kill the table.
#'
#' @param overrides Canonical overrides from [.normalizeRoundOverrides()], or NULL.
#' @param available Character vector of available parameter names.
#' @param context Label used in the message (e.g. the calling function name).
#'
#' @returns The filtered overrides, or NULL if nothing valid remains.
#' @noRd
.validateRoundOverrides <- function(overrides, available, context = "table") {
	if (is.null(overrides) || !nrow(overrides)) return(NULL)
	unknown <- setdiff(overrides$parameter, available)
	if (length(unknown)) {
		message("roundByParameter: ignoring unknown parameter(s): ",
						paste(unknown, collapse = ", "),
						"\nAvailable parameters for ", context, ": ",
						paste(sort(available), collapse = ", "))
		overrides <- overrides[!overrides$parameter %in% unknown, , drop = FALSE]
	}
	if (!nrow(overrides)) return(NULL)
	rownames(overrides) <- NULL
	overrides
}

################################################################################
#' Filter Observations by Time Deviation
#'
#' Filters out observations where the percentage deviation between actual time
#' and nominal time exceeds a specified threshold. Used internally by
#' `tableNCAConcentration` to exclude observations with large time deviations
#' from summary statistics.
#'
#' @param data A data frame containing concentration data with columns
#'   `ActualTime` and `NominalTime`.
#' @param occs A vector containing names of occasion varaibales
#'
#' @param threshold Numeric value specifying the maximum allowed percentage
#'   deviation. Observations where `abs((ActualTime - NominalTime) / NominalTime) * 100`
#'   exceeds this value are excluded.
#'
#' @details
#' The percentage deviation is calculated as:
#' \deqn{\frac{|ActualTime - NominalTime|}{NominalTime} \times 100}
#'
#' Observations at `NominalTime = 0` are always retained (excluded from filtering)
#' since the percentage deviation is undefined at t=0.
#'
#' The function attaches the following attributes to the returned data frame
#' for downstream footnote generation:
#' \itemize{
#'   \item `timeDeviationFiltered`: Logical flag indicating if any observations were excluded
#'   \item `timeDeviationThreshold`: The threshold value used
#'   \item `timeDeviationExcluded`: Data frame of excluded observations containing
#'     subject ID, occasion columns (if present), ActualTime, NominalTime, and
#'     the calculated percentage deviation
#' }
#'
#' @return A filtered data frame with observations exceeding the threshold removed.
#'   Attributes are attached for footnote generation (see Details).
#'
#' @keywords internal
.filterByTimeDeviation <- function(data, occs, threshold) {
	# Calculate percentage deviation: abs((actual - nominal) / nominal) * 100
	# t=0 observations are excluded from filtering (always kept)

	data <- data %>%
		dplyr::mutate(
			.pct_deviation = dplyr::case_when(
				NominalTime == 0 ~ NA_real_,
				TRUE ~ abs((ActualTime - NominalTime) / NominalTime) * 100),
			.exceeds_threshold = .pct_deviation > threshold)

	# Capture excluded observations (id + occasion columns + time info)
	id_cols <- c("id", occs)
	excluded_obs <- data %>%
		dplyr::filter(.exceeds_threshold == TRUE) %>%
		dplyr::select(
			dplyr::any_of(id_cols),
			ActualTime,
			NominalTime,
			.pct_deviation)

	n_excluded <- nrow(excluded_obs)

	# Filter and clean up

	filtered_data <- data %>%
		dplyr::filter(!.exceeds_threshold | is.na(.exceeds_threshold)) %>%
		dplyr::select(-.pct_deviation, -.exceeds_threshold)

	# Attach attributes for footnote generation
	if (n_excluded > 0) {
		attr(filtered_data, "timeDeviationFiltered") <- TRUE
		attr(filtered_data, "timeDeviationThreshold") <- threshold
		attr(filtered_data, "timeDeviationExcluded") <- excluded_obs

		message(sprintf("Excluded %d observations exceeding %g%% time deviation threshold.",
										n_excluded, threshold))
	} else {
		attr(filtered_data, "timeDeviationFiltered") <- FALSE
	}

	return(filtered_data)

}

################################################################################
#' Add individual subject data to summary statistics table
#'
#' Unified helper to add individual subject IDs and their values (parameters, ratios, or concentrations)
#' below summary statistics. Works for NCA parameters, ratios, and concentrations.
#'
#' @param summary_stats Summary statistics data frame (in vertical format with Statistic column for params/ratios,
#'   or in wide format with time columns for concentrations)
#' @param splitCovariates Character vector of covariate names (optional)
#' @param resultType Type of result: "ncaParameters", "ncaRatio", or "ncaConcentration"
#' @param params Parameter names to include (the caller's selectParameters for both parameters and ratios, NULL for all)
#' @param roundDigits Number of decimal places for rounding
#' @param roundMethod Rounding method ("monolixSuiteRounding", "significantFigures",
#'   "significantFiguresPreserved", or "none")
#' @param roundOverrides Optional canonical per-parameter rounding overrides from
#'   [.normalizeRoundOverrides()] (data.frame with columns parameter/digits/method),
#'   or NULL. When supplied, the individual values are rounded per parameter via
#'   [.applyRounding()] so they match the per-parameter rounding applied to the
#'   summary rows. Keys must use the original (non-aliased) parameter names.
#'   Only honored for resultType "ncaParameters"/"ncaRatio". Default NULL (global rounding).
#' @param format Logical; when TRUE the individual values are rendered as character
#'   display strings (via [.applyRounding()] `format = TRUE`) instead of numeric, so
#'   they match the formatted summary rows. Default FALSE (numeric, legacy behavior).
#' @param trailingZeros Logical; passed through to the display formatting when
#'   `format = TRUE` (ignored otherwise). Default TRUE.
#' @param units Logical; include units for parameter names (only for ncaParameters)
#' @param aliases Logical; use parameter aliases (only for ncaParameters)
#' @param useNominalTime Logical; use nominal time (only for ncaConcentration)
#' @param indiv_data Optional pre-fetched individual data frame (only for ncaConcentration).
#'   If NULL, data is fetched via listingNCAConcentration(). Passing pre-fetched data avoids
#'   redundant API calls when the calling function already has the data.
#'
#' @return Data frame with summary statistics and individual subject data combined.
#'   Contains a "Section" column with values "SUMMARY" or "INDIVIDUAL" to distinguish rows.
#'   SUMMARY rows appear before INDIVIDUAL rows within each covariate group.
#'
#' @details
#' The function handles occasion columns automatically:
#' - Detects occasion variables from the PKanalix project
#' - If duplicate id-covariate combinations exist due to occasions, creates composite IDs
#'   (e.g., "Subject1-Period1") using actual occasion values
#' - For ncaParameters, applies units and aliases using .addUnitsAliases() helper
#'
#' @keywords internal
#' @noRd
################################################################################
.addIndividuals <- function(summary_stats,
														splitCovariates= NULL,
														resultType     = c("ncaParameters", "ncaRatio", "ncaConcentration"),
														params         = NULL,
														roundDigits    = 3,
														roundMethod    = "monolixSuiteRounding",
														roundOverrides = NULL,
														format         = FALSE,
														trailingZeros  = TRUE,
														units          = TRUE,
														units_df       = NULL,
														aliases        = TRUE,
														aliases_df     = NULL,
														useNominalTime = TRUE,
														indiv_data     = NULL) {

	resultType <- match.arg(resultType)

	# ==========================================================================
	# BRANCH 1: NCA Parameters and Ratios (similar logic)
	# ==========================================================================

	if (resultType %in% c("ncaParameters", "ncaRatio")) {

		# Get ALL splitCovariates from project (including occasions) to properly identify parameter columns
		project_data <- lixoftConnectors::getData()
		all_splitCovariates <- project_data$header[project_data$headerTypes %in% c("catcov", "contcov", "occ")]
		occ_vars <- project_data$header[project_data$headerTypes == "occ"]

		# Derived strat covariates (e.g. "covariateSequence_FOOD") are merged into indiv_data but
		# absent from project_data$header, so subtract them explicitly to keep them out of param_cols.
		cov_info        <- lixoftConnectors::getCovariateInformation()
		strat_cov_names <- cov_info$name[cov_info$type %in% c("stratificationcategorical", "additionalstratificationcategorical")]

		# Get individual data based on type
		if (resultType == "ncaParameters") {
			# Get parameter columns (exclude id and ALL splitCovariates including occasions)
			param_cols <- setdiff(names(indiv_data), c("id", all_splitCovariates, strat_cov_names))
			# Numeric-only backstop: NCA parameters are always numeric.
			param_cols <- param_cols[vapply(indiv_data[param_cols], is.numeric, logical(1))]

		} else {  # ncaRatio
			ratio_info <- lixoftConnectors::getNCARatios()
			param_cols <- names(ratio_info)

			# Check for remaining occasions in ratio table
			remaining_occs <- .getRemainingOccasionsInRatioTable(indiv_data)
			occ_vars <- remaining_occs  # Only use REMAINING occasions, not all project occasions
		}

		# Filter to requested parameters if specified
		if (!is.null(params)) {
			cols_to_keep <- intersect(params, param_cols)
		} else {
			cols_to_keep <- param_cols
		}

		# Get parameter order from summary_stats (pretty names from tableNCAParameters)
		param_order <- setdiff(names(summary_stats), c("Statistic", splitCovariates))

		# ==========================================================================
		# Apply units/aliases FIRST to create mapping (before any pivoting)
		# This ensures parameter names match param_order from summary_stats
		# ==========================================================================
		# For ncaParameters: use units/aliases flags
		# For ncaRatio: use custom aliases_df only (ratios are dimensionless, no units)
		should_apply_aliases <- (resultType == "ncaParameters" && (units || aliases)) ||
		                        (resultType == "ncaRatio" && !is.null(aliases_df))

		if (should_apply_aliases) {
			temp_df <- data.frame(parameter = cols_to_keep, stringsAsFactors = FALSE)
			temp_df_formatted <- .addUnitsAliases(
				temp_df,
				add_units   = if (resultType == "ncaRatio") FALSE else units,
				add_aliases = if (resultType == "ncaRatio") TRUE else aliases,
				units_df    = units_df,
				aliases_df  = aliases_df)
			name_mapping <- setNames(temp_df_formatted$parameter, cols_to_keep)

			# For ncaRatio: update param_order to use aliased names (summary_stats has original names)
			if (resultType == "ncaRatio") {
				param_order <- unname(name_mapping)
			}
		} else {
			name_mapping <- setNames(cols_to_keep, cols_to_keep)  # identity mapping
		}

		# ==========================================================================
		# Select only needed columns, pivot to long, check for occasions
		# ==========================================================================

		# Columns to keep: id, user splitCovariates, occasion columns, selected parameters
		cols_for_pivot <- c("id", splitCovariates, occ_vars, cols_to_keep)
		cols_for_pivot <- intersect(cols_for_pivot, names(indiv_data))

		# Pivot to long format
		long_df <- indiv_data %>%
			dplyr::select(dplyr::all_of(cols_for_pivot)) %>%
			tidyr::pivot_longer(
				cols      = dplyr::all_of(cols_to_keep),
				names_to  = "Parameter",
				values_to = "Value")

		# round individual values with original param names
		# .applyRounding falls back to global digits/method when roundOverrides is NULL,
		# exclude everything but Value --> only value column is rounded (id/covariates/occs rmain untouched)
		long_df <- .applyRounding(
			df            = long_df,
			digits        = roundDigits,
			method        = roundMethod,
			exclude       = setdiff(names(long_df), "Value"),
			overrides     = roundOverrides,
			paramCol      = "Parameter",
			paramAxis     = "row",
			format        = format,
			trailingZeros = trailingZeros)

		# Apply name mapping to Parameter column (after rounding, so overrides matched)
		long_df$Parameter <- name_mapping[long_df$Parameter]

		# Check for occasions (duplicate id-covariate-parameter combinations)
		# Pass occ_vars explicitly - differs between ncaParameters and ncaRatio
		composite_result <- .createCompositeId(long_df, splitCovariates, occ_vars = occ_vars,
																					 value_group_col = "Parameter")
		long_df <- composite_result$data
		use_composite_id <- composite_result$composite_id_created

		# Determine which ID column to use
		id_col <- if (use_composite_id) "composite_id" else "id"

		# ==========================================================================
		# Pivot to wide format and combine with summary_stats
		# ==========================================================================

		# Pivot to wide (parameters as columns, id/composite_id as rows)
		indiv_wide <- long_df %>%
			tidyr::pivot_wider(
				id_cols     = c(!!rlang::sym(id_col), dplyr::all_of(splitCovariates)),
				names_from  = Parameter,
				values_from = Value) %>%
			dplyr::rename(Statistic = !!rlang::sym(id_col)) %>%
			dplyr::mutate(Section = "INDIVIDUAL")

		# Ensure column order matches summary_stats
		indiv_wide <- indiv_wide %>%
			dplyr::mutate(dplyr::across(dplyr::all_of(param_order), as.character)) %>%
			dplyr::select(dplyr::all_of(c(splitCovariates, "Statistic", param_order, "Section")))

		# Add Section column to summary stats
		summary_stats$Section <- "SUMMARY"

		# Combine and sort
		result <- dplyr::bind_rows(summary_stats, indiv_wide)

		# Convert Section to factor to ensure SUMMARY appears before INDIVIDUAL
		result$Section <- factor(result$Section, levels = c("SUMMARY", "INDIVIDUAL"))

		# Sort by splitCovariates (if present), then Section (SUMMARY before INDIVIDUAL)
		if (!is.null(splitCovariates) && length(splitCovariates) > 0) {
			result <- result %>%
				dplyr::arrange(dplyr::across(dplyr::all_of(splitCovariates)), Section)
		} else {
			result <- result %>%
				dplyr::arrange(Section)
		}

		# Convert Section back to character for consistent output
		result$Section <- as.character(result$Section)

		# Reorder columns: splitCovariates, Statistic, Section, then parameters
		result <- result %>%
			dplyr::select(dplyr::all_of(c(splitCovariates, "Statistic", param_order, "Section")))

		return(as.data.frame(result))
	}

	# ==========================================================================
	# BRANCH 2: NCA Concentrations (different structure - time as columns)
	# ==========================================================================

	if (resultType == "ncaConcentration") {

		# Get occasion variables from project settings (same as ncaParameters)
		project_data <- lixoftConnectors::getData()
		occ_vars <- project_data$header[project_data$headerTypes == "occ"]

		# Use provided indiv_data or fetch individual concentration data
		# Force raw column names; code below joins/groups on raw names
		if (is.null(indiv_data)) {
			indiv_data <- listingNCAConcentration(
				projectFile    = NULL,  # Already loaded
				splitCovariates     = splitCovariates,
				useNominalTime = useNominalTime,
				prettyLabels   = FALSE,
				units          = FALSE,
				roundDigits    = NULL,  # Don't round here, will round below
				roundMethod    = "none")
		}

		# Determine time column
		time_col <- if ("NominalTime" %in% names(indiv_data)) "NominalTime" else "ActualTime"

		# Round concentration values (raw_data from tableNCAConcentration is unrounded).
		# In format mode render them as display strings so they match the summary rows.
		if (isTRUE(format)) {
			indiv_data$UsedConc <- .formatRounded(
				indiv_data$UsedConc, digits = roundDigits,
				method = roundMethod, trailingZeros = trailingZeros)
		} else {
			indiv_data$UsedConc <- .round_num(
				to_round        = data.frame(val = indiv_data$UsedConc),
				digits          = roundDigits,
				rounding_method = roundMethod,
				exclude         = NULL)$val
		}

		# *** CHECK FOR OCCASIONS BEFORE PIVOTING TO WIDE ***
		# Pass full indiv_data with all columns (including occasions) to detect duplicates correctly
		composite_result <- .createCompositeId(indiv_data, splitCovariates, occ_vars = occ_vars,
																					 value_group_col = time_col)
		indiv_data_processed <- composite_result$data
		use_composite_id <- composite_result$composite_id_created

		# Determine which ID column to use
		id_col <- if (use_composite_id) "composite_id" else "id"

		# Pivot individual data to wide format (time as columns)
		indiv_wide <- indiv_data_processed %>%
			dplyr::select(!!rlang::sym(id_col), dplyr::all_of(splitCovariates),
										!!rlang::sym(time_col), UsedConc) %>%
			tidyr::pivot_wider(
				id_cols     = c(!!rlang::sym(id_col), dplyr::all_of(splitCovariates)),
				names_from  = !!rlang::sym(time_col),
				values_from = UsedConc) %>%
			dplyr::rename(Statistic = !!rlang::sym(id_col)) %>%
			dplyr::mutate(Section = "INDIVIDUAL")

		# Get time points from summary_stats (columns excluding Statistic and splitCovariates)
		time_cols <- setdiff(names(indiv_wide), c("Statistic", splitCovariates, "Section"))

		# Ensure column order matches summary_stats
		indiv_wide <- indiv_wide %>%
			dplyr::mutate(dplyr::across(dplyr::all_of(time_cols), as.character)) %>%
			dplyr::select(dplyr::all_of(c(splitCovariates, "Statistic", time_cols, "Section")))

		# Add Section column to summary stats
		summary_stats$Section <- "SUMMARY"

		# Combine and sort
		result <- dplyr::bind_rows(summary_stats, indiv_wide)

		# Convert Section to factor to ensure SUMMARY appears before INDIVIDUAL
		result$Section <- factor(result$Section, levels = c("SUMMARY", "INDIVIDUAL"))

		# Sort by splitCovariates (if present), then Section (SUMMARY before INDIVIDUAL)
		if (!is.null(splitCovariates) && length(splitCovariates) > 0) {
			result <- result %>%
				dplyr::arrange(dplyr::across(dplyr::all_of(splitCovariates)), Section)
		} else {
			result <- result %>%
				dplyr::arrange(Section)
		}

		# Convert Section back to character for consistent output
		result$Section <- as.character(result$Section)

		# Reorder columns: splitCovariates, Statistic, Section, then time points
		result <- result %>%
			dplyr::select(dplyr::all_of(c(splitCovariates, "Statistic", time_cols, "Section")))

		return(as.data.frame(result))
	}
}
################################################################################
#' Get remaining occasion columns in NCA ratio table
#'
#' Identifies which occasion variables from the project are still present in
#' the individual ratio table. Ratios are computed BETWEEN occasions (e.g.,
#' OCC2/OCC1), so those occasions are consumed. However, other occasion
#' variables may remain and cause non-unique subject identifiers.
#'
#' @param ratio_data Data frame from getNCAIndividualRatios(). If NULL,
#'   fetches it automatically.
#'
#' @return Character vector of occasion column names that remain in the ratio
#'   table (may be empty if all occasions were used for ratio computation).
#'
#' @details
#' This function:
#' 1. Gets all occasion variables defined in the project
#' 2. Gets the ratio parameter columns from getNCARatios()
#' 3. Identifies columns in ratio_data that are neither 'id' nor ratio params
#' 4. Returns intersection with project occasions (remaining occasions)
#'
#' Used by .addIndividuals() to determine which occasions apply for ncaRatio,
#' since ratio tables have different occasion handling than ncaParameters.
#'
#' @seealso \code{\link{.createCompositeId}} which uses this to determine
#'   appropriate occasion variables for ratio data
#'
#' @keywords internal
#' @noRd
################################################################################
.getRemainingOccasionsInRatioTable <- function(ratio_data = NULL) {
	# Fetch ratio data if not provided
	if (is.null(ratio_data)) {
		ratio_data <- lixoftConnectors::getNCAIndividualRatios()
	}

	# Get project occasion variables
	project_data <- lixoftConnectors::getData()
	occ_vars <- project_data$header[project_data$headerTypes == "occ"]

	# Get ratio parameter columns
	ratio_info <- lixoftConnectors::getNCARatios()
	ratio_cols <- names(ratio_info)

	# Find non-id, non-ratio columns
	other_cols <- setdiff(names(ratio_data), c("id", ratio_cols))

	# Which are occasion variables?
	remaining_occs <- intersect(other_cols, occ_vars)

	return(remaining_occs)
}
################################################################################
#' Create composite IDs for handling occasions in individual data
#'
#' Detects when duplicate id-covariate-parameter combinations exist due to
#' occasions and creates composite IDs by combining subject ID with occasion
#' values. Handles long format data where each subject has multiple rows
#' (one per parameter or time point).
#'
#' @param data Data frame containing individual data with id column and
#'   potentially occasion columns. Expected to be in long format.
#' @param splitCovariates Character vector of user-specified covariate names (may
#'   include occasion columns, which will be excluded from grouping).
#' @param occ_vars Character vector of occasion variable names to check.
#'   Must be provided by caller based on data type:
#'   \itemize{
#'     \item ncaParameters/ncaConcentration: from project settings
#'     \item ncaRatio: from .getRemainingOccasionsInRatioTable() (only
#'       remaining occasions, not those used to compute ratios)
#'   }
#' @param value_group_col Column name that causes expected row multiplication
#'   in long format. Used to properly check uniqueness:
#'   \itemize{
#'     \item ncaParameters/ncaRatio: "Parameter"
#'     \item ncaConcentration: time column (NominalTime or ActualTime)
#'   }
#'
#' @return A list with three elements:
#'   \item{data}{The input data frame, potentially with new "composite_id" column}
#'   \item{composite_id_created}{Logical; TRUE if composite IDs were created}
#'   \item{occasion_cols}{Character vector of occasion column names used for
#'     composite ID, or NULL if no composite ID was needed}
#'
#' @details
#' The function:
#' 1. Excludes occasions from user splitCovariates to define grouping columns
#' 2. Includes value_group_col to account for long format (multiple rows per subject)
#' 3. Checks if any (id, covariate, parameter/time) combination appears more than once
#' 4. If duplicates exist, identifies which occasion columns vary within groups
#' 5. Creates composite IDs using actual occasion values (e.g., "Subject1-Period1")
#'
#' This ensures unique identifiers when pivoting to wide format, while preserving
#' the mapping to original subject ID and occasion values.
#'
#' @seealso \code{\link{.getRemainingOccasionsInRatioTable}} for determining
#'   appropriate occ_vars for ratio data
#'
#' @keywords internal
#' @noRd
################################################################################
.createCompositeId <- function(data, splitCovariates, occ_vars, value_group_col) {
	# Define grouping columns (id + user-specified splitCovariates, excluding occasions)

	user_splitCovariates <- setdiff(splitCovariates, occ_vars)
	if (is.null(user_splitCovariates) || length(user_splitCovariates) == 0) {
		grouping_cols <- "id"
	} else {
		grouping_cols <- c("id", user_splitCovariates)
	}

	# Include value_group_col in check (accounts for long format where each
	# id-covariate has multiple rows - one per Parameter or time point)
	check_cols <- grouping_cols
	if (!is.null(value_group_col) && value_group_col %in% names(data)) {
		check_cols <- c(check_cols, value_group_col)
	}

	# Count occurrences of each id-covariate-valueGroup combination
	dup_check <- data %>%
		dplyr::group_by(dplyr::across(dplyr::all_of(check_cols))) %>%
		dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")

	# If all combinations appear exactly once, no composite ID needed
	if (all(dup_check$n_obs == 1)) {
		return(list(
			data = data,
			composite_id_created = FALSE,
			occasion_cols = NULL
		))
	}

	# Duplicates exist! Identify which occasion columns vary within id-covariate groups
	# Only use occasions that actually cause the duplication

	# Find occasion columns that are present in the data
	available_occ_cols <- intersect(occ_vars, names(data))

	if (length(available_occ_cols) == 0) {
		# No occasion columns found - cannot create composite IDs
		warning("Duplicate id-covariate combinations detected but no occasion columns found. ",
						"Cannot create composite IDs. Keeping original id.")
		return(list(
			data = data,
			composite_id_created = FALSE,
			occasion_cols = NULL
		))
	}

	# Check which occasion columns actually vary within id-covariate groups
	varying_occ_cols <- c()
	for (occ_col in available_occ_cols) {
		max_unique <- data %>%
			dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
			dplyr::summarise(
				n_unique = dplyr::n_distinct(!!rlang::sym(occ_col), na.rm = TRUE),
				.groups = "drop"
			) %>%
			dplyr::pull(n_unique) %>%
			max(na.rm = TRUE)

		if (max_unique > 1) {
			varying_occ_cols <- c(varying_occ_cols, occ_col)
		}
	}

	# Use varying occasion columns for composite ID
	if (length(varying_occ_cols) > 0) {
		# Create composite ID by pasting id with actual occasion values
		composite_parts <- list(rlang::sym("id"))
		for (col in varying_occ_cols) {
			composite_parts <- c(composite_parts, list(rlang::sym(col)))
		}

		data <- data %>%
			dplyr::mutate(
				composite_id = do.call(paste, c(composite_parts, list(sep = "-"))),
				.before = 1
			)

		message("Created composite IDs using: id + ", paste(varying_occ_cols, collapse = " + "))
	} else {
		# All occasion columns are constant within groups
		# This means occasions don't actually cause duplication - should not happen
		warning("Duplicate id-covariate combinations detected but occasion columns don't vary. ",
						"This should not occur. Keeping original id.")

		return(list(
			data = data,
			composite_id_created = FALSE,
			occasion_cols = NULL
		))
	}

	return(list(
		data = data,
		composite_id_created = TRUE,
		occasion_cols = varying_occ_cols
	))
}


##################################################################################
#' Internal helper to fetch and prepare NCA & Conecntration summary stats
#'
#' @description
#' This internal function acts as the backend data pipeline for NCA summary tables and listings.
#' It retrieves data, calculates statistics, applies rounding, handles stratification,
#' and formats labels based on settings inherited from the calling function.
#'
#' It operates in two distinct modes:
#' \itemize{
#'   \item **Parameter Mode** (`by_time = FALSE`): Retrieves and summarizes NCA parameters (e.g., Cmax, AUC).
#'   \item **Concentration Mode** (`by_time = TRUE`): Retrieves and summarizes concentration data grouped by timepoint.
#' }
#'
#' @param project A character string representing the path to the PKanalix project file (.pkx).
#' @param selectParameters A character vector of NCA parameter names to filter by.
#'   If `NULL`, all available parameters are retrieved.
#'   \emph{Applicable only when `by_time = FALSE`.}
#' @param splitCovariates A character vector of covariate names to stratify the results by.
#'   If `NULL`, overall (non-stratified) statistics are calculated.
#' @param roundDigits Integer. The rounding precision (number of digits).
#'   Inherited from the parent table/listing function settings.
#' @param roundMethod Character string. The rounding method (e.g., "monolixSuiteRounding",
#'   "significantFigures", or "significantFiguresPreserved"). Inherited from the parent
#'   table/listing function settings.
#' @param units Logical. If `TRUE`, units are appended to parameter names or values.
#'   \emph{Applicable only when `by_time = FALSE` (NCA Parameter summaries).}
#' @param aliases Logical. If `TRUE`, parameter aliases (friendly names) are used.
#'   \emph{Applicable only when `by_time = FALSE` (NCA Parameter summaries).}
#' @param by_time Logical. Toggles the function mode:
#'   \itemize{
#'     \item `FALSE` (default): Prepares NCA Parameter statistics.
#'     \item `TRUE`: Prepares Concentration summary statistics by timepoint.
#'   }
#' @param useNominalTime Logical. If `TRUE`, attempts to use Nominal Time for grouping concentrations.
#'   If unavailable, falls back to Actual Time with a warning.
#'   \emph{Applicable only when `by_time = TRUE` (Concentration summaries).}
#'
#' @return A data frame containing the summarized statistics (Mean, SD, Median, etc.)
#'   and combined metrics (MeanSD, GeomCV), formatted and rounded according to the inputs.
#'   Returns `NULL` if the required NCA results are not available.
#'
#' @keywords internal
#'
.prepareNCASummaryStats <- function(projectFile,
																		selectParameters = NULL,
																		splitCovariates = NULL,
																		roundDigits,
																		roundMethod,
																		units = TRUE,
																		aliases = TRUE,
																		by_time = FALSE,
																		useNominalTime = TRUE) {


	if (by_time) {
		# CONCENTRATION MODE: Calculate stats by timepoint
		# Set Stratification
		lixoftConnectors::setNCAResultsStratification() # reset
		if (!is.null(splitCovariates)) {
			lixoftConnectors::setNCAResultsStratification(split = splitCovariates)
		}

		# Check if nominal time exists
		has_nomtime <- FALSE
		if (useNominalTime) {
			data <- lixoftConnectors::getData()
			has_nomtime <- any(data$headerTypes == "nominaltime")
			if (!has_nomtime) {
				warning("No nominal time available in project; Actual time will be used instead.")
			}
		}

		# Get concentration data
		df <- lixoftConnectors::getNCAConcentrations(useNominalTime = has_nomtime)

		if (is.null(df)) {
			return(NULL)
		}

		# Fix column swap bug
		df_fixed <- .fixNCAConcentrationsColumnSwap(df, splitCovariates)

		# Choose label column
		stat_col <- if ("metric" %in% names(df_fixed)) "metric" else "subject"

		# Keep only summary stats (filter out individual subjects)
		summary_only <- df_fixed %>%
			dplyr::filter(!grepl("^\\d+$", !!rlang::sym(stat_col)))

		# Pivot time columns to rows
		cols_to_pivot <- setdiff(names(df_fixed), c(splitCovariates, stat_col))

		summary_long <- summary_only %>%
			tidyr::pivot_longer(
				cols      = dplyr::all_of(cols_to_pivot),
				names_to  = "Time",
				values_to = "Value")

		# Convert to numeric for proper rounding
		summary_long$Value <- suppressWarnings(as.numeric(summary_long$Value))

		# Pivot metrics to columns
		summary_stats <- summary_long %>%
			tidyr::pivot_wider(names_from = !!rlang::sym(stat_col), values_from = "Value") %>%
			dplyr::arrange(dplyr::across(dplyr::any_of(splitCovariates)), as.numeric(Time)) %>%
			dplyr::mutate(dplyr::across(dplyr::any_of(splitCovariates), as.character))

		# Apply Rounding
		summary_stats <- .round_num(
			to_round        = summary_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = c("Time", splitCovariates))

		# Add Combined Metrics
		summary_stats <- .combinedStats(summary_stats)

		# Note: units and aliases are not applicable for concentration data

		return(summary_stats)
	} else {
		# PARAMETER MODE: Original logic (calculate stats by parameter)
		# Set Stratification
		lixoftConnectors::setNCAResultsStratification() # reset
		if (!is.null(splitCovariates)) {
			lixoftConnectors::setNCAResultsStratification(split = splitCovariates)
		}

		# Get Summary Stats (from getNCAParameterStatistics)
		summary_stats <- lixoftConnectors::getNCAParameterStatistics(parameters = selectParameters)$statistics
		if (is.null(summary_stats)) {
			# Return NULL if no stats, parent function will handle
			return(NULL)
		}
		summary_stats$parameter <- as.character(summary_stats$parameter)

		# Get Individual Parameters & Stats
		indiv <- lixoftConnectors::getNCAIndividualParameters()$parameters

		# Identify grouping and pivoting structure
		grouping_vars <- c("id", splitCovariates)

		# Keep only numeric columns + splitCovariates + id
		indiv_long <- indiv %>%
			dplyr::select(dplyr::all_of(grouping_vars), dplyr::where(is.numeric)) %>%
			tidyr::pivot_longer(
				cols = -dplyr::all_of(grouping_vars),
				names_to = "parameter",
				values_to = "value") %>%
			dplyr::relocate(dplyr::any_of(splitCovariates), id, parameter, value)

		# Summarize (handles stats automatically)
		if (is.null(splitCovariates)) {
			descr_stats <- .summarizeStats(
				df         = indiv_long,
				group_vars = "parameter",
				value_col  = "value",
				id_col     = "id") %>%
				dplyr::select("parameter", P05, Q1, median, Q3, P95, max) %>%
				dplyr::arrange(parameter)
		} else {
			descr_stats <- .summarizeStats(
				df         = indiv_long,
				group_vars = c(splitCovariates, "parameter"),
				value_col  = "value",
				id_col     = "id") %>%
				dplyr::select(dplyr::all_of(c(splitCovariates, "parameter")), P05, Q1, median, Q3, P95, max) %>%
				dplyr::arrange(dplyr::across(dplyr::all_of(c(splitCovariates, "parameter"))))
		}

		# Merge Summaries
		join_cols <- c("parameter", splitCovariates)
		summary_stats <- dplyr::left_join(summary_stats, descr_stats, by = join_cols)

		# Apply Rounding & Combining
		summary_stats <- .round_num(
			to_round        = summary_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = c("parameter", splitCovariates))

		summary_stats <- .combinedStats(summary_stats)

		# Apply Labels (Units/Aliases only)
		if (isTRUE(units) || isTRUE(aliases)) {
			summary_stats <- .addUnitsAliases(summary_stats, add_units = units, add_aliases = aliases)
		}

		return(summary_stats)
	}
}


##################################################################################
#' .calculateCategoricalFrequencies
#'
#' Calculates raw frequency counts and percentages for categorical variables.
#' For each unique value (modality) within each variable, computes the count,
#' percentage, and total number of distinct subjects.
#'
#' @param df_input A data frame in long format with columns:
#'   \itemize{
#'     \item `id`: Subject/individual identifier
#'     \item `variable`: Name of the categorical variable being summarized
#'     \item `value`: The categorical value/modality for that variable
#'   }
#'
#' @details
#' This function is used internally for creating frequency tables of categorical
#' splitCovariates. It first counts the total number of distinct subjects (IDs) for
#' each variable, then calculates how many subjects have each specific value
#' (modality) of that variable.
#'
#' The percentage is calculated as `(Count / N) * 100` where:
#' \itemize{
#'   \item **Count**: Number of subjects with this specific modality
#'   \item **N**: Total number of distinct subjects for this variable
#' }
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item `variable`: Variable name
#'     \item `Modalities`: The specific value/category
#'     \item `Count`: Number of subjects with this modality
#'     \item `Percentage`: Percentage of subjects with this modality
#'     \item `N`: Total number of distinct subjects for this variable
#'   }
#'
#' @keywords internal
#'
#' @examples
#' # df <- data.frame(id = c(1,2,3,1,2,3),
#' #                  variable = rep(c("SEX", "RACE"), each = 3),
#' #                  value = c("M", "F", "M", "White", "White", "Asian"))
#' # .calculate_raw_freq(df)
#'
.calculateCategoricalFrequencies <- function(df_input) {
	# This logic assumes df_input has columns: id, variable, value
	subject_counts <- df_input %>%
		dplyr::group_by(variable) %>%
		dplyr::summarise(N = dplyr::n_distinct(id), .groups = "drop")

	df_input %>%
		dplyr::group_by(variable, value) %>%
		dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
		dplyr::left_join(subject_counts, by = "variable") %>%
		dplyr::mutate(Percentage = (Count / N) * 100) %>%
		dplyr::select(variable, Modalities = value, Count, Percentage, N)
}


##################################################################################
#' formatFrequencyTable
#'
#' Formats a frequency table by applying rounding and creating a combined
#' display metric showing count and percentage together as "N (%)".
#'
#' @param summary_tbl A data frame containing frequency statistics, typically
#'   the output from `.calculate_raw_freq()`. Must contain columns `Count` and
#'   `Percentage`.
#' @param digits Integer specifying the number of decimal places for rounding
#'   the percentage values.
#' @param method Character string specifying the rounding method: `"monolixSuiteRounding"`
#'   (adaptive precision), `"significantFigures"` (standard significant figures, zeroes
#'   non-significant digits before the decimal), or `"significantFiguresPreserved"` (modified
#'   significant figures that never zeroes digits before the decimal).
#' @param exclude_cols Character vector of column names to exclude from rounding.
#'
#' @details
#' This function performs two main operations:
#' \enumerate{
#'   \item Applies rounding to numeric columns using `.roundNumericCols()`
#'   \item Creates a combined `N (%)` column formatted as "Count (Percentage)"
#' }
#'
#' The percentage is formatted using `sprintf()` to ensure consistent decimal places.
#' After creating the combined column, the original `Count` and `Percentage` columns
#' are removed.
#'
#' @return A data frame with the same structure as the input, but with:
#'   \itemize{
#'     \item Numeric values rounded according to the specified method
#'     \item A new `N (%)` column replacing the separate `Count` and `Percentage` columns
#'   }
#'
#' @keywords internal
#'
#' @examples
#' # freq_tbl <- data.frame(variable = "SEX", Modalities = c("M", "F"),
#' #                        Count = c(15, 10), Percentage = c(60.0, 40.0), N = c(25, 25))
#' # .format_frequency_table(freq_tbl, digits = 1, method = "monolixSuiteRounding", exclude_cols = c("variable", "Modalities", "N"))
#'
.format_frequency_table <- function(summary_tbl, digits, method, exclude_cols) {
	# 1. Apply rounding (respects roundMethod)
	summary_tbl <- .round_num(
		to_round        = summary_tbl,
		digits          = digits,
		rounding_method = method,
		exclude         = exclude_cols)

	# 2. Create the final combined display metric N (%)
	summary_tbl <- summary_tbl %>%
		dplyr::mutate(`N (%)` = paste0(Count, " (", sprintf(paste0("%.", digits, "f"), Percentage), ")")) %>%
		dplyr::select(-Count, -Percentage)

	return(summary_tbl)
}


##################################################################################
#' calc_format_stratum
#'
#' Convenience wrapper that calculates raw frequencies and formats them in one step.
#' Combines `.calculate_raw_freq()` and `.format_frequency_table()` into a single
#' function call.
#'
#' @param df_input A data frame in long format with columns `id`, `variable`, and `value`.
#'   See `.calculate_raw_freq()` for details.
#' @param digits Integer specifying the number of decimal places for rounding percentages.
#' @param method Character string specifying the rounding method ("monolixSuiteRounding",
#'   "significantFigures", or "significantFiguresPreserved").
#' @param exclude_cols Character vector of column names to exclude from rounding.
#'
#' @details
#' This is a pipeline function that:
#' \enumerate{
#'   \item Calculates raw frequency statistics using `.calculate_raw_freq()`
#'   \item Formats the results using `.format_frequency_table()`
#' }
#'
#' It's used internally to streamline the creation of formatted frequency tables
#' for categorical splitCovariates in summary reports.
#'
#' @return A formatted frequency table with combined `N (%)` display metric.
#'   See `.format_frequency_table()` for details on the output structure.
#'
#' @keywords internal
#'
#' @examples
#' # df <- data.frame(id = c(1,2,3,1,2,3),
#' #                  variable = rep(c("SEX", "RACE"), each = 3),
#' #                  value = c("M", "F", "M", "White", "White", "Asian"))
#' # .calc_format_stratum(df, digits = 1, method = "monolixSuiteRounding", exclude_cols = c("variable", "Modalities", "N"))
#'
.calc_format_stratum <- function(df_input, digits, method, exclude_cols) {
	raw_stats <- .calculateCategoricalFrequencies(df_input)
	final_stats <- .format_frequency_table(raw_stats, digits, method, exclude_cols)
	return(final_stats)
}


##################################################################################
#' Mask table output for documentation / example use
#'
#' Masks sensitive data in output tables by replacing every numeric value with a
#' placeholder (whole numbers become `"xx"`, decimals become `"xx.xx"` at the
#' requested width) and replacing real category codes with generic sequential
#' labels (`"Category 1"`, `"Category 2"`, ...). Used for creating example outputs
#' or documentation without revealing actual study data. The caller decides
#' *whether* to mock (guard with `if (isTRUE(mock))`); this helper always masks.
#'
#' @param df A data frame containing the table to be masked.
#' @param digits Integer. Number of decimal places used for the mask width, i.e.
#'   `digits = 2` produces `"xx.xx"`, `digits = 3` `"xx.xxx"`, `digits = 0` `"xx"`.
#'   Every numeric value uses this mask (whole numbers included).
#' @param excludeCols Character vector of column names to leave untouched (identifier
#'   / label columns such as parameter names, statistic names, covariate names).
#' @param modalityCols Optional named list mapping a category/modality column name to
#'   either (a) the column it should be numbered *within* (its grouping column), or
#'   `NA`/`""` for column-wide numbering — labels default to `"Category 1"`,
#'   `"Category 2"`, ...; or (b) a list `list(group = ..., prefix = ...)` to also
#'   customize the label prefix (`group` defaults to `NA`, `prefix` to
#'   `"Category "`). Each listed column is relabeled with sequential labels per
#'   group and is never token-masked. Example:
#'   `list(Modalities = "Subject characteristics", SEX = NA,`
#'   `Comparison = list(prefix = "Comparison "))`.
#' @param naFillAxis Either `"column"` (default) or `"row"`. Tells the masker which
#'   axis holds a single metric, so uncomputable / missing cells can be filled with
#'   that metric's format. Use `"column"` for wide layouts (one metric per column)
#'   and `"row"` for long / stratified layouts (one metric per row).
#'
#' @details
#' Cell masking is token-based, so structure and delimiters are preserved. Every
#' number (whole or decimal) uses the same `digits`-wide mask:
#' \itemize{
#'   \item Values (incl. counts / whole numbers): `25` or `50.0` -> `"xx.xx"`
#'   \item Combined metrics:      `"12.3 (4.5)"`    -> `"xx.xx (xx.xx)"`
#'   \item Frequencies:           `"25 (50.00)"`    -> `"xx.xx (xx.xx)"`
#'   \item Confidence intervals:  `"(0.8; 1.25)"`   -> `"(xx.xx; xx.xx)"`
#'   \item Uncomputable / missing values (`NA`, or a part of a combined cell such as
#'     `"83 (NA)"`) are masked to match the format of the same metric's other cells,
#'     so they do not reveal that a stratum is too small to compute the metric.
#' }
#'
#' Category codes (both in `modalityCols` columns and in stratified column headers
#' of the form `"LABEL=value\\n(N=<n>)"`) are replaced with `"Category k"` while the
#' covariate LABEL is kept, so rows and per-stratum columns stay distinct. The
#' per-stratum subject count is masked (`N=25` -> `N=xx`).
#'
#' @return A data frame with the same columns as `df`; masked columns are returned
#'   as character.
#'
#' @keywords internal
#' @noRd
.mockOutputData <- function(df, digits, excludeCols = character(0), modalityCols = NULL,
														naFillAxis = c("column", "row")) {
	naFillAxis  <- match.arg(naFillAxis)
	df_mocked   <- df
	excludeCols <- excludeCols %||% character(0)

	# --------------------------------------------------------------------------
	# PREPARE MASK
	# Width follows `digits` (roundDigits): digits = 2 -> "xx.xx", digits = 3 ->
	# "xx.xxx", digits = 0 -> "xx". Every numeric token uses this same mask, so a
	# value renders "xx.xx" and a combined cell "xx.xx (xx.xx)" even when the real
	# value happened to be a whole number.
	num_mask <- if (digits > 0) paste0("xx.", strrep("x", digits)) else "xx"

	# Category columns are relabeled (not token-masked), so protect them here.
	modality_names <- names(modalityCols)
	protect        <- union(excludeCols, modality_names)

	# --------------------------------------------------------------------------
	# MASK DATA CELLS
	# Token-based replacement over non-protected columns: every number (whole or
	# decimal) becomes `num_mask`. Surrounding text and delimiters ("N (%)",
	# "(lo; hi)", ...) are preserved.
	# "Incomplete" cells (NA, or a combined cell with an uncomputable part such as
	# "83 (NA)") are then overwritten with the format of the same metric's other
	# cells, so a missing/uncomputable value never reveals that a stratum is tiny.
	# `naFillAxis` says which axis holds one metric: "column" (metric per column, the
	# wide layout) or "row" (metric per row, the long / stratified layout).
	cols_to_mock <- setdiff(names(df_mocked), protect)
	if (length(cols_to_mock) > 0 && nrow(df_mocked) > 0) {
		masked <- lapply(cols_to_mock, function(col) {
			v <- as.character(df_mocked[[col]])
			v <- gsub("[0-9]+\\.[0-9]+", num_mask, v)
			v <- gsub("[0-9]+", num_mask, v)
			v
		})
		M <- do.call(cbind, masked)                       # character matrix
		incomplete <- is.na(M) | grepl("NA", M)
		incomplete[is.na(incomplete)] <- TRUE

		# Most frequent complete masked pattern of a set of cells (NA/incomplete ignored).
		rep_pat <- function(x) {
			x <- x[!is.na(x) & !grepl("NA", x) & nzchar(x)]
			if (length(x) == 0) return(NA_character_)
			names(sort(table(x), decreasing = TRUE))[1]
		}

		for (idx in which(incomplete)) {
			r  <- ((idx - 1) %%  nrow(M)) + 1
			cc <- ((idx - 1) %/% nrow(M)) + 1
			primary   <- if (naFillAxis == "row") M[r, ] else M[, cc]
			secondary <- if (naFillAxis == "row") M[, cc] else M[r, ]
			fill <- rep_pat(primary)
			if (is.na(fill)) fill <- rep_pat(secondary)
			if (is.na(fill)) fill <- num_mask
			M[r, cc] <- fill
		}

		for (j in seq_along(cols_to_mock)) df_mocked[[cols_to_mock[j]]] <- M[, j]
	}

	# --------------------------------------------------------------------------
	# RELABEL CATEGORY / MODALITY COLUMNS  (sequential labels per group)
	for (col in modality_names) {
		if (!col %in% names(df_mocked)) next
		spec    <- modalityCols[[col]]
		if (is.list(spec)) {
			grp_col <- spec$group  %||% NA_character_
			prefix  <- spec$prefix %||% "Category "
		} else {
			grp_col <- spec
			prefix  <- "Category "
		}
		vals    <- as.character(df_mocked[[col]])

		use_group <- !is.null(grp_col) && !is.na(grp_col) && nzchar(grp_col) &&
			grp_col %in% names(df_mocked)
		groups <- if (use_group) as.character(df_mocked[[grp_col]]) else rep("", length(vals))

		new_vals <- vals
		for (g in unique(groups)) {
			idx           <- which(groups == g & !is.na(vals))
			distinct_vals <- unique(vals[idx])
			label_map     <- stats::setNames(paste0(prefix, seq_along(distinct_vals)),
																			 distinct_vals)
			new_vals[idx] <- label_map[vals[idx]]
		}
		df_mocked[[col]] <- new_vals
	}

	# --------------------------------------------------------------------------
	# RELABEL STRATIFIED HEADERS  (mask counts + generic labels for modality values)
	df_mocked <- .mockStratHeaders(df_mocked, protect = protect)

	return(df_mocked)
}

##################################################################################
#' Mask stratified column headers
#'
#' Rewrites stratified column headers of the form `"LABEL=value\\n(N=<n>)"`: the
#' subject count is masked (`N=25` -> `N=xx`) and each modality value is replaced
#' with a generic `"Category k"` label numbered consistently per LABEL across all
#' headers, so per-stratum columns keep distinct names. Only headers that actually
#' carry the `(N=<n>)` count suffix are treated as stratified headers; any other
#' header (e.g. `"Overall"`, plain metric columns, or an incidental `"="` such as
#' `"Time = 0"`) is left completely unchanged.
#'
#' @param df Data frame whose column names may contain stratum headers.
#' @param protect Character vector of column names to leave untouched.
#' @return `df` with rewritten column names.
#' @keywords internal
#' @noRd
.mockStratHeaders <- function(df, protect = character(0)) {
	nms      <- names(df)
	is_count <- function(s) grepl("^\\(N=[0-9]+\\)$", s)
	has_count_suffix <- function(nm) grepl("\\(N=[0-9]+\\)", nm)
	parse_kv <- function(s) regmatches(s, regexec("^(.+?)=(.+)$", s))[[1]]

	# Build per-LABEL value -> "Category k" map across all (non-protected) headers.
	strat_maps <- list()
	for (nm in nms) {
		if (nm %in% protect || !has_count_suffix(nm)) next
		for (s in strsplit(nm, "\n", fixed = TRUE)[[1]]) {
			if (is_count(s)) next
			kv <- parse_kv(s)
			if (length(kv) == 3) {
				key <- kv[2]; val <- kv[3]
				if (is.null(strat_maps[[key]])) strat_maps[[key]] <- character(0)
				if (!(val %in% names(strat_maps[[key]]))) {
					strat_maps[[key]][[val]] <- paste0("Category ", length(strat_maps[[key]]) + 1)
				}
			}
		}
	}

	new_nms <- vapply(nms, function(nm) {
		if (nm %in% protect || !has_count_suffix(nm)) return(nm)
		segs <- vapply(strsplit(nm, "\n", fixed = TRUE)[[1]], function(s) {
			if (is_count(s)) return("(N=xx)")
			kv <- parse_kv(s)
			if (length(kv) == 3) return(paste0(kv[2], "=", strat_maps[[kv[2]]][[kv[3]]]))
			s
		}, character(1), USE.NAMES = FALSE)
		paste(segs, collapse = "\n")
	}, character(1), USE.NAMES = FALSE)

	names(df) <- new_nms
	df
}

##################################################################################
#' Mask a raw-data listing by column data type
#'
#' Masks a per-record data listing (as returned by [listingExclusions()]) using the
#' MonolixSuite header type of each column, so real study data is hidden while the
#' shape of the dataset (subject grouping, category distinctness, date layout) is
#' preserved. Missing markers (`"."`, `""`, `NA`) are kept as-is.
#'
#' Masking strategy per header type:
#' \itemize{
#'   \item `id` -> generic subject labels `"id1"`, `"id2"`, ... (rows of one subject
#'     keep the same label, so multi-record subjects stay visible).
#'   \item `obsid` -> `"obsid1"`, `"obsid2"`, ... (may be strings, so relabeled).
#'   \item `admid` / `adm` -> `"adm1"`, `"adm2"`, ... (administration identifier).
#'   \item `catcov` / `stratcatcov` (and stratification categorical covariate types)
#'     -> `"Category 1"`, `"Category 2"`, ... per column.
#'   \item `occ` -> `"occ1"`, `"occ2"`, ... per column.
#'   \item `date` / `dat1` / `dat2` / `dat3` -> each digit becomes `"x"`, keeping
#'     separators, e.g. `"03/15/2024"` -> `"xx/xx/xxxx"` (order-agnostic, so one
#'     rule covers every date datatype).
#'   \item everything numeric (`time`, `observation`, `contcov`, `amount`, `rate`,
#'     `mdv`, `cens`, `limit`, ...) and any unrecognized type -> numeric mask
#'     (`"xx.xx"` at `digits` width); non-numeric text in such a column is kept,
#'     unless `maskUntagged = TRUE` (see below).
#' }
#'
#' @param df Data frame (one row per record) to mask.
#' @param colTypes Named character vector mapping column name -> header type (from
#'   `getData()`: `setNames(headerTypes, header)`). Columns not found default to the
#'   numeric mask.
#' @param digits Integer mask width for numeric columns (default 2 -> `"xx.xx"`).
#' @param maskUntagged Logical; if `TRUE`, every column that is not one of the
#'   relabeled kinds (id/obsid/adm/cat/occ/date) has its whole value replaced by
#'   the numeric mask rather than only its digit runs. This masks free-text
#'   columns that carry no digits and are therefore left untouched by digits-only
#'   masking (e.g. an exclusion-reason column in `listingExclusions()`, typed
#'   `NA`, `"ignore"` or `""`). Missing markers are still kept. Default `FALSE`
#'   (digits-only masking, previous behaviour).
#' @return `df` with masked columns (all returned as character).
#' @keywords internal
#' @noRd
.mockDataListing <- function(df, colTypes, digits = 2, maskUntagged = FALSE) {
	num_mask <- if (digits > 0) paste0("xx.", strrep("x", digits)) else "xx"

	relabel <- function(x, prefix) {
		x <- as.character(x)
		codes <- x[!is.na(x) & nzchar(x) & x != "."]
		u <- unique(codes)
		if (length(u) == 0) return(x)
		map <- stats::setNames(paste0(prefix, seq_along(u)), u)
		hit <- x %in% u
		x[hit] <- unname(map[x[hit]])
		x
	}
	mask_num <- function(x) {
		x <- as.character(x)
		x <- gsub("[0-9]+\\.[0-9]+", num_mask, x)
		x <- gsub("[0-9]+", num_mask, x)
		x
	}
	# Whole-value mask for untagged free-text columns (no digits to catch);
	# missing markers are kept.
	mask_full <- function(x) {
		x    <- as.character(x)
		keep <- is.na(x) | !nzchar(x) | x == "."
		x[!keep] <- num_mask
		x
	}
	mask_date <- function(x) {
		x  <- as.character(x)
		ok <- !is.na(x)
		x[ok] <- gsub("[0-9]", "x", x[ok])
		x
	}

	kind_of <- function(t) {
		if (is.na(t))                              return("num")
		if (t == "id")                             return("id")
		if (t %in% c("obsid", "ytype"))            return("obsid")
		if (t %in% c("admid", "adm"))              return("adm")
		if (t %in% c("catcov", "stratcatcov",
								 "stratificationcategorical",
								 "additionalstratificationcategorical")) return("cat")
		if (t == "occ")                            return("occ")
		if (t %in% c("date", "dat1", "dat2", "dat3")) return("date")
		"num"
	}

	# Columns that are not one of the relabeled kinds fall to the numeric mask.
	# When maskUntagged is set, use the whole-value mask there instead of the
	# digits-only one, so free-text columns with no digits (and columns typed
	# "ignore"/"" rather than NA) are masked too.
	num_mask_fun <- if (isTRUE(maskUntagged)) mask_full else mask_num

	for (col in names(df)) {
		k <- kind_of(unname(colTypes[col]))
		df[[col]] <- switch(k,
			id    = relabel(df[[col]], "id"),
			obsid = relabel(df[[col]], "obsid"),
			adm   = relabel(df[[col]], "adm"),
			cat   = relabel(df[[col]], "Category "),
			occ   = relabel(df[[col]], "occ"),
			date  = mask_date(df[[col]]),
			num_mask_fun(df[[col]]))
	}
	df
}

##################################################################################
#' Mask individual subject IDs within a Section-tagged summary table
#'
#' With `includeIds = TRUE`, the "Statistic" column mixes metric names (SUMMARY
#' rows) with real subject/composite IDs (INDIVIDUAL rows). This relabels only
#' the individual-row IDs, mirroring the `id` case in `.mockDataListing()`.
#'
#' @param df Data frame, possibly containing `sectionCol` / `statCol`.
#' @param sectionCol Column name holding the `"SUMMARY"`/`"INDIVIDUAL"` flag.
#' @param statCol Column name holding metric names (SUMMARY) or subject /
#'   composite IDs (INDIVIDUAL).
#' @param individualValue Value of `sectionCol` marking individual-level rows.
#' @param prefix Label prefix for the relabeled IDs (default `"id"`).
#' @return `df` with individual-row IDs relabeled; a no-op if `sectionCol` or
#'   `statCol` is absent, or no rows are tagged `individualValue`.
#' @keywords internal
#' @noRd
.mockIndividualSection <- function(df, sectionCol = "Section", statCol = "Statistic",
																	 individualValue = "INDIVIDUAL", prefix = "id") {
	if (!sectionCol %in% names(df) || !statCol %in% names(df)) return(df)

	idx <- which(df[[sectionCol]] == individualValue)
	if (length(idx) == 0) return(df)

	vals          <- as.character(df[[statCol]])
	distinct_vals <- unique(vals[idx])
	label_map     <- stats::setNames(paste0(prefix, seq_along(distinct_vals)), distinct_vals)
	vals[idx]     <- label_map[vals[idx]]
	df[[statCol]] <- vals
	df
}


##################################################################################
#' Helper: Build stratified table with covariate headers
#'
#' @param summary_stats Stratified summary statistics
#' @param overall_stats Overall summary statistics (optional)
#' @param splitCovariates Covariate names
#' @param selected_metrics Selected metrics to include
#' @param param_col Parameter column name
#' @param splitCovariate_labels Custom labels for splitCovariates (optional)
#' @keywords internal
#' @noRd
.buildStratifiedTable <- function(summary_stats, overall_stats = NULL, splitCovariates,
																	selected_metrics, param_col, covariateLabels = NULL) {
	if (!is.null(splitCovariates)) {
		# Prepare covariate labels
		if (is.null(covariateLabels)) labels <- splitCovariates else labels <- covariateLabels

		# Create header column combining splitCovariates
		df_prepared <- summary_stats %>%
			dplyr::rowwise() %>%
			dplyr::mutate(header = {
				values <- dplyr::c_across(dplyr::all_of(splitCovariates))
				pieces <- paste0(labels, "=", as.character(values))
				paste(pieces, collapse = "\n")
			}) %>%
			dplyr::ungroup()

		# Order strata numerically-aware so integer modalities (e.g. 4, 5, 10, 20) become columns in ascending numeric order rather than lexicographic
		# ("10", "14", "20", "4", ...)
		df_prepared <- df_prepared %>%
			dplyr::arrange(dplyr::across(
				dplyr::all_of(splitCovariates),
				.covariateSortKey
			))

		# Map headers to N counts
		n_per_stratum <- df_prepared %>%
			dplyr::distinct(header, N) %>%
			dplyr::mutate(header_N = paste0(header, "\n(N=", N, ")"))

		# Prepare overall data if provided
		overall_prepared <- NULL
		if (!is.null(overall_stats)) {
			overall_prepared <- overall_stats %>%
				dplyr::select(!!rlang::sym(param_col), N, dplyr::all_of(selected_metrics)) %>%
				dplyr::mutate(header_N = paste0("Overall\n(N=", N, ")"))
		}

		# Use pivot helper
		result <- .pivotMetricsWithOverall(
			df_strat         = df_prepared,
			overall_df       = overall_prepared,
			selected_metrics = selected_metrics,
			param_col        = param_col,
			n_per_stratum    = n_per_stratum
		)
	} else {
		# No splitCovariates - just Overall with N
		# N may be character here (display formatting upstream); coerce for max().
		max_n <- max(as.numeric(summary_stats$N), na.rm = TRUE)

		if (length(selected_metrics) > 1) {
			result <- summary_stats %>%
				dplyr::select(!!rlang::sym(param_col), Nobs, dplyr::all_of(selected_metrics)) %>%
				dplyr::mutate(header_N = paste0("Overall\n(N=", max_n, ")")) %>%
				dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
				tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Value") %>%
				tidyr::pivot_wider(names_from = header_N, values_from = Value) %>%
				dplyr::select(-dplyr::any_of("Nobs"))
		} else {
			result <- summary_stats %>%
				dplyr::select(!!rlang::sym(param_col), Nobs, dplyr::all_of(selected_metrics)) %>%
				dplyr::rename(!!paste0("Overall\n(N=", max_n, ")") := dplyr::all_of(selected_metrics)) %>%
				dplyr::select(-dplyr::any_of("Nobs"))
		}
	}

	return(result)
}

#' Helper: Pivot table to vertical format (Statistic as first column)
#'
#' @param summary_stats Summary statistics data frame
#' @param param_col Parameter column name
#' @param splitCovariates Covariate names to preserve
#' @keywords internal
#' @noRd
.pivotToVertical <- function(summary_stats, param_col, splitCovariates = NULL) {
	stat_cols <- setdiff(names(summary_stats), c(param_col, splitCovariates))
	param_order <- unique(summary_stats[[param_col]])
	stat_order <- stat_cols

	result <- summary_stats %>%
		dplyr::mutate(dplyr::across(dplyr::all_of(stat_cols), as.character)) %>%
		tidyr::pivot_longer(
			cols      = dplyr::all_of(stat_cols),
			names_to  = "Statistic",
			values_to = "Value"
		) %>%
		dplyr::mutate(Statistic = factor(Statistic, levels = stat_order)) %>%
		tidyr::pivot_wider(
			id_cols     = c(dplyr::any_of(splitCovariates), Statistic),
			names_from  = dplyr::all_of(param_col),
			values_from = Value
		) %>%
		dplyr::arrange(dplyr::across(dplyr::any_of(splitCovariates)), Statistic) %>%
		dplyr::select(dplyr::any_of(splitCovariates), Statistic, dplyr::all_of(as.character(param_order)))

	return(result)
}


#' Helper: Pivot metrics with optional overall column
#'
#' @param df_strat Stratified data with header column
#' @param overall_df Overall data (optional)
#' @param selected_metrics Metrics to pivot
#' @param param_col Parameter column name
#' @param n_per_stratum Data frame with header and header_N mapping
#' @keywords internal
#' @noRd
.pivotMetricsWithOverall <- function(df_strat, overall_df = NULL, selected_metrics,
																		 param_col, n_per_stratum = NULL) {
	if (length(selected_metrics) > 1) {
		# === MULTIPLE METRICS: Pivot to 'Statistic' column ===

		# Pivot Stratified
		result <- df_strat %>%
			dplyr::select(!!rlang::sym(param_col), header, dplyr::all_of(selected_metrics)) %>%
			dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
			tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Value") %>%
			tidyr::pivot_wider(names_from = header, values_from = Value)

		# Add N to column headers if available
		if (!is.null(n_per_stratum)) {
			result <- result %>%
				dplyr::rename_with(~ ifelse(
					.x %in% n_per_stratum$header,
					n_per_stratum$header_N[match(.x, n_per_stratum$header)],
					.x
				))
		}

		# Join with Overall if provided
		if (!is.null(overall_df)) {
			overall_pivoted <- overall_df %>%
				dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
				tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Overall") %>%
				dplyr::select(!!rlang::sym(param_col), Statistic, Overall, header_N)

			overall_label <- unique(overall_pivoted$header_N)[1]
			result <- result %>%
				dplyr::left_join(overall_pivoted %>% dplyr::select(-header_N), by = c(param_col, "Statistic")) %>%
				dplyr::rename(!!overall_label := Overall)
		}
	} else {
		# === SINGLE METRIC ===
		metric_name <- selected_metrics[1]

		result <- df_strat %>%
			dplyr::select(!!rlang::sym(param_col), header, dplyr::all_of(metric_name)) %>%
			tidyr::pivot_wider(names_from = header, values_from = dplyr::all_of(metric_name))

		# Add N to column headers if available
		if (!is.null(n_per_stratum)) {
			result <- result %>%
				dplyr::rename_with(~ ifelse(
					.x %in% n_per_stratum$header,
					n_per_stratum$header_N[match(.x, n_per_stratum$header)],
					.x
				))
		}

		# Join with Overall if provided
		if (!is.null(overall_df)) {
			overall_renamed <- overall_df %>%
				dplyr::rename(Overall = dplyr::all_of(metric_name)) %>%
				dplyr::select(!!rlang::sym(param_col), Overall, header_N)

			result <- result %>%
				dplyr::left_join(overall_renamed, by = param_col) %>%
				dplyr::rename(!!unique(overall_renamed$header_N)[1] := Overall) %>%
				dplyr::select(-header_N)
		}
	}

	return(result)
}
################################################################################
#' Apply Log Transformation Label to Parameter Names
#'
#' @description
#' Wraps parameter names with "ln()" prefix for parameters that were
#' log-transformed in the bioequivalence analysis.
#'
#' @param df A data.frame containing parameter information.
#' @param log_map A named logical vector where names are parameter names and
#'   values indicate whether log transformation was applied (TRUE/FALSE).
#' @param orig_col Character. Name of the column containing original parameter
#'   names used for log transformation lookup. Default: "Original_Parameter".
#' @param param_col Character. Name of the column containing formatted parameter
#'   names to be wrapped. Default: "parameter".
#'
#' @return A data.frame with log-transformed parameters wrapped in "ln()".
#'   The original parameter column is retained for subsequent processing.
#'
#' @details
#' This function checks each parameter against the log transformation map
#' (typically from \code{getBioequivalenceSettings()$computedbioequivalenceparameters})
#' and prepends "ln(" and appends ")" to parameters where \code{logTransform = TRUE}.
#'
#' @note This is an internal function used by \code{tableBEResults()} to indicate
#'   which parameters were analyzed on the log scale. Should be called after
#'   aliases are applied but before units are added.
#'
#' @seealso \code{\link{.addUnitsOnly}}, \code{\link{tableBEResults}}
#'
#' @keywords internal
.applyLogTransformLabel <- function(df, log_map, orig_col = "Original_Parameter", param_col = "parameter") {
	df[[param_col]] <- sapply(seq_len(nrow(df)), function(i) {
		orig_param <- df[[orig_col]][i]
		formatted_param <- df[[param_col]][i]

		if (isTRUE(log_map[orig_param])) {
			paste0("ln(", formatted_param, ")")
		} else {
			formatted_param
		}
	})

	df
}
################################################################################
#' Add Units to Parameter Names
#'
#' @description
#' Appends units to parameter display names by looking up the original parameter
#' name in the project's NCA settings. Units are formatted and appended in
#' parentheses (e.g., "AUClast (h*ng/mL)").
#'
#' @param df A data.frame containing parameter information.
#' @param orig_col Character. Name of the column containing original parameter
#'   names used for unit lookup. Default: "Original_Parameter".
#' @param param_col Character. Name of the column containing formatted parameter
#'   names to which units will be appended. Default: "parameter".
#'
#' @return A data.frame with units appended to the parameter column and the
#'   original parameter column removed.
#'
#' @details
#' This function retrieves unit information from the loaded PKanalix project
#' using \code{lixoftConnectors::getNCAParameterStatistics()}. If units are not
#' enabled in the project settings, a warning is issued and the original
#' parameter column is removed without modification.
#'
#' The function uses \code{.formatUnits()} to properly format unit strings
#' before appending.
#'
#' @note This is an internal function used by \code{tableBEResults()} to add
#'   units separately from aliases, allowing log transformation labels to be
#'   applied to parameter names before units are appended.
#'
#' @seealso \code{\link{.addUnitsAliases}}, \code{\link{.applyLogTransformLabel}}
#'
#' @keywords internal
.addUnitsOnly <- function(df, orig_col = "Original_Parameter", param_col = "parameter", units_df = NULL) {

	# ---------------------------------------------------------------------------
	# UNITS: Use custom data or fetch from PKanalix
	# ---------------------------------------------------------------------------
	if (!is.null(units_df)) {
		# Use custom units (already normalized to parameter/units columns)
		# Apply .formatUnits() to ensure consistent formatting
		units_df <- units_df %>%
			dplyr::mutate(units = sapply(units, .formatUnits))
	} else {
		# Fetch from PKanalix project (existing behavior)
		# Check if units are enabled in project
		if (!isTRUE(lixoftConnectors::getDataSettings()$enableunits)) {
			warning("Units were requested but are not enabled in the current project. Skipping unit retrieval.")
			df[[orig_col]] <- NULL
			return(df)
		}

		allparams <- lixoftConnectors::getNCASettings()$computedncaparameters

		NCAParamSummary_info <- lixoftConnectors::getNCAParameterStatistics(parameters = allparams)$information %>%
			tibble::rownames_to_column(var = "parameter")

		units_df <- NCAParamSummary_info %>%
			dplyr::select(parameter, units) %>%
			dplyr::mutate(units = sapply(units, .formatUnits))
	}

	# Create lookup map
	units_map <- setNames(units_df$units, units_df$parameter)

	# Apply units to parameter column
	df[[param_col]] <- sapply(seq_len(nrow(df)), function(i) {
		orig_param <- df[[orig_col]][i]
		formatted_param <- df[[param_col]][i]
		unit <- units_map[orig_param]

		if (!is.na(unit) && nchar(unit) > 0) {
			paste0(formatted_param, " (", unit, ")")
		} else {
			formatted_param
		}
	})

	# Remove Original_Parameter column
	df[[orig_col]] <- NULL
	df
}

################################################################################
#' Internal helper to relabel / unit-annotate NCA concentration listing columns
#'
#' @description
#' Applies presentation labels to the fixed columns of a `listingNCAConcentration`
#' output. `prettyLabels` delegates naming to the shared `.prettyLabels()` helper
#' (which maps `ActualTime` -> "Actual time", `NominalTime` -> "Nominal time",
#' `UsedConc` -> "Used concentration"; `Concentration` / `Censoring` are already
#' their pretty form). For urine (excretion-rate) projects the value columns are
#' instead labelled "Excretion Rate" / "Used Excretion Rate" and the time column
#' "Interval Midpoint". `units` appends the project unit in parentheses: the time
#' columns use `getDataSettings()$units$time` and the value columns use
#' amount-per-volume (`units$dose` / `units$volume`, e.g. "mg/L"), or
#' amount-per-time for urine (`units$dose` / `units$time`, e.g. "ng/h").
#'
#' The two flags are independent and compose freely:
#' \itemize{
#'   \item `prettyLabels = FALSE, units = FALSE` -> `ActualTime`
#'   \item `prettyLabels = TRUE,  units = FALSE` -> `Actual time`
#'   \item `prettyLabels = FALSE, units = TRUE`  -> `ActualTime (h)`
#'   \item `prettyLabels = TRUE,  units = TRUE`  -> `Actual time (h)`
#' }
#'
#' The time/concentration columns are captured by their raw names *before* any
#' rename, so units are appended by position onto whatever the final (raw or
#' pretty) label is. Units are only added when units are enabled in the project
#' (`getDataSettings()$enableunits`); otherwise a warning is issued and labels
#' are applied without units. Only columns present in `df` are touched, so this is
#' safe whether actual time, nominal time, or both exist.
#'
#' @param df Data frame (a `listingNCAConcentration` result).
#' @param prettyLabels Logical; rename fixed columns to publication form.
#' @param units Logical; append project units to time and concentration columns.
#'
#' @return `df` with the fixed columns renamed where requested.
#'
#' @keywords internal
#' @noRd
.applyNCAConcentrationColumnLabels <- function(df, prettyLabels = FALSE, units = FALSE) {
	if (!isTRUE(prettyLabels) && !isTRUE(units)) {
		return(df)
	}

	# Resolve project units once (shared by the time and concentration columns)
	unit_info <- .getNCAConcentrationUnits(enabled = isTRUE(units))

	# Capture the time / concentration columns by raw name BEFORE renaming,
	# so units can be appended by position regardless of the pretty rename.
	time_idx <- which(names(df) %in% c("ActualTime", "NominalTime"))
	conc_idx <- which(names(df) %in% c("Concentration", "UsedConc"))
	fixed_idx <- c(time_idx, conc_idx)
	raw_fixed <- names(df)[fixed_idx]

	# Pretty names via the shared package helper (single source of truth)
	if (isTRUE(prettyLabels)) {
		df <- .prettyLabels(df)
		# Urine data is excretion rate, not concentration: override the fixed labels
		if (.isUrineProject()) {
			for (k in seq_along(fixed_idx)) {
				u_lab <- .urineNCAConcLabel(raw_fixed[k])
				if (!is.null(u_lab)) names(df)[fixed_idx[k]] <- u_lab
			}
		}
	}

	# Append units onto the (now possibly pretty) labels by captured position
	if (isTRUE(units)) {
		if (nzchar(unit_info$time) && length(time_idx) > 0) {
			names(df)[time_idx] <- paste0(names(df)[time_idx], " (", unit_info$time, ")")
		}
		if (nzchar(unit_info$concentration) && length(conc_idx) > 0) {
			names(df)[conc_idx] <- paste0(names(df)[conc_idx], " (", unit_info$concentration, ")")
		}
	}

	return(df)
}

################################################################################
#' Internal helper to resolve time and concentration units from the project
#'
#' @description
#' Reads the project unit settings via `lixoftConnectors::getDataSettings()`.
#' The time unit is taken directly from `$units$time`; the value unit is derived
#' as amount per volume (`$units$dose` / `$units$volume`, e.g. "mg/L") for
#' concentration data, or amount per time (`$units$dose` / `$units$time`, e.g.
#' "ng/h") for urine (excretion-rate) projects. Each component is passed through
#' `.formatUnits()` for consistent display.
#'
#' Units are only returned when `enabled = TRUE` *and* units are enabled in the
#' project (`$enableunits`); otherwise a warning is issued (when `enabled`) and
#' empty strings are returned so callers can skip unit annotation.
#'
#' @param enabled Logical; whether the caller requested units.
#'
#' @return A list with character elements `time` and `concentration` (possibly
#'   empty strings).
#'
#' @keywords internal
#' @noRd
.getNCAConcentrationUnits <- function(enabled = FALSE) {
	empty <- list(time = "", concentration = "")
	if (!isTRUE(enabled)) {
		return(empty)
	}

	data_settings <- lixoftConnectors::getDataSettings()
	if (!isTRUE(data_settings$enableunits)) {
		warning("Units were requested (units = TRUE) but are not enabled in the current project. Skipping unit retrieval.")
		return(empty)
	}

	u <- data_settings$units

	time_unit <- ""
	if (!is.null(u$time) && nzchar(u$time)) {
		time_unit <- .formatUnits(u$time)
	}

	# Value unit: amount/volume for concentration data, amount/time for urine
	# (excretion rate) data.
	conc_unit <- ""
	if (.isUrineProject()) {
		if (!is.null(u$dose) && !is.null(u$time) && nzchar(u$dose) && nzchar(u$time)) {
			conc_unit <- paste0(.formatUnits(u$dose), "/", .formatUnits(u$time))
		}
	} else if (!is.null(u$dose) && !is.null(u$volume) && nzchar(u$dose) && nzchar(u$volume)) {
		conc_unit <- paste0(.formatUnits(u$dose), "/", .formatUnits(u$volume))
	}

	list(time = time_unit, concentration = conc_unit)
}

################################################################################
#' Is the loaded PKanalix project urine (excretion-rate) data?
#'
#' @return TRUE when `getDataSettings()$datatype` is "urine".
#' @keywords internal
#' @noRd
.isUrineProject <- function() {
	dt <- tryCatch(lixoftConnectors::getDataSettings()$datatype, error = function(e) NULL)
	isTRUE(any(dt == "urine"))
}

################################################################################
#' Urine (excretion-rate) pretty label for a fixed NCA-concentration column
#'
#' @param rawName Raw column name (ActualTime/NominalTime/Concentration/UsedConc).
#' @return Urine-specific pretty label, or NULL if the column has no urine override.
#' @keywords internal
#' @noRd
.urineNCAConcLabel <- function(rawName) {
	switch(rawName,
		ActualTime    = "Interval Midpoint",
		NominalTime   = "Nominal Interval Midpoint",
		Concentration = "Excretion Rate",
		UsedConc      = "Used Excretion Rate",
		NULL)
}

################################################################################
#' .isCensoredFlag
#'
#' Interprets a single value from the interpreted-data censoring column as a
#' censored / not-censored indicator. The column may be reported either as a
#' string label (PKanalix \code{getInterpretedData()} emits
#' \code{"leftCensored"}, \code{"rightCensored"}, \code{"intervalCensored"},
#' \code{"notCensored"} or \code{""}) or as a numeric flag (Monolix-style
#' censoring columns use \code{0}/\code{1}). Returns \code{TRUE} only for an
#' actively-censored value.
#'
#' @param x A single censoring-column value (character or numeric).
#' @return TRUE if the value denotes a censored observation, FALSE otherwise.
#' @keywords internal
.isCensoredFlag <- function(x) {
	x <- trimws(as.character(x))
	if (length(x) == 0 || is.na(x) || x == "") {
		return(FALSE)
	}
	# String labels: anything "...Censored" except "notCensored".
	if (grepl("censored", x, ignore.case = TRUE)) {
		return(!grepl("^not", x, ignore.case = TRUE))
	}
	# Numeric flag (e.g. 0 = not censored, 1 = censored).
	num <- suppressWarnings(as.numeric(x))
	!is.na(num) && num != 0
}

################################################################################
#' .isCensoredObservation
#'
#' Determines whether a raw observation that does not numerically match the
#' interpreted data corresponds to a censored (BLQ / ALQ) measurement that was
#' kept in the analysis through Data Formatting rather than excluded.
#'
#' @param obsValueRaw Character scalar; raw observation string (e.g. a BLQ tag).
#' @param matchingRows Data frame; interpreted-data rows matching this
#'   id/time/occasion combination.
#' @param iObs Integer; column index of the observation in the interpreted data.
#' @param censCol Character scalar or NULL; name of the censoring column in the
#'   interpreted data, or NULL when the project has none.
#'
#' @return TRUE if the row is a kept censored observation, FALSE otherwise.
#' @keywords internal
.isCensoredObservation <- function(obsValueRaw, matchingRows, iObs, censCol) {
	# No censoring column => the project does not use Data-Formatting censoring.
	if (is.null(censCol) || !(censCol %in% names(matchingRows))) {
		return(FALSE)
	}

	# Only relevant when the raw value is a non-numeric tag (e.g. "BLQ").
	# A numeric-but-mismatching value is a genuine discrepancy, not censoring.
	if (!is.na(suppressWarnings(as.numeric(obsValueRaw)))) {
		return(FALSE)
	}

	# A kept censored row carries an active censoring flag together with a
	# numeric LLOQ/ULOQ value substituted for the original tag.
	for (idx in seq_len(nrow(matchingRows))) {
		interpNum <- suppressWarnings(as.numeric(matchingRows[idx, iObs]))
		if (.isCensoredFlag(matchingRows[idx, censCol]) && !is.na(interpNum)) {
			return(TRUE)
		}
	}

	FALSE
}

################################################################################
#' .warnUnsuitableGrouping
#'
#' Warns when a chosen grouping variable has too many distinct values relative
#' to the number of subjects (\code{n_modalities / n_subjects > maxSubjectRatio})
#' to be a useful stratification key.
#'
#' @param data A data frame containing the grouping columns and the ID column.
#' @param splitCovariates Character vector of chosen grouping columns.
#' @param id_col Name of the subject-ID column in \code{data}.
#' @param maxSubjectRatio Ratio above which a warning is emitted (default 0.5).
#' @return Invisibly NULL. Emits a warning per unsuitable variable.
#' @keywords internal
.warnUnsuitableGrouping <- function(data, splitCovariates, id_col, maxSubjectRatio = 0.5) {
	if (length(splitCovariates) == 0 || !(id_col %in% names(data))) {
		return(invisible(NULL))
	}

	n_subj <- length(unique(as.character(data[[id_col]])))
	if (n_subj == 0) {
		return(invisible(NULL))
	}

	for (v in splitCovariates) {
		if (!(v %in% names(data))) next
		vals <- trimws(as.character(data[[v]]))
		vals <- vals[!is.na(vals) & vals != "" & vals != "."]
		k     <- length(unique(vals))
		ratio <- k / n_subj
		if (ratio > maxSubjectRatio) {
			warning(sprintf(
				paste0("Grouping variable '%s' has %d distinct values across %d subjects ",
							 "(%.0f%% of subjects); it might not be a suitable grouping variable."),
				v, k, n_subj, 100 * ratio), call. = FALSE)
		}
	}
	invisible(NULL)
}

################################################################################

#' .groupableExclusionColumns
#'
#' Suggests likely grouping columns for an exclusion table by dropping
#' structural/role columns (by header type); used only for messaging, not to
#' restrict what \code{tableExclusions()} accepts.
#'
#' @param hdr A tibble/data frame with columns \code{col} and \code{type} from
#'   \code{lixoftConnectors::getData()}.
#' @param listing Optional exclusion listing; if supplied, the result is
#'   restricted to columns actually present in the listing.
#' @return Character vector of permitted grouping column names.
#' @keywords internal
.groupableExclusionColumns <- function(hdr, listing = NULL) {
	disallow_types <- c("id", "amount", "observation", "cens", "censored",
											"limit", "mdv", "time", "nominaltime",
											"regressor", "evid", "date", "ss", "ii", "addl",
											"tinf", "rate", "admid", "ytype", "obsid")

	candidates <- hdr$col[!(hdr$type %in% disallow_types)]
	if (!is.null(listing)) {
		candidates <- intersect(candidates, names(listing))
	}
	candidates
}
