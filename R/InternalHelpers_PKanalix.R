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
#' - Actual rendering (as_sub(), as_sup()) happens in print_table_flex()
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
#' .addUnitsAliases
#'
#' Enhances parameter data frames by adding units and aliases to create formatted
#' display names. Combines parameter names with their corresponding units and aliases
#' to generate publication-ready parameter labels with proper fallback handling.
#'
#' @param df_param A data frame containing parameter information with a 'parameter' column.
#' @param df_units A data frame containing parameter units with 'parameter' and 'units' columns.
#'                 If NULL, no units are added.
#' @param df_aliases A data frame containing parameter aliases with 'parameter' and 'aliases' columns.
#'                   If NULL, no aliases are added.
#'
#' @return A data frame with formatted display names that combine aliases and units,
#'         maintaining the original parameter order.
#'
#' @examples
#' # Example usage:
#' # df_param <- data.frame(parameter = c("AUC", "Cmax"))
#' # df_units <- data.frame(parameter = c("AUC", "Cmax"), units = c("ng*h/mL", "ng/mL"))
#' # df_aliases <- data.frame(parameter = c("AUC", "Cmax"), aliases = c("AUC₀₋ₜ", "Cₘₐₓ"))
#' # AddUnitsAliases(df_param, df_units, df_aliases)
#'
#' #
.addUnitsAliases <- function(df, add_units = TRUE, add_aliases = TRUE) {
	# Retrieve all parameters from project
	allparams <- lixoftConnectors::getNCASettings()$computedncaparameters

	NCAParamSummary_info <- lixoftConnectors::getNCAParameterStatistics(parameters = allparams)$information %>%
		tibble::rownames_to_column(var = "parameter")


	# Check and retrieve units information (if requested)
	if (isTRUE(add_units)) {
		# extract units if active in project
		if (isTRUE(lixoftConnectors::getDataSettings()$enableunits)) {
			units_df <- NCAParamSummary_info %>%
				dplyr::select(parameter, units)
		} else {
			# warning if units are requested but not enabled
			warning("Units were requested (units=TRUE) but are not enabled in the current project. Skipping unit retrieval.")
			units_df <- NULL
		}
	} else {
		units_df <- NULL
	}

	# Check and retrieve aliases information (if requested)
	# ----------------------------------------------------------
	if (isTRUE(add_aliases)) {
		if ("aliases" %in% names(NCAParamSummary_info)) {
			aliases_df <- NCAParamSummary_info %>%
				dplyr::select(parameter, aliases) %>%
				.convertHtmlAliasesForFlextable()
		} else {
			warning("Aliases were requested (aliases=TRUE) but are not available in the current project. Skipping alias retrieval.")
			aliases_df <- NULL
		}
	} else {
		aliases_df <- NULL
	}

	# Save original order for later restoration
	original_order <- unique(df$parameter)

	# Merge units if provided
	if (!is.null(units_df)) {
		units_df <- units_df %>% dplyr::mutate(
			parameter = as.character(parameter),
			units = as.character(units)
		)
		df <- dplyr::left_join(df, units_df, by = "parameter")
	} else {
		df$units <- NA_character_
	}

	# Merge aliases if provided
	if (!is.null(aliases_df)) {
		aliases_df <- aliases_df %>% dplyr::mutate(
			parameter = as.character(parameter),
			aliases = as.character(aliases)
		)
		df <- dplyr::left_join(df, aliases_df, by = "parameter")
	} else {
		df$aliases <- NA_character_
	}

	# Coerce all relevant columns to character to prevent paste0() errors
	df <- df %>%
		dplyr::mutate(
			parameter = as.character(parameter),
			aliases = as.character(aliases),
			units = as.character(units)
		)

	# Build display name with proper fallback handling
	df <- df %>%
		dplyr::mutate(display_name = dplyr::case_when(
			!is.na(aliases) & !is.na(units) & units != "" ~ paste0(aliases, " (", units, ")"),
			!is.na(aliases) ~ aliases,
			is.na(aliases) & !is.na(units) & units != "" ~ paste0(parameter, " (", units, ")"),
			TRUE ~ parameter
		))

	# Restore original order and finalize structure
	df <- df %>%
		dplyr::mutate(parameter = factor(parameter, levels = original_order)) %>%
		dplyr::arrange(parameter) %>%
		dplyr::relocate(display_name, .before = 1) %>%
		dplyr::select(-parameter, -units, -aliases) %>%
		dplyr::rename(parameter = display_name)

	return(df)
}


##################################################################################
#' RoundNumericCols
#'
#' Rounds all numeric columns in a data frame to a specified number of decimal places.
#' Also attempts to convert character columns that contain numeric values to numeric
#' before rounding, providing robust handling of mixed data types.
#'
#' @param df A data frame containing columns to be rounded.
#' @param digits An integer specifying the number of decimal places for rounding. Default is 3.
#'
#' @return A data frame with all numeric columns rounded to the specified number of decimal places.
#'
#' @examples
#' # Example usage:
#' # df <- data.frame(param = c("AUC", "Cmax"), value1 = c(123.456, 67.891), value2 = c("234.567", "89.123"))
#' # RoundNumericCols(df, digits = 2)
# .roundNumericCols <- function(x,
#                               digits = 3,
#                               exclude = NULL,
#                               rounding_method = c("monolix", "signif")) {
#     rounding_method <- match.arg(rounding_method)
#
#     # Inner function: rounds a single numeric vector
#     .round_num <- function(number, digits, rounding_method) {
#         # handle NA/NaN
#         if (!is.numeric(number)) {
#             number <- suppressWarnings(as.numeric(as.character(number)))
#         }
#
#         if (!is.numeric(number)) {
#             return(number)
#         }
#
#         if (rounding_method == "monolix") {
#             adj_digits <- digits + ifelse(
#                 abs(number) < 1 & number != 0 & !is.na(number),
#                 abs(floor(log10(abs(number))) + 1),
#                 0
#             )
#             rounded <- round(number, digits = adj_digits)
#         } else {
#             rounded <- signif(number, digits = digits)
#         }
#
#         # keep NaN/NA as-is
#         rounded[is.nan(number)] <- NaN
#         rounded[is.na(number)] <- NA
#         return(rounded)
#     }
#
#     # case 1: data frame apply columnwise
#     if (is.data.frame(x)) {
#         for (nm in names(x)) {
#             if (!is.null(exclude) && nm %in% exclude) next
#             x[[nm]] <- .round_num(x[[nm]], digits, rounding_method)
#         }
#         return(as.data.frame(x))
#     }
#
#     # case 2: atomic vector
#     return(.round_num(x, digits, rounding_method))
# }


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
			P05 = stats::quantile(.data[[value_col]], 0.05, na.rm = TRUE),
			Q1 = stats::quantile(.data[[value_col]], 0.25, na.rm = TRUE),
			mean = mean(.data[[value_col]], na.rm = TRUE),
			SD = stats::sd(.data[[value_col]], na.rm = TRUE),
			CV = if (is.na(mean) || mean == 0) NA_real_ else 100 * SD / mean,
			median = stats::median(.data[[value_col]], na.rm = TRUE),
			Q3 = stats::quantile(.data[[value_col]], 0.75, na.rm = TRUE),
			P95 = stats::quantile(.data[[value_col]], 0.95, na.rm = TRUE),
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
#'   Used to calculate N (number of unique subjects) and SE (standard error).
#' @param group_by Character vector or NULL specifying column names to group by.
#'   Examples: `c("Time", "SEX")`, `c("Parameter")`, `NULL` for no grouping.
#'
#' @return A data frame with grouping columns (if any) and the following metrics:
#'   N, Nobs, min, P05, Q1, mean, SD, SE, CV, median, Q3, P95, max,
#'   geoMean, geoSD, geoCV, harmMean
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

	# Group and calculate all metrics
	data %>%
		dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) %>%
		dplyr::summarise(
			N = if (!is.null(id_col)) dplyr::n_distinct(.data[[id_col]]) else dplyr::n(),
			Nobs = dplyr::n(),
			min = min(.data[[value_col]], na.rm = TRUE),
			P05 = stats::quantile(.data[[value_col]], 0.05, na.rm = TRUE),
			Q1 = stats::quantile(.data[[value_col]], 0.25, na.rm = TRUE),
			mean = mean(.data[[value_col]], na.rm = TRUE),
			SD = stats::sd(.data[[value_col]], na.rm = TRUE),
			SE = if (!is.null(id_col)) {
				sd_val <- stats::sd(.data[[value_col]], na.rm = TRUE)
				n_subj <- dplyr::n_distinct(.data[[id_col]])
				if (n_subj > 0 && !is.na(sd_val)) sd_val / sqrt(n_subj) else NA_real_
			} else NA_real_,
			CV = {
				m <- mean(.data[[value_col]], na.rm = TRUE)
				s <- stats::sd(.data[[value_col]], na.rm = TRUE)
				if (!is.na(m) && m != 0 && !is.na(s)) 100 * s / m else NA_real_
			},
			median = stats::median(.data[[value_col]], na.rm = TRUE),
			Q3 = stats::quantile(.data[[value_col]], 0.75, na.rm = TRUE),
			P95 = stats::quantile(.data[[value_col]], 0.95, na.rm = TRUE),
			max = max(.data[[value_col]], na.rm = TRUE),
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
			GeomCV = paste0(.data$geoMean, " (", .data$geoCV, ")"),
			GeomSD = paste0(.data$geoMean, " (", .data$geoSD, ")")
		)
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
#' @param original_covariates Character vector of covariate names in their intended
#'   order, as originally passed to \code{setNCAResultsStratification(split = covariates)}.
#'   Must match the order used when setting up the stratification.
#'
#' @details This function corrects a known bug where \code{getNCAConcentrations()}
#'   with \code{displayIndividualProfiles = TRUE} returns covariate columns with
#'   correct headers but swapped content. When the number of covariates is even,
#'   all covariates are affected in reverse order. When odd, the middle covariate
#'   remains correct while others are mirrored around it. The function identifies
#'   covariate columns by name, reverses their order, and applies the original
#'   intended column names to restore proper data alignment.
#'
#' @return A data frame with the same structure as the input but with covariate
#'   columns correctly ordered and named according to \code{original_covariates}.
#'   Non-covariate columns (subject, time points, concentrations) remain unchanged.
#'
#' @examples
#' # covariates <- c("DOSE", "FOOD", "VISIT")
#' # setNCAResultsStratification(split = covariates)
#' # df <- getNCAConcentrations(displayIndividualProfiles = TRUE)
#' # df_fixed <- fixNCAConcentrationsColumnSwap(df, covariates)
.fixNCAConcentrationsColumnSwap <- function(df, original_covariates) {
	# Get the number of covariates
	n_cov <- length(original_covariates)

	# If no covariates or only one, no swapping occurs
	if (n_cov <= 1) {
		return(df)
	}

	# Find covariate columns in the dataframe
	cov_cols <- names(df)[names(df) %in% original_covariates]

	# Find non-covariate columns
	non_cov_cols <- names(df)[!names(df) %in% original_covariates]

	# Create the corrected column order
	# The covariates appear to be reversed in the output
	corrected_cov_order <- rev(cov_cols)

	# Reorder the dataframe with corrected covariate order
	df_fixed <- df[, c(corrected_cov_order, non_cov_cols)]

	# Rename columns to match the original intended order
	names(df_fixed)[1:n_cov] <- original_covariates

	return(df_fixed)
}

##################################################################################

#' Internal helper to fetch and prepare Covariate summary stats
#'
#' @description Fetches covariate data, reshapes to long format,
#' summarizes using .summarizeStats, rounds, and adds combined metrics.
#'
.prepareCovariateSummaryStats <- function(projectFile, covariates, cont_cols, roundDigits, roundMethod) {
	# Base data: variables from dataset tagged as covariates
	# Note: We fetch ID to ensure we can count subjects correctly
	cov_df <- lixoftConnectors::getCovariateInformation()$covariate

	# Ensure we select valid columns (intersection of what is in cov_df and what is expected)
	# This handles edge cases where header info might slightly differ from loaded covariate info
	valid_cont_cols <- intersect(names(cov_df), cont_cols)

	if (length(valid_cont_cols) == 0) {
		return(NULL)
	}

	keep_cols <- c("id", valid_cont_cols, covariates)

	# --- Reshape to Long Format ---
	# variable = covariate name, value = measurement
	df_long <- cov_df %>%
		dplyr::select(dplyr::all_of(keep_cols)) %>%
		tidyr::pivot_longer(
			cols = dplyr::all_of(valid_cont_cols),
			names_to = "variable",
			values_to = "value"
		) %>%
		dplyr::mutate(value = suppressWarnings(as.numeric(value)))

	# Summarize & determine grouping
	if (is.null(covariates)) {
		group_vars <- "variable"
	} else {
		group_vars <- c(covariates, "variable")
	}

	# Calculate stats
	summary_stats <- .summarizeStats(
		df         = df_long,
		group_vars = group_vars,
		value_col  = "value",
		id_col     = "id"
	)

	# Standardize Count column to "Nobs"
	if ("N" %in% names(summary_stats)) {
		summary_stats <- dplyr::rename(summary_stats, Nobs = N)
	} else if ("N Total" %in% names(summary_stats)) {
		summary_stats <- dplyr::rename(summary_stats, Nobs = `N Total`)
	}

	# Rounding & combining metrics
	summary_stats <- .roundNumericCols(
		summary_stats,
		digits          = roundDigits,
		rounding_method = roundMethod,
		exclude         = c("variable", covariates, "Nobs")
	)

	summary_stats <- .combinedStats(summary_stats)

	return(summary_stats)
}

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
			be_result <- tryCatch(lixoftConnectors::getBEResults(), error = function(e) NULL)

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

		# Warning if SOME metrics missing
		if (length(missing_metrics) > 0) {
			warning(paste0(
				"Ignoring unknown metrics: ", paste(missing_metrics, collapse = ", "),
				".\nAvailable metrics: ", paste(available_metrics_list, collapse = ", ")
			))
		}

		return(valid_metrics)
	}
}

##################################################################################
#' Validate selected covariates for NCA tables and listings
#'
#' @description
#' Checks if the provided covariates exist in the currently loaded PKanalix project
#' and are of valid types. For ratio tables, only categorical covariates are allowed.
#' For other tables/listings, both categorical covariates and occasions are allowed.
#'
#' @param covariates Character vector of covariate names to validate, or NULL
#' @param ratio Logical; if TRUE, validates for ratio tables (only categorical covariates allowed
#'   via getCovariateInformation()). If FALSE (default), allows both categorical covariates
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
#'   - Allows only categorical covariates (type == "categorical")
#'   - Does NOT allow occasions
#'   - Used in: tableNCARatios, listingNCARatios
#'
#' @noRd

.validateCovariateSelection <- function(covariates, ratio = FALSE) {
	# Skip validation if no covariates provided
	if (is.null(covariates)) {
		return(invisible(NULL))
	}

	if (ratio) {
		# For ratio tables: intersection of ratio columns and data variables
		# This excludes parameters (which only appear in ratio results, not in data)
		ratio_result <- lixoftConnectors::getNCAIndividualRatios()
		ratio_cols <- setdiff(names(ratio_result), "id")
		# Get all variables from data
		data_vars <- lixoftConnectors::getData()$header
		# Available covariates = intersection (parameters are filtered out)
		available_covs <- intersect(ratio_cols, data_vars)
		context_msg <- "covariates in ratio results"
	} else {
		# For other tables/listings: use getData(), allow catcov or occ
		project_data <- lixoftConnectors::getData()
		available_covs <- project_data$header[project_data$headerTypes %in% c("catcov", "occ")]
		context_msg <- "covariates (catcov or occ)"
	}

	# Check if provided covariates are valid
	invalid_covs <- setdiff(covariates, available_covs)

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
##################################################################################
#' Internal helper to validate stratification arguments
#'
#' @param covariates Vector of covariate names (or NULL).
#' @param stratify_columns Boolean flag indicating if stratification is requested.
#'
#' @return Boolean. Returns FALSE if covariates are missing, otherwise returns original stratify_columns.
#'
.validateStratificationArgs <- function(covariates, stratify_columns) {
	# 1. Argument consistency check
	if (isTRUE(stratify_columns) && is.null(covariates)) {
		warning("stratify_columns=TRUE requires covariates. Resetting stratify_columns to FALSE.")
		return(FALSE)
	}
	# 2. Covariate existence check (if covariates provided)
	if (!is.null(covariates)) {
		# Get available covariates from current PKanalix project
		cov_info <- lixoftConnectors::getCovariateInformation()$covariate
		available_covariates <- setdiff(names(cov_info), "id")

		# Validate requested covariates exist
		bad_covariates <- setdiff(covariates, available_covariates)
		if (length(bad_covariates) > 0) {
			stop(
				"Invalid covariate(s): ", paste(bad_covariates, collapse = ", "), "\n",
				"Available covariates: ", paste(available_covariates, collapse = ", ")
			)
		}
	}
	return(stratify_columns)
}


##################################################################################

#' Internal helper to validate covariate labels against covariates
#'
#' @param covariates Character vector of covariate names.
#' @param covariate_labels Character vector of labels (can be NULL).
#'
#' @return A character vector of validated labels matching the length of covariates.
#'
.validateCovariateLabels <- function(covariates, covariate_labels) {
	if (is.null(covariates)) {
		return(NULL)
	}

	# If no labels provided, use variable names
	if (is.null(covariate_labels)) {
		return(covariates)
	}

	n_cov <- length(covariates)
	n_lab <- length(covariate_labels)

	if (n_lab < n_cov) {
		# Scenario: Fewer labels than covariates -> Fill the rest with names
		# e.g. covs=c(A,B,C), labels=c("LabelA") -> result=c("LabelA", "B", "C")
		final_labels <- c(covariate_labels, covariates[(n_lab + 1):n_cov])
	} else if (n_lab > n_cov) {
		# Scenario: More labels than covariates -> Truncate and Warn
		final_labels <- covariate_labels[1:n_cov]
		warning(paste0("More labels provided (", n_lab, ") than covariates (", n_cov, "). Extra labels ignored."))
	} else {
		# Scenario: Exact match
		final_labels <- covariate_labels
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
#' Stops if none of the requested parameters are found; warns if some are missing.
#'
#' @param NCAparams Character vector of NCA parameter names to validate, or NULL
#' @param ratio Logical; if TRUE, validates for ratio tables (checks ratio parameters).
#'   If FALSE (default), validates against computed NCA parameters.
#'
#' @return Invisible NULL if validation passes. Stops with error if no valid parameters found.
#'   Issues warning if some parameters are missing but continues.
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
#'   - Warns if SOME parameters are missing (continues with valid ones)
#'
#' @noRd
.validateNCAParametersSelection <- function(NCAparams, ratio = FALSE) {
	# Skip validation if no parameters specified
	if (is.null(NCAparams)) {
		return(invisible(NULL))
	}

	if (ratio) {
		# For ratio tables: get available ratio parameters
		ratio_result <- lixoftConnectors::getNCAIndividualRatios()

		# Get all parameters columns except id and covariates
		data_info <- lixoftConnectors::getData()
		data_vars <- data_info$header
		ratio_cols <- setdiff(names(ratio_result), "id")

		# Parameters are columns that only appear in ratio results, not in data
		available_params <- setdiff(ratio_cols, data_vars)
		context_msg      <- "ratio parameters"
	} else {
		# For non-ratio tables: get computed NCA parameters
		available_params <- lixoftConnectors::getNCASettings()$computedncaparameters
		context_msg      <- "NCA parameters"
	}

	# Find intersection of requested and available
	valid_params   <- intersect(NCAparams, available_params)
	missing_params <- setdiff(NCAparams, available_params)

	# Stop if none of the requested parameters are available
	if (length(valid_params) == 0) {
		stop(sprintf(
			"None of the requested %s were found.\nRequested: %s\nAvailable: %s",
			context_msg,
			paste(NCAparams, collapse = ", "),
			if (length(available_params) > 0) paste(available_params, collapse = ", ") else "none"
		), call. = FALSE)
	}

	# Warn if some parameters are missing (but continue with valid ones)
	if (length(missing_params) > 0) {
		warning(sprintf(
			"Ignoring unknown %s: %s",
			context_msg,
			paste(missing_params, collapse = ", ")
		), call. = FALSE)
	}

	return(invisible(NULL))
}
##################################################################################
#' Internal helper to add individual NCA parameters
#'
#' @description
#' Retrieves individual NCA parameter values and appends them below summary statistics.
#' Used by \code{tableNCAParameters} when \code{include_id = TRUE}.
#'
#' When covariates are specified, individual IDs are grouped by their covariate values
#' and listed after the corresponding summary metrics (metrics of modality 1, IDs of
#' modality 1, metrics of modality 2, IDs of modality 2, etc.).
#'
#' @param summary_stats Data frame containing summary statistics in vertical format
#'   (with "Statistic" column and parameter columns).
#' @param covariates Character vector of covariate names used for stratification.
#' @param NCAparams Character vector of NCA parameter names to include.
#' @param roundDigits Integer specifying the number of decimal places for rounding.
#' @param roundMethod Character string specifying the rounding method ("monolix", "signif", "none").
#' @param units Logical; if TRUE, units are appended to parameter names.
#' @param aliases Logical; if TRUE, parameter aliases (friendly names) are used.
#'
#' @return A data frame combining summary statistics with individual parameter values.
#'
#' @keywords internal
#'
.addIndividualsNCAParameters <- function(summary_stats,
                                         covariates,
                                         NCAparams,
                                         roundDigits,
                                         roundMethod,
                                         units,
                                         aliases) {

	# Get individual NCA parameters
	indiv_data   <- lixoftConnectors::getNCAIndividualParameters()
	indiv_params <- indiv_data$parameters
	param_info   <- indiv_data$info

	# Convert rownames to a column for easier manipulation
	param_info <- param_info %>% tibble::rownames_to_column(var = "name")

	# Filter to requested parameters if specified (preserving covariates)
	if (!is.null(NCAparams)) {
		# Keep only requested parameters plus id and covariates
		available_params <- intersect(names(indiv_params), param_info$name)
		params_to_keep   <- intersect(NCAparams, available_params)
		# Include covariates in cols_to_keep (they're already in indiv_params for occasion-level covariates)
		cov_cols_present <- intersect(covariates, names(indiv_params))
		cols_to_keep     <- c("id", cov_cols_present, params_to_keep)
		indiv_params     <- indiv_params[, cols_to_keep, drop = FALSE]
	}

	# Round individual parameter values
	indiv_params <- .round_num(
		to_round        = indiv_params,
		digits          = roundDigits,
		rounding_method = roundMethod,
		exclude         = c("id", covariates))

	# Apply units and aliases to parameter column names
	# Get parameter columns (exclude id and covariates)
	param_cols <- setdiff(names(indiv_params), c("id", covariates))

	if (length(param_cols) > 0) {
		# temp dataframe for transformation
		temp_df <- data.frame(parameter = param_cols, stringsAsFactors = FALSE)
		temp_df <- .addUnitsAliases(temp_df, add_units = units, add_aliases = aliases)

		# Create mapping: old name -> new name
		name_mapping <- setNames(temp_df$parameter, param_cols)

		# Rename columns in indiv_params (vectorized approach)
		new_names  <- names(indiv_params)
		rename_idx <- which(new_names %in% names(name_mapping))
		new_names[rename_idx] <- name_mapping[new_names[rename_idx]]
		names(indiv_params)   <- new_names
	}

	# Get parameter columns from summary_stats (excluding Statistic and covariates)
	summary_param_cols <- setdiff(names(summary_stats), c("Statistic", covariates))

	# Select only columns that match summary_stats
	cols_to_keep <- c("id", covariates, intersect(names(indiv_params), summary_param_cols))
	indiv_params <- indiv_params[, cols_to_keep, drop = FALSE]

	# Rename id to Statistic
	indiv_params <- indiv_params %>% dplyr::rename(Statistic = id)

	# Convert parameter columns to character to match summary_stats format
	param_cols <- setdiff(names(indiv_params), c("Statistic", covariates))
	if (length(param_cols) > 0) {
		indiv_params <- indiv_params %>% dplyr::mutate(dplyr::across(dplyr::all_of(param_cols), as.character))
	}

	# Combine with summary_stats
	if (!is.null(covariates) && length(covariates) > 0) {
		# Split both dataframes by covariate combinations
		summary_groups <- summary_stats %>%
			dplyr::group_by(dplyr::across(dplyr::all_of(covariates))) %>%
			dplyr::group_split()

		indiv_groups <- indiv_params %>%
			dplyr::group_by(dplyr::across(dplyr::all_of(covariates))) %>%
			dplyr::group_split()

		# Add Section column to each group
		summary_groups <- lapply(summary_groups, function(x) {
			x$Section <- "SUMMARY"
			x
		})

		indiv_groups <- lapply(indiv_groups, function(x) {
			x$Section <- "INDIVIDUAL"
			x
		})

		# Combine corresponding groups (metrics first, then individuals)
		result_list <- purrr::map2(summary_groups, indiv_groups, rbind)

		final_result <- do.call(rbind, result_list)
	} else {
		# No covariates: simple append
		summary_stats$Section <- "SUMMARY"
		indiv_params$Section  <- "INDIVIDUAL"
		final_result          <- rbind(summary_stats, indiv_params)
	}

	return(final_result)
}

##################################################################################
#' Internal helper to add individual concentration values
#'
#' @description
#' Retrieves individual concentration data and appends them below summary statistics.
#' Used by \code{tableNCAConcentration} when \code{include_id = TRUE}.
#'
#' When covariates are specified, individual IDs are grouped by their covariate values
#' and listed after the corresponding summary metrics.
#'
#' @param summary_stats Data frame containing summary statistics with Time as a column
#'   and metrics as other columns. Will be pivoted so time points become columns.
#' @param covariates Character vector of covariate names used for stratification.
#' @param useNominalTime Logical; whether nominal time should be used.
#' @param roundDigits Integer specifying the number of decimal places for rounding.
#' @param roundMethod Character string specifying the rounding method ("monolix", "signif", "none").
#'
#' @return A data frame combining summary statistics with individual concentration values,
#'   with a "Section" column distinguishing "SUMMARY" from "INDIVIDUAL" rows.
#'   Time points are columns, subjects/metrics are rows.
#'
#' @keywords internal
#'
.addIndividualsNCAConcentration <- function(summary_stats,
																						covariates,
																						useNominalTime,
																						roundDigits,
																						roundMethod) {

	# ==========================================================================
	# 1. PIVOT SUMMARY STATISTICS: Time as columns
	# ==========================================================================
	# Current format: Time | [Covariates] | Metric1 | Metric2 | ...
	# Target format:  Statistic | [Covariates] | Time1 | Time2 | ...

	# Identify columns
	time_col <- "Time"
	stat_cols <- setdiff(names(summary_stats), c(time_col, covariates))

	# Convert all metric columns to character (handles mixed numeric/character like MeanSD)
	summary_stats <- summary_stats %>%
		dplyr::mutate(dplyr::across(dplyr::all_of(stat_cols), as.character))

	# Pivot longer (stack metrics), then wider (time as columns)
	summary_pivoted <- summary_stats %>%
		tidyr::pivot_longer(
			cols      = dplyr::all_of(stat_cols),
			names_to  = "Statistic",
			values_to = "Value"
		) %>%
		tidyr::pivot_wider(
			names_from  = !!rlang::sym(time_col),
			values_from = Value
		)

	# ==========================================================================
	# 2. GET INDIVIDUAL CONCENTRATION DATA
	# ==========================================================================
	# reinitialize stratification connector
	lixoftConnectors::setNCAResultsStratification()

	# Set stratification
	if (!is.null(covariates)) {
		lixoftConnectors::setNCAResultsStratification(split = covariates)
	}

	indiv_data <- lixoftConnectors::getNCAConcentrations(displayIndividualProfiles = TRUE, useNominalTime = useNominalTime)

	# Fix column swap bug if covariates present
	if (!is.null(covariates) && length(covariates) > 0) {
		indiv_data <- .fixNCAConcentrationsColumnSwap(indiv_data, covariates)
	}

	# ==========================================================================
	# 3. FILTER OUT METRIC ROWS (keep only individuals)
	# ==========================================================================
	# Identify subject/metric column
	stat_col <- if ("metric" %in% names(indiv_data)) "metric" else "subject"

	# Get metric labels to filter out
	metrics <- lixoftConnectors::getNCAConcentrations()$metric
	metric_labels <- as.character(metrics[!is.na(metrics)])

	# Keep only individual profiles
	indiv_only <- indiv_data %>% dplyr::filter(!(!!rlang::sym(stat_col) %in% metric_labels))

	# ==========================================================================
	# 4. ROUND INDIVIDUAL VALUES
	# ==========================================================================
	# Get time columns (everything except ID and covariates)
	time_cols <- setdiff(names(indiv_only), c(stat_col, covariates))

	# Convert all time columns to character before pivoting (prevents list-column issues)
	indiv_only <- indiv_only %>% dplyr::mutate(dplyr::across(dplyr::all_of(time_cols), as.character))

	# Pivot to long for rounding
	indiv_long <- indiv_only %>%
		tidyr::pivot_longer(
			cols      = dplyr::all_of(time_cols),
			names_to  = "Time",
			values_to = "Value") %>%
		dplyr::filter(Value != "")  # Remove empty values

	# Round values (preserve non-numeric values like <LLOQ)
	indiv_long <- indiv_long %>%
		dplyr::mutate(
			.value_numeric = suppressWarnings(as.numeric(Value)),
			Value = dplyr::case_when(
				is.na(.value_numeric) ~ Value,  # Keep non-numeric values (e.g., <LLOQ)
				TRUE ~ as.character(.round_num(.value_numeric,
																			 digits = roundDigits,
																			 rounding_method = roundMethod)))) %>%
		dplyr::select(-.value_numeric)  # Remove helper column

	# Pivot back to wide: time points as columns
	indiv_wide <- indiv_long %>%
		tidyr::pivot_wider(
			names_from  = Time,
			values_from = Value)

	# ==========================================================================
	# 5. ALIGN COLUMN NAMES AND ORDER
	# ==========================================================================
	# Rename subject column to "Statistic" to match summary format
	indiv_wide <- indiv_wide %>% dplyr::rename(Statistic = !!rlang::sym(stat_col))

	# Sort by Statistic (subject ID) - numeric if possible, otherwise character
	indiv_wide <- indiv_wide %>%
		dplyr::mutate(.stat_numeric = suppressWarnings(as.numeric(Statistic))) %>%
		dplyr::arrange(
			# Sort by numeric if possible, otherwise by character
			dplyr::coalesce(.stat_numeric, Inf), # Numeric IDs first, sorted numerically
			Statistic                             # Then by character (for non-numeric IDs)
		) %>%
		dplyr::select(-.stat_numeric)  # Remove helper column

	# Ensure columns match summary_pivoted
	summary_cols <- names(summary_pivoted)
	cols_to_keep <- intersect(summary_cols, names(indiv_wide))

	# Reorder columns to match
	indiv_wide <- indiv_wide[, cols_to_keep, drop = FALSE]

	# ==========================================================================
	# 6. STACK WITH SECTION MARKERS AND SORT
	# ==========================================================================
	# Convert covariates to character to prevent list-column issues during rbind
	if (!is.null(covariates) && length(covariates) > 0) {
		summary_pivoted <- summary_pivoted %>%
			dplyr::mutate(dplyr::across(dplyr::all_of(covariates), as.character))
		indiv_wide <- indiv_wide %>%
			dplyr::mutate(dplyr::across(dplyr::all_of(covariates), as.character))
	}

	# Add Section column and stack (works with or without covariates)
	summary_pivoted$Section <- "SUMMARY"
	indiv_wide$Section      <- "INDIVIDUAL"
	final_result            <- rbind(summary_pivoted, indiv_wide)

	# Sort by covariates (if present), then by Section (SUMMARY before INDIVIDUAL)
	if (!is.null(covariates) && length(covariates) > 0) {
		final_result <- final_result %>%
			dplyr::mutate(Section = factor(Section, levels = c("SUMMARY", "INDIVIDUAL"))) %>%
			dplyr::arrange(dplyr::across(dplyr::all_of(covariates)), Section) %>%
			dplyr::mutate(Section = as.character(Section))
	}

	return(as.data.frame(final_result))
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
#' @param NCAparams A character vector of NCA parameter names to filter by.
#'   If `NULL`, all available parameters are retrieved.
#'   \emph{Applicable only when `by_time = FALSE`.}
#' @param covariates A character vector of covariate names to stratify the results by.
#'   If `NULL`, overall (non-stratified) statistics are calculated.
#' @param roundDigits Integer. The rounding precision (number of digits).
#'   Inherited from the parent table/listing function settings.
#' @param roundMethod Character string. The rounding method (e.g., "monolix" or "signif").
#'   Inherited from the parent table/listing function settings.
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
																		NCAparams = NULL,
																		covariates = NULL,
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
		if (!is.null(covariates)) {
			lixoftConnectors::setNCAResultsStratification(split = covariates)
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
		df_fixed <- .fixNCAConcentrationsColumnSwap(df, covariates)

		# Choose label column
		stat_col <- if ("metric" %in% names(df_fixed)) "metric" else "subject"

		# Keep only summary stats (filter out individual subjects)
		summary_only <- df_fixed %>%
			dplyr::filter(!grepl("^\\d+$", !!rlang::sym(stat_col)))

		# Pivot time columns to rows
		cols_to_pivot <- setdiff(names(df_fixed), c(covariates, stat_col))

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
			dplyr::arrange(dplyr::across(dplyr::any_of(covariates)), as.numeric(Time)) %>%
			dplyr::mutate(dplyr::across(dplyr::any_of(covariates), as.character))

		# Apply Rounding
		summary_stats <- .round_num(
			to_round        = summary_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = c("Time", covariates))

		# Add Combined Metrics
		summary_stats <- .combinedStats(summary_stats)

		# Note: units and aliases are not applicable for concentration data

		return(summary_stats)
	} else {
		# PARAMETER MODE: Original logic (calculate stats by parameter)
		# Set Stratification
		lixoftConnectors::setNCAResultsStratification() # reset
		if (!is.null(covariates)) {
			lixoftConnectors::setNCAResultsStratification(split = covariates)
		}

		# Get Summary Stats (from getNCAParameterStatistics)
		summary_stats <- lixoftConnectors::getNCAParameterStatistics(parameters = NCAparams)$statistics
		if (is.null(summary_stats)) {
			# Return NULL if no stats, parent function will handle
			return(NULL)
		}
		summary_stats$parameter <- as.character(summary_stats$parameter)

		# Get Individual Parameters & Stats
		indiv <- lixoftConnectors::getNCAIndividualParameters()$parameters

		# Identify grouping and pivoting structure
		grouping_vars <- c("id", covariates)

		# Keep only numeric columns + covariates + id
		indiv_long <- indiv %>%
			dplyr::select(dplyr::all_of(grouping_vars), dplyr::where(is.numeric)) %>%
			tidyr::pivot_longer(
				cols = -dplyr::all_of(grouping_vars),
				names_to = "parameter",
				values_to = "value") %>%
			dplyr::relocate(dplyr::any_of(covariates), id, parameter, value)

		# Summarize (handles stats automatically)
		if (is.null(covariates)) {
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
				group_vars = c(covariates, "parameter"),
				value_col  = "value",
				id_col     = "id") %>%
				dplyr::select(dplyr::all_of(c(covariates, "parameter")), P05, Q1, median, Q3, P95, max) %>%
				dplyr::arrange(dplyr::across(dplyr::all_of(c(covariates, "parameter"))))
		}

		# Merge Summaries
		join_cols <- c("parameter", covariates)
		summary_stats <- dplyr::left_join(summary_stats, descr_stats, by = join_cols)

		# Apply Rounding & Combining
		summary_stats <- .round_num(
			to_round        = summary_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = c("parameter", covariates))

		summary_stats <- .combinedStats(summary_stats)

		# Apply Labels (Units/Aliases only)
		if (isTRUE(units) || isTRUE(aliases)) {
			summary_stats <- .addUnitsAliases(summary_stats, add_units = units, add_aliases = aliases)
		}

		return(summary_stats)
	}
}


##################################################################################
#' calculate_raw_freq
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
#' covariates. It first counts the total number of distinct subjects (IDs) for
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
.calculate_raw_freq <- function(df_input) {
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
#' format_frequency_table
#'
#' Formats a frequency table by applying rounding and creating a combined
#' display metric showing count and percentage together as "N (%)".
#'
#' @param summary_tbl A data frame containing frequency statistics, typically
#'   the output from `.calculate_raw_freq()`. Must contain columns `Count` and
#'   `Percentage`.
#' @param digits Integer specifying the number of decimal places for rounding
#'   the percentage values.
#' @param method Character string specifying the rounding method. Either
#'   `"monolix"` (adaptive precision) or `"signif"` (significant figures).
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
#' # .format_frequency_table(freq_tbl, digits = 1, method = "monolix", exclude_cols = c("variable", "Modalities", "N"))
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
#' @param method Character string specifying the rounding method ("monolix" or "signif").
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
#' for categorical covariates in summary reports.
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
#' # .calc_format_stratum(df, digits = 1, method = "monolix", exclude_cols = c("variable", "Modalities", "N"))
#'
.calc_format_stratum <- function(df_input, digits, method, exclude_cols) {
	raw_stats <- .calculate_raw_freq(df_input)
	final_stats <- .format_frequency_table(raw_stats, digits, method, exclude_cols)
	return(final_stats)
}


##################################################################################
#' mockOutputData
#'
#' Masks sensitive data in output tables by replacing numeric values and counts
#' with placeholder patterns (e.g., "xx.xxx"). Used for creating example outputs
#' or documentation without revealing actual study data.
#'
#' @param df A data frame containing the table to be mocked.
#' @param mock_settings A list with the following elements:
#'   \itemize{
#'     \item `enable`: Logical. If `FALSE`, the function returns the input unchanged.
#'     \item `exclude_cols`: Character vector of column names to exclude from mocking
#'       (e.g., parameter names, variable labels).
#'     \item `max_rows`: Numeric or `NULL`. Maximum number of rows to include in output.
#'       If `NULL` or `"all"`, all rows are included.
#'   }
#' @param digits Integer specifying the number of decimal places, used to determine
#'   the pattern for mocked values (e.g., digits = 3 produces "xx.xxx").
#' @param method Character string specifying the rounding method. Passed to
#'   `.roundNumericCols()` but primarily used for consistency with other functions.
#'
#' @details
#' This function performs three main operations:
#'
#' **1. Mock Data Cells**: Replaces numeric values with placeholder patterns:
#' \itemize{
#'   \item Simple numbers: "xx.xxx" (based on `digits` parameter)
#'   \item Combined metrics: "xx.xxx (xx.xxx)" for values containing parentheses
#'   \item Preserves NA values as-is
#' }
#'
#' **2. Mock Column Headers**: Masks stratification information in column names:
#' \itemize{
#'   \item Modality values: "DOSE=100" becomes "DOSE=xx"
#'   \item Subject counts: "(N=25)" becomes "(N=xx)"
#' }
#'
#' **3. Limit Rows**: Optionally truncates the output to a maximum number of rows
#' as specified in `mock_settings$max_rows`.
#'
#' The function intelligently detects whether a column contains numeric data or
#' combined metrics (with parentheses) and applies the appropriate masking pattern.
#'
#' @return A data frame with the same structure as the input but with:
#'   \itemize{
#'     \item Numeric values replaced with "xx" patterns
#'     \item Column headers masked if they contain stratification info
#'     \item Rows limited to `max_rows` if specified
#'   }
#'   Returns the original data frame unchanged if `mock_settings$enable` is `FALSE`.
#'
#' @keywords internal
#'
#' @examples
#' # df <- data.frame(Parameter = c("AUC", "Cmax"),
#' #                  `Mean (SD)` = c("100.5 (20.3)", "50.2 (10.1)"),
#' #                  check.names = FALSE)
#' # mock_settings <- list(enable = TRUE, exclude_cols = "Parameter", max_rows = 10)
#' # .mockOutputData(df, mock_settings, digits = 1, method = "monolix")
#' # Result: Parameter column unchanged, Mean (SD) becomes "xx.x (xx.x)"
#'
.mockOutputData <- function(df, mock_settings, digits, method) {
	if (!isTRUE(mock_settings$enable)) {
		return(df)
	}

	df_mocked <- df
	exclude_cols <- mock_settings$exclude_cols %||% character(0)

	# Identify which columns need mocking
	cols_to_mock <- setdiff(names(df), exclude_cols)

	# --------------------------------------------------------------------------
	# PREPARE MASKS
	x_integer_part <- "xx"
	if (digits > 0) {
		x_decimal_part <- paste(rep("x", digits), collapse = "")
		x_base_number <- paste0(x_integer_part, ".", x_decimal_part)
	} else {
		x_base_number <- x_integer_part
	}
	x_combined_format <- paste0(x_base_number, " (", x_base_number, ")")

	n_rows <- nrow(df)

	# --------------------------------------------------------------------------
	# MOCK DATA CELLS

	for (col in cols_to_mock) {
		original_data <- df[[col]]

		# Check 1: Is it a combined string? e.g. "10 (2)"
		contains_combined <- any(grepl("\\(", original_data))

		# Check 2: Is it numeric OR does it look like a number?
		as_num <- suppressWarnings(as.numeric(original_data))
		looks_numeric <- is.numeric(original_data) || (!all(is.na(as_num) & !is.na(original_data)))

		if (contains_combined || looks_numeric) {
			# Create a baseline vector filled with the simple format (xx.xxx)
			new_vals <- rep(x_base_number, n_rows)

			# Identify specific rows that have the combined format (parentheses)
			# and overwrite those specific slots with the combined mask
			has_paren <- grepl("\\(", original_data)
			new_vals[has_paren] <- x_combined_format

			# Preserve NAs
			new_vals[is.na(original_data)] <- NA_character_

			# Assign back to df_mocked
			df_mocked[[col]] <- new_vals
		}
	}

	# --------------------------------------------------------------------------
	# MOCK HEADERS
	header_pattern <- "\\n\\(N=(\\d+)\\)$"
	new_names <- names(df_mocked)

	for (i in seq_along(new_names)) {
		col_name <- new_names[i]

		if (col_name %in% exclude_cols) next

		if (grepl(header_pattern, col_name)) {
			# Mask Modality
			if (grepl("=.*\\n", col_name)) {
				col_name <- sub("=(.*?)\\n", "=xx\n", col_name)
			}
			# Mask N Count
			col_name <- sub("N=(\\d+)", "N=xx", col_name)

			new_names[i] <- col_name
		}
	}
	names(df_mocked) <- new_names

	# --------------------------------------------------------------------------
	# LIMIT ROWS
	max_rows_setting <- mock_settings$max_rows

	# Logic: If NULL (not provided) or "all", show everything. Only limit if numeric.
	if (is.numeric(max_rows_setting)) {
		n_limit <- min(nrow(df), max_rows_setting)
	} else {
		# Default behavior (NULL, "all", or invalid input) -> Show ALL rows
		n_limit <- nrow(df)
	}

	df_mocked <- df_mocked[1:n_limit, , drop = FALSE]

	return(df_mocked)
}


##################################################################################
#' Helper: Build stratified table with covariate headers
#'
#' @param summary_stats Stratified summary statistics
#' @param overall_stats Overall summary statistics (optional)
#' @param covariates Covariate names
#' @param selected_metrics Selected metrics to include
#' @param param_col Parameter column name
#' @param covariate_labels Custom labels for covariates (optional)
#' @keywords internal
#' @noRd
.buildStratifiedTable <- function(summary_stats, overall_stats = NULL, covariates,
																	selected_metrics, param_col, covariate_labels = NULL) {
	if (!is.null(covariates)) {
		# Prepare covariate labels
		if (is.null(covariate_labels)) labels <- covariates else labels <- covariate_labels

		# Create header column combining covariates
		df_prepared <- summary_stats %>%
			dplyr::rowwise() %>%
			dplyr::mutate(header = {
				values <- dplyr::c_across(dplyr::all_of(covariates))
				pieces <- paste0(labels, "=", as.character(values))
				paste(pieces, collapse = "\n")
			}) %>%
			dplyr::ungroup()

		# Map headers to N counts
		n_per_stratum <- df_prepared %>%
			distinct(header, Nobs) %>%
			dplyr::mutate(header_N = paste0(header, "\n(N=", Nobs, ")"))

		# Prepare overall data if provided
		overall_prepared <- NULL
		if (!is.null(overall_stats)) {
			overall_prepared <- overall_stats %>%
				dplyr::select(!!rlang::sym(param_col), Nobs, dplyr::all_of(selected_metrics)) %>%
				dplyr::mutate(header_N = paste0("Overall\n(N=", Nobs, ")"))
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
		# No covariates - just Overall with N
		max_n <- max(summary_stats$Nobs, na.rm = TRUE)

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
#' @param covariates Covariate names to preserve
#' @keywords internal
#' @noRd
.pivotToVertical <- function(summary_stats, param_col, covariates = NULL) {
	stat_cols <- setdiff(names(summary_stats), c(param_col, covariates))
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
			id_cols     = c(dplyr::any_of(covariates), Statistic),
			names_from  = dplyr::all_of(param_col),
			values_from = Value
		) %>%
		dplyr::arrange(dplyr::across(dplyr::any_of(covariates)), Statistic) %>%
		dplyr::select(dplyr::any_of(covariates), Statistic, dplyr::all_of(as.character(param_order)))

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

			overall_label <- unique(overall_pivoted$header_N)
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
				dplyr::rename(!!unique(overall_renamed$header_N) := Overall) %>%
				dplyr::select(-header_N)
		}
	}

	return(result)
}
