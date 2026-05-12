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
			stop("custom_aliases/custom_units dataframe must have at least 2 columns.", call. = FALSE)
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
	stop("custom_aliases/custom_units must be a named vector or a 2-column dataframe.", call. = FALSE)
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
#' #
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
			!is.na(aliases) & !is.na(units) & units != "" ~ paste0(aliases, "\n(", units, ")"),
			!is.na(aliases) ~ aliases,
			is.na(aliases) & !is.na(units) & units != "" ~ paste0(parameter, "\n(", units, ")"),
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
		# For ratio tables: intersection of ratio columns and data variables
		# This excludes parameters (which only appear in ratio results, not in data)
		ratio_result <- lixoftConnectors::getNCAIndividualRatios()
		ratio_cols   <- setdiff(names(ratio_result), "id")
		# Get all variables from data
		data_vars <- lixoftConnectors::getData()$header
		# Available splitCovariates = intersection (parameters are filtered out)
		available_covs <- intersect(ratio_cols, data_vars)
		context_msg    <- "splitCovariates in ratio results"
	} else {
		# For other tables/listings: use getData(), allow catcov or occ
		project_data   <- lixoftConnectors::getData()
		available_covs <- project_data$header[project_data$headerTypes %in% c("catcov", "occ")]
		context_msg    <- "splitCovariates (catcov or occ)"
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
#' Internal helper to validate covariate_filter and id_filter arguments.
#' Checks that filter splitCovariates are categorical (catcov or occ) based on
#' the resultType. Does NOT validate that filter values exist in data
#' (that is handled by .applyFilters()).
#'
#' @param covariate_filter Named list where names are categorical covariate names
#'   and values are vectors of allowed values. Example:
#'   `list(DOSE = c("100mg", "200mg"), SEX = "Male")`. If NULL, no covariate
#'   filtering is applied.
#' @param id_filter Character or numeric vector of subject IDs to include.
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
#'   \item covariate_filter must be a named list (or NULL)
#'   \item Filter covariate names must be categorical splitCovariates (catcov or occ)
#'   \item id_filter must be a vector (or NULL)
#' }
#'
#' Note: This function does NOT validate that filter values exist in the data.
#' Invalid values will simply result in no rows matching (handled by .applyFilters()).
#'
#' @keywords internal
#'
.validateFilters <- function(covariate_filter = NULL,
                             id_filter        = NULL,
                             resultType       = c("nca", "ncaRatio")) {

	resultType <- match.arg(resultType)

	# Early return if no filters specified
	if (is.null(covariate_filter) && is.null(id_filter)) {
		return(invisible(NULL))
	}

	# --------------------------------------------------------------------------
	# Validate covariate_filter
	# --------------------------------------------------------------------------
	if (!is.null(covariate_filter)) {

		# Must be a named list
		if (!is.list(covariate_filter) || is.null(names(covariate_filter))) {
			stop("covariate_filter must be a named list. Example: list(DOSE = c('100mg', '200mg'), SEX = 'Male')",
					 call. = FALSE)
		}

		# Check for empty names
		if (any(names(covariate_filter) == "")) {
			stop("All elements in covariate_filter must be named.", call. = FALSE)
		}

		# Get available categorical splitCovariates based on resultType
		if (resultType == "ncaRatio") {
			# For ratio tables: intersection of ratio columns and data variables
			ratio_result       <- lixoftConnectors::getNCAIndividualRatios()
			ratio_cols         <- setdiff(names(ratio_result), "id")
			data_vars          <- lixoftConnectors::getData()$header
			available_cat_covs <- intersect(ratio_cols, data_vars)
		} else {
			# For nca and ncaConcentration: categorical splitCovariates and occasions
			project_data       <- lixoftConnectors::getData()
			available_cat_covs <- project_data$header[project_data$headerTypes %in% c("catcov", "occ")]
		}

		# Check that filter splitCovariates are valid categorical splitCovariates
		filter_covs  <- names(covariate_filter)
		invalid_covs <- setdiff(filter_covs, available_cat_covs)

		if (length(invalid_covs) > 0) {
			# Check if they're continuous splitCovariates (more specific error message)
			project_data <- lixoftConnectors::getData()
			cont_covs    <- project_data$header[project_data$headerTypes == "contcov"]
			invalid_cont <- intersect(invalid_covs, cont_covs)

			if (length(invalid_cont) > 0) {
				stop(sprintf(
					"covariate_filter can only filter categorical covariates (catcov or occ).\n'%s' %s continuous covariate%s and cannot be filtered.\nAvailable categorical covariates: %s",
					paste(invalid_cont, collapse = "', '"),
					if (length(invalid_cont) > 1) "are" else "is a",
					if (length(invalid_cont) > 1) "s" else "",
					if (length(available_cat_covs) > 0) paste(available_cat_covs, collapse = ", ") else "none"), call. = FALSE)
			} else {
				stop(sprintf(
					"Invalid covariate%s in covariate_filter: %s\nAvailable categorical covariates: %s",
					if (length(invalid_covs) > 1) "s" else "",
					paste(invalid_covs, collapse = ", "),
					if (length(available_cat_covs) > 0) paste(available_cat_covs, collapse = ", ") else "none"), call. = FALSE)
			}
		}
	}

	# --------------------------------------------------------------------------
	# Validate id_filter
	# --------------------------------------------------------------------------
	if (!is.null(id_filter)) {

		# Must be a vector (character or numeric)
		if (!is.vector(id_filter) || is.list(id_filter)) {
			stop("id_filter must be a character or numeric vector of IDs.", call. = FALSE)
		}

		# Check it's not empty
		if (length(id_filter) == 0) {
			stop("id_filter cannot be an empty vector.", call. = FALSE)
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
#' @param covariate_filter Named list where names are categorical covariate names
#'   and values are vectors of allowed values. Example:
#'   `list(DOSE = c("100mg", "200mg"), SEX = "Male")`. If NULL, no covariate
#'   filtering is applied.
#' @param id_filter Character or numeric vector of subject IDs to include.
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
                          covariate_filter = NULL,
                          id_filter        = NULL,
                          id_col           = "id") {

	# Early return if no filters specified
	if (is.null(covariate_filter) && is.null(id_filter)) {
		return(data)
	}

	# Store original row count for messaging
	n_original <- nrow(data)

	# --------------------------------------------------------------------------
	# Apply ID filter
	# --------------------------------------------------------------------------
	if (!is.null(id_filter)) {
		if (id_col %in% names(data)) {
			# Convert both to character for consistent comparison
			data_ids <- as.character(data[[id_col]])
			filter_ids <- as.character(id_filter)

			data <- data[data_ids %in% filter_ids, , drop = FALSE]
		} else {
			warning(sprintf("ID column '%s' not found in data. ID filter not applied.", id_col),
							call. = FALSE)
		}
	}

	# --------------------------------------------------------------------------
	# Apply covariate filters
	# --------------------------------------------------------------------------
	if (!is.null(covariate_filter)) {
		for (cov_name in names(covariate_filter)) {
			if (cov_name %in% names(data)) {
				filter_values <- covariate_filter[[cov_name]]

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
			"Filter resulted in empty dataset (0 of %d rows remaining).\nCheck your covariate_filter and id_filter values.",
			n_original
		), call. = FALSE)
	}

	return(data)
}

##################################################################################
#' Internal helper to validate stratification arguments
#'
#' @param splitCovariates Vector of covariate names (or NULL).
#' @param stratify_columns Boolean flag indicating if stratification is requested.
#'
#' @return Boolean. Returns FALSE if splitCovariates are missing, otherwise returns original stratify_columns.
#'
.validateStratificationArgs <- function(splitCovariates, stratify_columns) {
	# 1. Argument consistency check
	if (isTRUE(stratify_columns) && is.null(splitCovariates)) {
		warning("stratify_columns=TRUE requires covariates. Resetting stratify_columns to FALSE.")
		return(FALSE)
	}
	# 2. Covariate existence check (if splitCovariates provided)
	if (!is.null(splitCovariates)) {
		# Get available splitCovariates from current PKanalix project
		cov_info                  <- lixoftConnectors::getCovariateInformation()$covariate
		available_splitCovariates <- setdiff(names(cov_info), "id")

		# Validate requested splitCovariates exist
		bad_splitCovariates <- setdiff(splitCovariates, available_splitCovariates)
		if (length(bad_splitCovariates) > 0) {
			stop(
				"Invalid covariate(s): ", paste(bad_splitCovariates, collapse = ", "), "\n",
				"Available splitCovariates: ", paste(available_splitCovariates, collapse = ", ")
			)
		}
	}
	return(stratify_columns)
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
.validateNCAParametersSelection <- function(NCAparams, ratio = FALSE, be = FALSE) {
	# Skip validation if no parameters specified
	if (is.null(NCAparams)) {
		return(invisible(NULL))
	}
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
#' @param params Parameter names to include (NCAparams for parameters, ratioParams for ratios, NULL for all)
#' @param roundDigits Number of decimal places for rounding
#' @param roundMethod Rounding method ("monolix", "signif", or "none")
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
														roundMethod    = "monolix",
														units          = TRUE,
														units_df       = NULL,
														aliases        = TRUE,
														aliases_df     = NULL,
														useNominalTime = TRUE,
														indiv_data     = NULL
														) {

	resultType <- match.arg(resultType)

	# ==========================================================================
	# BRANCH 1: NCA Parameters and Ratios (similar logic)
	# ==========================================================================

	if (resultType %in% c("ncaParameters", "ncaRatio")) {

		# Get ALL splitCovariates from project (including occasions) to properly identify parameter columns
		project_data <- lixoftConnectors::getData()
		all_splitCovariates <- project_data$header[project_data$headerTypes %in% c("catcov", "contcov", "occ")]
		occ_vars <- project_data$header[project_data$headerTypes == "occ"]

		# Get individual data based on type
		if (resultType == "ncaParameters") {
			# Get parameter columns (exclude id and ALL splitCovariates including occasions)
			param_cols <- setdiff(names(indiv_data), c("id", all_splitCovariates))

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

		# Apply name mapping to Parameter column
		long_df$Parameter <- name_mapping[long_df$Parameter]

		# Check for occasions (duplicate id-covariate-parameter combinations)
		# Pass occ_vars explicitly - differs between ncaParameters and ncaRatio
		composite_result <- .createCompositeId(long_df, splitCovariates, occ_vars = occ_vars,
																					 value_group_col = "Parameter")
		long_df <- composite_result$data
		use_composite_id <- composite_result$composite_id_created

		# Determine which ID column to use
		id_col <- if (use_composite_id) "composite_id" else "id"

		# Round individual values
		long_df$Value <- .round_num(
			to_round        = data.frame(val = long_df$Value),
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = NULL)$val

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
		if (is.null(indiv_data)) {
			indiv_data <- listingNCAConcentration(
				projectFile    = NULL,  # Already loaded
				splitCovariates     = splitCovariates,
				useNominalTime = useNominalTime,
				roundDigits    = NULL,  # Don't round here, will round below
				roundMethod    = "none")
		}

		# Determine time column
		time_col <- if ("NominalTime" %in% names(indiv_data)) "NominalTime" else "ActualTime"

		# Round concentration values (raw_data from tableNCAConcentration is unrounded)
		indiv_data$UsedConc <- .round_num(
			to_round        = data.frame(val = indiv_data$UsedConc),
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = NULL)$val

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
#' @param NCAparams A character vector of NCA parameter names to filter by.
#'   If `NULL`, all available parameters are retrieved.
#'   \emph{Applicable only when `by_time = FALSE`.}
#' @param splitCovariates A character vector of covariate names to stratify the results by.
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
		summary_stats <- lixoftConnectors::getNCAParameterStatistics(parameters = NCAparams)$statistics
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
#' @param splitCovariates Covariate names
#' @param selected_metrics Selected metrics to include
#' @param param_col Parameter column name
#' @param splitCovariate_labels Custom labels for splitCovariates (optional)
#' @keywords internal
#' @noRd
.buildStratifiedTable <- function(summary_stats, overall_stats = NULL, splitCovariates,
																	selected_metrics, param_col, covariate_labels = NULL) {
	if (!is.null(splitCovariates)) {
		# Prepare covariate labels
		if (is.null(covariate_labels)) labels <- splitCovariates else labels <- covariate_labels

		# Create header column combining splitCovariates
		df_prepared <- summary_stats %>%
			dplyr::rowwise() %>%
			dplyr::mutate(header = {
				values <- dplyr::c_across(dplyr::all_of(splitCovariates))
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
		# No splitCovariates - just Overall with N
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
.addUnitsOnly <- function(df, orig_col = "Original_Parameter", param_col = "parameter") {
	# Retrieve all parameters from project
	allparams <- lixoftConnectors::getNCASettings()$computedncaparameters

	NCAParamSummary_info <- lixoftConnectors::getNCAParameterStatistics(parameters = allparams)$information %>%
		tibble::rownames_to_column(var = "parameter")

	# Check if units are enabled in project
	if (!isTRUE(lixoftConnectors::getDataSettings()$enableunits)) {
		warning("Units were requested but are not enabled in the current project. Skipping unit retrieval.")
		df[[orig_col]] <- NULL
		return(df)
	}

	# Extract and format units
	units_df <- NCAParamSummary_info %>%
		dplyr::select(parameter, units) %>%
		dplyr::mutate(units = sapply(units, .formatUnits))

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
