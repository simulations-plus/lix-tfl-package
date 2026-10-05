################################################################################
#-------------------Quarto Report: Non Compartmental analysis------------------#
#-----------------------------PKanalix Helpers---------------------------------#
################################################################################

################################################################################
# Public Helper Functions for PKanalix
# Helpers for Tables

#' tableNCAConcentration
#'
#' Generates a summary table of concentration data from NCA analysis, with optional
#' stratification by splitCovariates. Transforms concentration data from wide to long format
#' and provides summary statistics across time points.
#'
#' @param projectFile Character string specifying the path to a PKanalix project file (.pkx).
#'   If `NULL` (default), uses the currently loaded project. The function will load the
#'   specified project if it differs from the currently active one.
#'
#' @param splitCovariates Character vector of covariate names to stratify results by
#'   (e.g., `c("DOSE", "FOOD")`). If `NULL` (default), no stratification is applied
#'   and overall summary statistics are returned.
#'
#' @param useNominalTime Logical; if `TRUE` (default), the function attempts to use
#'   nominal time for the summary table. If nominal time is not available in the
#'   project (checked via `lixoftConnectors::getData()` and `headerTypes == "nominaltime"`),
#'   the function will fall back to actual time and issue a warning.
#'
#' @param metrics Character vector specifying which summary statistics to include in the
#'   output table. If `NULL` (default), all available metrics are included. Common options
#'   include: `"mean"`, `"SD"`, `"CV"`, `"geoMean"`, `"geoCV"`, `"geoSD"`, `"MeanSD"`,
#'   `"GeomCV"`, `"GeomSD"`, `"median"`, `"min"`, `"max"`, `"P05"`, `"P95"`, `"Nobs"`.
#'
#' @param stratifyColumns Logical; if `TRUE`, creates a stratified table layout with
#'   separate columns for each covariate combination plus an "Overall" column. If `FALSE`
#'   (default), splitCovariates appear as separate columns on the left side of the table.
#'   Only applicable when `splitCovariates` is specified.
#'
#' @param covariateLabels Character vector of custom labels for splitCovariates, must be the
#'   same length as `splitCovariates`. If `NULL` (default), the original covariate names are
#'   used. These labels appear in column headers when `stratifyColumns = TRUE`.
#'
#' @param useWideFormat Logical. If `TRUE` (default), returns a "wide" table format where
#'   time points appear as columns and statistics appear as rows (one row per statistic).
#'   Layout: `Statistic | [Covariates] | Time_0 | Time_1 | Time_2 | ...`
#'   If `FALSE`, returns a "long" table format where time points appear as rows and
#'   statistics appear as columns (one column per statistic).
#'   Layout: `Time | [Covariates] | mean | SD | median | ...`
#'   Note: `stratifyColumns = TRUE` is only compatible with `useWideFormat = FALSE`.
#'
#' @param maxTimeDeviation Numeric value specifying the maximum allowed percentage
#'   deviation between actual time and nominal time. If `NULL` (default), no filtering
#'   is applied. When set (e.g., `20` for 20%), observations where
#'   `abs((ActualTime - NominalTime) / NominalTime) * 100` exceeds this threshold
#'   are excluded from summary statistics. Only applies when `useNominalTime = TRUE`.
#'   Observations at `NominalTime = 0` are always retained. Individual data (when
#'   `includeId = TRUE`) is not affected by this filter. When observations are
#'   excluded, attributes are attached to the output for footnote generation:
#'   `timeDeviationFiltered`, `timeDeviationThreshold`, and `timeDeviationExcluded`.
#'
#' @param includeId Logical. If `TRUE`, adds individual subject IDs and their
#'   concentration values at each time point below the summary statistics. When
#'   stratified by splitCovariates, individual IDs are grouped by their covariate values.
#'   Default is `FALSE`. Only applicable when `stratifyColumns = FALSE`.
#'
#' @param roundDigits Integer specifying the number of decimal places for rounding
#'   numeric values. Default is `3`.
#'
#' @param roundMethod Character string specifying the rounding method. Options are:
#'   - `"monolixSuiteRounding"` (default): Adaptive rounding that keeps all digits before the
#'     decimal untouched and rounds only the fractional part to the specified number of
#'     significant digits (adds extra digits for values < 1)
#'   - `"significantFigures"`: Standard significant figures rounding - zeroes non-significant
#'     digits before the decimal for large numbers
#'   - `"significantFiguresPreserved"`: Modified significant figures rounding that never zeroes
#'     digits before the decimal - once the integer part already has as many digits as requested,
#'     the value is simply rounded to the nearest whole number
#'   - `"none"`: No rounding performed
#'
#' @param prettyLabels Logical; if `TRUE` (default), converts column names to
#'   human-readable labels (e.g., "CV" becomes "CV (%)", "geoMean" becomes "Geometric Mean").
#'   If `FALSE`, uses raw column names.
#'
#' @param mock Logical; if `TRUE`, masks the table for documentation / example
#'   outputs: concentration statistic values become placeholders (e.g.
#'   `"xx.xx"`), and `splitCovariates` values become generic labels
#'   (`"Category 1"`, ...). The time column/headers and `Statistic`/`Section`
#'   labels are kept, except that with `includeId = TRUE` the individual
#'   subject IDs listed under `Statistic` are relabeled (`"id1"`, `"id2"`,
#'   ...), since those are real identifiers. Default `FALSE`.
#'
#' @details
#' The function resets stratification via `lixoftConnectors::setNCAResultsStratification()` and
#' optionally splits results by `splitCovariates`. Concentration data are retrieved
#' with `lixoftConnectors::getNCAConcentrations()`, using `useNominalTime` only if nominal
#' time is set and available. Covariate column order is corrected with
#' `fixNCAConcentrationsColumnSwap()`. Only summary rows (statistics) are kept,
#' excluding individual profiles. Data are reshaped into long format, numeric values
#' are rounded, and the results are returned in wide format with one row per
#' timepoint/covariate combination.
#'
#' @return A data frame containing concentration summary statistics organized by time points
#'         and optionally stratified by splitCovariates.
#'
#' @examples
#' # Overall summary (no stratification)
#' # tableNCAConcentration()
#'
#' # Stratified by treatment, all metrics
#' # tableNCAConcentration(splitCovariates = "TREAT")
#'
#' # Stratified by multiple splitCovariates with column layout
#' # tableNCAConcentration(
#' #   splitCovariates = c("DOSE", "FOOD"),
#' #   stratifyColumns = TRUE
#' # )
#'
#' # Custom metrics and labels
#' # tableNCAConcentration(
#' #   splitCovariates = c("SEX", "AGE_CAT"),
#' #   metrics = c("GeomCV", "MeanSD"),
#' #   covariateLabels = c("Sex", "Age Category"),
#' #   stratifyColumns = TRUE
#' # )
#'
#' # With individual concentrations included
#' # tableNCAConcentration(
#' #   includeId = TRUE,
#' #   metrics = c("mean", "SD"),
#' #   useWideFormat = FALSE
#' # )
#'
#' @export
tableNCAConcentration <- function(projectFile      = NULL,
																	splitCovariates  = NULL,
																	useNominalTime   = TRUE,
																	metrics          = NULL,
																	stratifyColumns = FALSE,
																	covariateLabels = NULL,
																	useWideFormat  = TRUE,
																	maxTimeDeviation = NULL,
																	includeId       = FALSE,
																	filterCovariates = NULL,
																	filterIds        = NULL,
																	roundDigits      = 3,
																	roundMethod      = c("monolixSuiteRounding", "significantFigures", "significantFiguresPreserved", "none"),
																	prettyLabels     = TRUE,
																	mock             = FALSE) {

	roundMethod <- match.arg(roundMethod)
	# ==========================================================================
	# VALIDATION & SETUP

	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "nca")

	# Validate splitCovariates, stratification and covariate label arguments
	.validateCovariateSelection(splitCovariates, ratio = FALSE)
	stratifyColumns <- .validateStratificationArgs(splitCovariates, stratifyColumns)
	covariateLabels <- .validateCovariateLabels(splitCovariates, covariateLabels)

	# Validate covariate and id filters
	.validateFilters(filterCovariates, filterIds, resultType = "nca")
	# Initialize time deviation attributes container (for footnote generation)
	.time_dev_attrs <- NULL

	# Validate: stratifyColumns only works with long format
	if (stratifyColumns && useWideFormat) {
		message("stratifyColumns = TRUE requires long format, but useWideFormat = TRUE was set; ignoring stratifyColumns. To stratify, set useWideFormat = FALSE.")
		stratifyColumns <- FALSE
	}
	# Validate combination of includeId and useWideFormat setting
	if (includeId && !useWideFormat) {
		warning("includeId = TRUE only works with useWideFormat = TRUE. Setting includeId to FALSE.")
		includeId <- FALSE
	}
	# Validate includeId and stratifyColumns setting
	if (includeId && stratifyColumns) {
		warning("includeId = TRUE is not compatible with stratifyColumns = TRUE. Setting includeId to FALSE.")
		includeId <- FALSE
	}

	# ==========================================================================
	# GET RAW INDIVIDUAL DATA

	# Get individual concentration data using listingNCAConcentration
	# Don't apply rounding yet (will be done after calculating stats)
	raw_data  <- listingNCAConcentration(
		projectFile    = NULL, # project already loaded
		splitCovariates= splitCovariates,
		useNominalTime = useNominalTime,
		roundDigits    = NULL,
		roundMethod    = "none")

	# Apply covariate and ID filters
	raw_data <- .applyFilters(raw_data, filterCovariates, filterIds, id_col = "id")

	# Keep full data for individual listing
	raw_data_full <- raw_data

	# Detect occasion variables from project
	project_data <- lixoftConnectors::getData()
	occ_vars <- project_data$header[project_data$headerTypes == "occ"]

	# Apply time deviation filter only when:
	# 1. Grouping by NominalTime (useNominalTime = TRUE)
	# 2. Threshold is set
	if (useNominalTime && !is.null(maxTimeDeviation)) {
		if (all(c("NominalTime", "ActualTime") %in% names(raw_data))) {
			raw_data <- .filterByTimeDeviation(raw_data, occs = occ_vars, maxTimeDeviation)
			# Store attributes to attach to final output later
			.time_dev_attrs <- list(
				filtered  = attr(raw_data, "timeDeviationFiltered"),
				threshold = attr(raw_data, "timeDeviationThreshold"),
				excluded  = attr(raw_data, "timeDeviationExcluded"))
		} else {
			warning("Cannot apply time deviation filter: both NominalTime and ActualTime required.")
		}
	}

	# Determine which time column to use for grouping
	time_col <- if ("NominalTime" %in% names(raw_data)) "NominalTime" else "ActualTime"

	# ==========================================================================
	# CALCULATE DESCRIPTIVE STATISTICS

	# For stratified analysis, calculate stats separately by splitCovariates
	# For non-stratified, calculate overall stats (may still include splitCovariates as grouping)
	if (stratifyColumns && !is.null(splitCovariates)) {
		# STRATIFIED CASE: Calculate stats for each covariate combination
		group_by_vars <- c(time_col, splitCovariates)

		summary_stats <- .calculateDescriptiveStats(
			data      = raw_data,
			value_col = "UsedConc",  # Use the concentration values from getPointsIncludedForLambdaZ
			id_col    = "id",
			group_by  = group_by_vars)

		# Apply rounding to summary_stats
		summary_stats <- .round_num(
			to_round        = summary_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = c(time_col, splitCovariates))

		# Add combined metrics to summary_stats
		summary_stats <- .combinedStats(summary_stats)

		# Also calculate overall stats (no covariate grouping)
		overall_stats <- .calculateDescriptiveStats(
			data      = raw_data,
			value_col = "UsedConc",
			id_col    = "id",
			group_by  = time_col)  # Only group by time

		# Apply rounding to overall_stats
		overall_stats <- .round_num(
			to_round        = overall_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = time_col)

		# Add combined metrics to overall_stats
		overall_stats <- .combinedStats(overall_stats)

		# Validate Metrics existence
		available_cols   <- names(summary_stats)
		descriptor_cols  <- c(time_col, splitCovariates)  # N/Nobs are optional metrics
		selected_metrics <- .validateAndSelectMetrics(available_cols, metrics, descriptor_cols)

		# Use internal helper to build stratified table
		final_df <- .buildStratifiedTable(
			summary_stats    = summary_stats,
			overall_stats    = overall_stats,
			splitCovariates       = splitCovariates,
			selected_metrics = selected_metrics,
			param_col        = time_col,
			covariateLabels = covariateLabels)

		# Apply pretty labels to the Statistic column (metric names)
		if (isTRUE(prettyLabels) && "Statistic" %in% names(final_df)) {
			stat_metrics    <- unique(final_df$Statistic)
			dummy_df        <- as.data.frame(matrix(NA, nrow = 1, ncol = length(stat_metrics)))
			names(dummy_df) <- stat_metrics
			dummy_df        <- .prettyLabels(dummy_df)
			metric_map      <- setNames(names(dummy_df), stat_metrics)
			final_df$Statistic <- metric_map[final_df$Statistic]
		}

	} else {
		# ----------------------------------------------------------------------
		# CASE: No Stratification
		# ----------------------------------------------------------------------

		# Calculate stats with splitCovariates as columns (if present)
		group_by_vars <- if (!is.null(splitCovariates)) c(time_col, splitCovariates) else time_col

		summary_stats <- .calculateDescriptiveStats(
			data      = raw_data,
			value_col = "UsedConc",
			id_col    = "id",
			group_by  = group_by_vars)

		# Apply rounding to numeric columns (exclude time and splitCovariates)
		summary_stats <- .round_num(
			to_round        = summary_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = c(time_col, splitCovariates))

		# Add MeanSD, GeomCV, GeomSD columns
		summary_stats <- .combinedStats(summary_stats)

		# Validate and select metrics
		descriptor_cols  <- c(time_col, splitCovariates)
		selected_metrics <- .validateAndSelectMetrics(names(summary_stats), metrics, descriptor_cols)

		# Filter columns
		cols_to_keep <- intersect(c(descriptor_cols, selected_metrics), names(summary_stats))
		final_df     <- summary_stats[, cols_to_keep, drop = FALSE]

		# ==========================================================================
		# FORMAT: WIDE vs LONG
		# ==========================================================================

		if (useWideFormat) {
			# ======================================================================
			# WIDE FORMAT: Statistic | [Covariates] | Time_0 | Time_1 | Time_2 | ...
			# ======================================================================

			# Rename time column to "Time" for consistent handling
			names(final_df)[names(final_df) == time_col] <- "Time"
			time_col_name <- "Time"

			# Identify metric columns (everything except Time and splitCovariates)
			stat_cols <- setdiff(names(final_df), c("Time", splitCovariates))

			# Convert all metric columns to character (handles mixed numeric/character like MeanSD)
			final_df <- final_df %>%
				dplyr::mutate(dplyr::across(dplyr::all_of(stat_cols), as.character))

			# Pivot longer (stack metrics), then wider (time as columns)
			final_df <- final_df %>%
				tidyr::pivot_longer(
					cols      = dplyr::all_of(stat_cols),
					names_to  = "Statistic",
					values_to = "Value") %>%
				dplyr::filter(!is.na(Time) & Time != "") %>%  # Remove empty time values
				tidyr::pivot_wider(
					names_from  = Time,
					values_from = Value)

			# ----------------------------------------------------------------------
			# Add individual IDs if requested
			# ----------------------------------------------------------------------
			if (includeId) {
				final_df <- .addIndividuals(
					summary_stats  = final_df,
					splitCovariates     = splitCovariates,
					resultType     = "ncaConcentration",
					params         = NULL,
					roundDigits    = roundDigits,
					roundMethod    = roundMethod,
					useNominalTime = useNominalTime,
					indiv_data     = raw_data_full)  # Pass existing indiv conc data to avoid redundant fetch (full dataset (before potential filtering))
			}

			# ----------------------------------------------------------------------
			# Apply pretty labels for wide format
			# ----------------------------------------------------------------------
			if (isTRUE(prettyLabels)) {
				# Get time columns (numeric column names, excluding Statistic, Section, and splitCovariates)
				exclude_cols <- c("Statistic", "Section", splitCovariates)
				time_cols <- setdiff(names(final_df), exclude_cols)

				# Apply "Time = " prefix to time columns
				if (length(time_cols) > 0) {
					time_map <- setNames(paste0("Time = ", time_cols), time_cols)
					cols_to_rename <- intersect(time_cols, names(final_df))
					names(final_df)[names(final_df) %in% cols_to_rename] <- time_map[cols_to_rename]
				}

				# Apply pretty labels to Statistic column content
				if ("Statistic" %in% names(final_df)) {
					# Only apply to metrics (SUMMARY section if Section exists), not to individual IDs
					if ("Section" %in% names(final_df)) {
						summary_metrics <- unique(final_df$Statistic[final_df$Section == "SUMMARY"])
					} else {
						summary_metrics <- unique(final_df$Statistic)
					}

					# Create mapping for metrics
					dummy_df <- as.data.frame(matrix(NA, nrow = 1, ncol = length(summary_metrics)))
					names(dummy_df) <- summary_metrics
					dummy_df <- .prettyLabels(dummy_df)
					metric_map <- setNames(names(dummy_df), summary_metrics)

					# Apply to appropriate rows
					if ("Section" %in% names(final_df)) {
						final_df$Statistic[final_df$Section == "SUMMARY"] <-
							metric_map[final_df$Statistic[final_df$Section == "SUMMARY"]]
					} else {
						final_df$Statistic <- metric_map[final_df$Statistic]
					}
				}
			}

			# Set table attributes for style_tfl
			attr(final_df, "table_type") <- "concentration"
			attr(final_df, "format")     <- "wide"

		} else {
			# ======================================================================
			# LONG FORMAT: Time | [Covariates] | mean | SD | median | ...
			# ======================================================================
			# Data is already in long format, just apply pretty labels and sorting

			if (isTRUE(prettyLabels)) {
				final_df <- .prettyLabels(final_df)
			}

			# Sort by splitCovariates (if present and not stratified), then by Time
			if (!stratifyColumns && !is.null(splitCovariates) && length(splitCovariates) > 0) {
				final_df <- final_df %>%
					dplyr::arrange(dplyr::across(dplyr::all_of(splitCovariates)),
												 suppressWarnings(as.numeric(!!rlang::sym(time_col))))
			} else if (time_col %in% names(final_df)) {
				final_df <- final_df %>%
					dplyr::arrange(suppressWarnings(as.numeric(!!rlang::sym(time_col))))
			}
		}
	}

	# ==========================================================================
	# FINALIZE OUTPUT

	# Convert to data.frame first
	final_df <- as.data.frame(final_df)

	# Attach time deviation attributes AFTER as.data.frame() conversion
	if (!is.null(.time_dev_attrs) && isTRUE(.time_dev_attrs$filtered)) {
		attr(final_df, "timeDeviationFiltered")  <- .time_dev_attrs$filtered
		attr(final_df, "timeDeviationThreshold") <- .time_dev_attrs$threshold
		attr(final_df, "timeDeviationExcluded")  <- .time_dev_attrs$excluded
	}

	if (isTRUE(mock)) {
		modality_cols <- list()
		if (!stratifyColumns && !is.null(splitCovariates)) {
			for (sc in splitCovariates) modality_cols[[sc]] <- NA_character_
		}

		final_df <- .mockOutputData(
			df           = final_df,
			digits       = roundDigits,
			excludeCols  = c(time_col, "Statistic", "Section"),
			modalityCols = if (length(modality_cols)) modality_cols else NULL,
			naFillAxis   = if (stratifyColumns || useWideFormat) "row" else "column")

		final_df <- .mockIndividualSection(final_df)
	}

	# ==========================================================================
	# SET ATTRIBUTES for style_tfl
	# To recognize this as NCA concentration table with sections
	if (includeId && "Section" %in% names(final_df)) {
		attr(final_df, "table_type") <- "nca_concentration"
		attr(final_df, "format")     <- "long"  # Always long when includeId=TRUE
	}
	attr(final_df, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(final_df, "r_source")       <- .getRScriptPath()

	return(final_df)

}


#-------------------------------------------------------------------------------
#' tableNCAParameters
#'
#' Generates a comprehensive summary table of NCA parameters including both standard
#' statistics from PKanalix and additional descriptive statistics (quartiles, median, max).
#' Combines individual parameter data with summary statistics for complete reporting.
#'
#' @param projectFile Character string specifying the path to a PKanalix project file (.pkx).
#'   If `NULL` (default), uses the currently loaded project. The function will load the
#'   specified project if it differs from the currently active one.
#'
#' @param splitCovariates Character vector of covariate names to stratify by
#'   (e.g., `"TREAT"`, `"FOOD"`, or `c("TREAT","FOOD")`). If `NULL`
#'   (default), no stratification is applied and overall summaries are returned.
#'
#' @param ncaParams Character vector of NCA parameter names to include. If
#'   `NULL` (default), all parameters currently enabled in the project are used
#'   (i.e., `lixoftConnectors::getNCASettings()$computedncaparameters`).
#'
#' @param metrics Character vector specifying which summary statistics to include in the
#'   output table. If `NULL` (default), all available metrics are included. Common options
#'   include: `"mean"`, `"SD"`, `"CV"`, `"geoMean"`, `"geoCV"`, `"geoSD"`, `"MeanSD"`,
#'   `"GeomCV"`, `"GeomSD"`, `"median"`, `"min"`, `"max"`, `"P05"`, `"P95"`, `"Nobs"`.
#'
#' @param useWideFormat Logical. If `TRUE` (default), returns a "wide" table with
#'   one row per covariate level (or overall) and one column per statistic for
#'   each parameter, i.e., PKanalix-like structure. If `FALSE`, returns a
#'   "vertical" table where \code{Statistic} forms the first column and the
#'   parameters form the subsequent columns (useful for reporting).
#'
#' @param stratifyColumns Logical; if `TRUE`, creates a stratified table layout with
#'   separate columns for each covariate combination plus an "Overall" column. If `FALSE`
#'   (default), splitCovariates appear as separate columns on the left side of the table.
#'   Only applicable when `splitCovariates` is specified.
#'
#' @param covariateLabels Character vector of custom labels for splitCovariates, must be the
#'   same length as `splitCovariates`. If `NULL` (default), the original covariate names are
#'   used. These labels appear in column headers when `stratifyColumns = TRUE`.
#'
#' @param includeId Logical. If `TRUE` and `useWideFormat = FALSE`, adds individual
#'   subject IDs and their parameter values below the summary statistics in the
#'   "Statistic" column. When stratified by splitCovariates, individual IDs are grouped
#'   by their covariate values and listed after the corresponding summary metrics.
#'   Default is `FALSE`. Only works when `useWideFormat = FALSE`.
#'
#' @param roundDigits Integer specifying the number of decimal places for rounding
#'   numeric values. Default is `3`.
#'
#' @param roundMethod Character string specifying the rounding method. Options are:
#'   - `"monolixSuiteRounding"` (default): Adaptive rounding that keeps all digits before the
#'     decimal untouched and rounds only the fractional part to the specified number of
#'     significant digits (adds extra digits for values < 1)
#'   - `"significantFigures"`: Standard significant figures rounding - zeroes non-significant
#'     digits before the decimal for large numbers
#'   - `"significantFiguresPreserved"`: Modified significant figures rounding that never zeroes
#'     digits before the decimal - once the integer part already has as many digits as requested,
#'     the value is simply rounded to the nearest whole number
#'   - `"none"`: No rounding performed
#'
#' @param roundByParameter Optional per-parameter rounding overrides on top of the
#'   global `roundDigits`/`roundMethod`. Either a named numeric vector of digits
#'   (`c(Cmax = 2, AUClast = 4)`; method falls back to `roundMethod`), or a
#'   data.frame with columns `parameter`, `digits` and/or `method`
#'   (`NA`/missing entries fall back to the global settings). Parameters not
#'   listed keep the global settings. Unknown parameter names are ignored with a
#'   message. Default `NULL` (global rounding everywhere).
#'
#' @param units Logical. If `TRUE` (default), units are appended to parameter names.
#'
#' @param aliases Logical. If `TRUE` (default), parameter aliases (friendly names) are used
#'   instead of internal parameter codes.
#'
#' @param customAliases Named vector or 2-column dataframe mapping parameter names
#'   to custom alias labels. Example: `c(Cmax = "C[[sub:max]]", AUClast = "AUC[[sub:0-tlast]]")`.
#'   Supports special formatting syntax: `[[sub:text]]` for subscript, `[[sup:text]]` for
#'   superscript, and `~text~` for italic.
#'
#' @param customUnits Named vector or 2-column dataframe mapping parameter names
#'   to custom unit labels. Example: c(Cmax = "ng/mL", AUClast = "h*ng/mL")
#'
#' @param prettyLabels Logical; if `TRUE` (default), converts column names to
#'   human-readable labels (e.g., "CV" becomes "CV (%)", "geoMean" becomes "Geometric Mean").
#'   If `FALSE`, uses raw column names.
#'
#' @param mock Logical; if `TRUE`, masks the table for documentation / example
#'   outputs: metric values become placeholders (e.g. `"xx.xx"`), and
#'   `splitCovariates` values become generic labels (`"Category 1"`, ...). The
#'   parameter name and `Statistic`/`Section` labels are kept, except that with
#'   `includeId = TRUE` the individual subject IDs listed under `Statistic`
#'   are relabeled (`"id1"`, `"id2"`, ...), since those are real identifiers.
#'   Default `FALSE`.
#'
#' @return A data frame combining standard NCA statistics with additional descriptive statistics
#'         (Q1, Q3, median, max) for the specified parameters. If `includeId = TRUE`,
#'         also includes individual parameter values for each subject.
#'
#' @examples
#' # Overall (no stratification), all computed parameters, wide layout
#' # tableNCAParameters()
#'
#' # Stratified by a categorical covariate, selected parameters, vertical layout
#' # tableNCAParameters(
#' #   splitCovariates = "FORM",
#' #   ncaParams  = c("AUCINF_obs", "AUClast", "Cmax"),
#' #   roundDigits = 2,
#' #   useWideFormat = FALSE
#' # )
#'
#' # Vertical layout with individual IDs included
#' # tableNCAParameters(
#' #   useWideFormat = FALSE,
#' #   includeId = TRUE,
#' #   ncaParams  = c("Cmax", "AUClast", "Lambda_z")
#' # )
#'
#' # Stratified with individual IDs
#' # tableNCAParameters(
#' #   splitCovariates = "TREAT",
#' #   useWideFormat = FALSE,
#' #   includeId = TRUE
#' # )
#'
#' @export
tableNCAParameters <- function(projectFile      = NULL,
															 splitCovariates  = NULL,
															 ncaParams        = NULL,
															 metrics          = NULL,
															 useWideFormat  = TRUE,
															 stratifyColumns = FALSE,
															 covariateLabels = NULL,
															 includeId       = FALSE,
															 filterCovariates = NULL,
															 filterIds        = NULL,
															 roundDigits      = 3,
															 roundMethod      = c("monolixSuiteRounding", "significantFigures", "significantFiguresPreserved", "none"),
															 roundByParameter = NULL,
															 units            = TRUE,
															 aliases          = TRUE,
															 customUnits     = NULL,
															 customAliases   = NULL,
															 prettyLabels     = TRUE,
															 mock             = FALSE) {

	roundMethod <- match.arg(roundMethod)

	# ==========================================================================
	# VALIDATION & SETUP

	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "nca")
	.validateCovariateSelection(splitCovariates, ratio = FALSE)

	stratifyColumns <- .validateStratificationArgs(splitCovariates, stratifyColumns)
	covariateLabels <- .validateCovariateLabels(splitCovariates, covariateLabels)

	.validateFilters(filterCovariates, filterIds, resultType = "nca")
	available_params <- .validateNCAParametersSelection(ncaParams, ratio = FALSE)

	round_overrides <- .normalizeRoundOverrides(roundByParameter, roundDigits, roundMethod)
	round_overrides <- .validateRoundOverrides(round_overrides, available_params, context = "tableNCAParameters")

	if (stratifyColumns && useWideFormat) {
		message("stratifyColumns = TRUE requires long format, but useWideFormat = TRUE was set; ignoring stratifyColumns. To stratify, set useWideFormat = FALSE.")
		stratifyColumns <- FALSE
	}
	if (includeId && useWideFormat) {
		warning("includeId = TRUE only works with useWideFormat = FALSE. Setting includeId to FALSE.")
		includeId <- FALSE
	}
	if (includeId && stratifyColumns) {
		warning("includeId = TRUE is not compatible with stratifyColumns = TRUE. Setting includeId to FALSE.")
		includeId <- FALSE
	}

	# Normalize custom alias/unit mappings
	aliases_df <- .normalizeAliasMapping(customAliases, "aliases")
	units_df   <- .normalizeAliasMapping(customUnits, "units")

	# ==========================================================================
	# GET INDIVIDUAL PARAMETERS DATA

	indiv_params <- lixoftConnectors::getNCAIndividualParameters()$parameters


	# getNCAIndividualParameters() does not carry stratificationcategorical or additionalstratificationcategorical columns.
	# Get them from getCovariateInformation() + merge by id before filtering
	cov_info        <- lixoftConnectors::getCovariateInformation()
	strat_cov_names <- cov_info$name[cov_info$type %in% c("stratificationcategorical", "additionalstratificationcategorical")]

	if (length(strat_cov_names) > 0) {
		strat_cov_df <- cov_info$covariate[, c("id", strat_cov_names), drop = FALSE]
		indiv_params <- merge(indiv_params, strat_cov_df, by = "id", all.x = TRUE, sort = FALSE)
	}
	# --------------------------------------------------------------------------

	indiv_params <- .applyFilters(indiv_params, filterCovariates, filterIds, id_col = "id")

	project_data        <- lixoftConnectors::getData()
	all_splitCovariates <- project_data$header[project_data$headerTypes %in% c("catcov", "contcov", "occ")]
	occ_vars            <- project_data$header[project_data$headerTypes == "occ"]
	param_cols          <- setdiff(names(indiv_params), c("id", all_splitCovariates, strat_cov_names))

	if (!is.null(ncaParams)) {
		param_cols <- intersect(ncaParams, param_cols)
		if (length(param_cols) == 0) {
			stop("None of the specified ncaParams found in the individual parameters data.")
		}
	}

	# ==========================================================================
	# PREPARE LONG-FORMAT DATA

	cols_to_select <- c("id", splitCovariates, occ_vars, param_cols)
	cols_to_select <- intersect(cols_to_select, names(indiv_params))

	indiv_long <- indiv_params %>%
		dplyr::select(dplyr::all_of(cols_to_select)) %>%
		tidyr::pivot_longer(
			cols      = dplyr::all_of(param_cols),
			names_to  = "parameter",
			values_to = "value")

	# ==========================================================================
	# CALCULATE DESCRIPTIVE STATISTICS

	if (stratifyColumns) {
		if (is.null(metrics)) {
			user_metrics <- "GeomCV"
			message("No metric specified for stratified table. Defaulting to 'GeomCV'. To show multiple metrics specify metrics explicitly.")
		} else {
			user_metrics <- metrics
		}

		group_by_vars <- c("parameter", splitCovariates)

		summary_stats <- .calculateDescriptiveStats(
			data      = indiv_long,
			value_col = "value",
			id_col    = "id",
			group_by  = group_by_vars)

		summary_stats <- .applyRounding(
			df        = summary_stats,
			digits    = roundDigits,
			method    = roundMethod,
			exclude   = c("parameter", splitCovariates),
			overrides = round_overrides,
			paramCol  = "parameter",
			paramAxis = "row")

		summary_stats <- .combinedStats(summary_stats)

		if (isTRUE(units) || isTRUE(aliases)) {
			summary_stats <- .addUnitsAliases(
				summary_stats,
				add_units   = units,
				add_aliases = aliases,
				units_df    = units_df,
				aliases_df  = aliases_df)
		}

		param_col <- "parameter"
		descriptor_cols  <- c(param_col, splitCovariates)
		selected_metrics <- .validateAndSelectMetrics(
			available_cols = names(summary_stats),
			user_metrics   = user_metrics,
			descriptors    = descriptor_cols)

		if (!is.null(splitCovariates)) {
			overall_stats <- .calculateDescriptiveStats(
				data      = indiv_long,
				value_col = "value",
				id_col    = "id",
				group_by  = "parameter")

			overall_stats <- .applyRounding(
				df        = overall_stats,
				digits    = roundDigits,
				method    = roundMethod,
				exclude   = "parameter",
				overrides = round_overrides,
				paramCol  = "parameter",
				paramAxis = "row")

			overall_stats <- .combinedStats(overall_stats)

			if (isTRUE(units) || isTRUE(aliases)) {
				overall_stats <- .addUnitsAliases(
					overall_stats,
					add_units   = units,
					add_aliases = aliases,
					units_df    = units_df,
					aliases_df  = aliases_df)
			}

			if (!all(selected_metrics %in% names(overall_stats))) {
				stop("Mismatch in metrics between Stratified and Overall results.")
			}
		} else {
			overall_stats <- NULL
		}

		summary_stats <- .buildStratifiedTable(
			summary_stats    = summary_stats,
			overall_stats    = overall_stats,
			splitCovariates  = splitCovariates,
			selected_metrics = selected_metrics,
			param_col        = param_col,
			covariateLabels  = covariateLabels)

	} else {
		group_by_vars <- if (!is.null(splitCovariates)) c("parameter", splitCovariates) else "parameter"

		summary_stats <- .calculateDescriptiveStats(
			data      = indiv_long,
			value_col = "value",
			id_col    = "id",
			group_by  = group_by_vars)

		summary_stats <- .applyRounding(
			df        = summary_stats,
			digits    = roundDigits,
			method    = roundMethod,
			exclude   = c("parameter", splitCovariates),
			overrides = round_overrides,
			paramCol  = "parameter",
			paramAxis = "row")

		summary_stats <- .combinedStats(summary_stats)

		if (isTRUE(units) || isTRUE(aliases)) {
			summary_stats <- .addUnitsAliases(
				summary_stats,
				add_units   = units,
				add_aliases = aliases,
				units_df    = units_df,
				aliases_df  = aliases_df)
		}

		param_col <- "parameter"
		if (!(param_col %in% names(summary_stats))) {
			param_col <- intersect(names(summary_stats), c("parameter", "Parameter"))[1]
		}

		descriptor_cols  <- c(param_col, splitCovariates)
		selected_metrics <- .validateAndSelectMetrics(
			available_cols = names(summary_stats),
			user_metrics   = metrics,
			descriptors    = descriptor_cols)

		cols_to_keep  <- intersect(c(descriptor_cols, selected_metrics), names(summary_stats))
		summary_stats <- summary_stats[, cols_to_keep, drop = FALSE]

		if (!useWideFormat) {
			summary_stats <- .pivotToVertical(
				summary_stats = summary_stats,
				param_col     = param_col,
				splitCovariates    = splitCovariates)
		}
	}

	# ==========================================================================
	# ADD INDIVIDUAL IDs

	if (!useWideFormat && includeId) {
		summary_stats <- .addIndividuals(
			summary_stats = summary_stats,
			splitCovariates    = splitCovariates,
			resultType    = "ncaParameters",
			params        = ncaParams,
			roundDigits   = roundDigits,
			roundMethod   = roundMethod,
			roundOverrides = round_overrides,
			units         = units,
			units_df      = units_df,
			aliases       = aliases,
			aliases_df    = aliases_df,
			indiv_data    = indiv_params)
	}

	# ==========================================================================
	# SORT BY COVARIATES

	if (!useWideFormat && !stratifyColumns && !is.null(splitCovariates) && length(splitCovariates) > 0) {
		if (!"Section" %in% names(summary_stats)) {
			summary_stats <- summary_stats %>%
				dplyr::arrange(dplyr::across(
					dplyr::all_of(splitCovariates),
					~{
						num <- suppressWarnings(as.numeric(as.character(.x)))
						if(all(is.na(num))) .x else num
					}
				))
		}
	}

	# ==========================================================================
	# PRETTY LABELS & FINALIZATION

	if (isTRUE(prettyLabels)) {
		summary_stats <- .prettyLabels(summary_stats)

		if ("Statistic" %in% names(summary_stats)) {
			curr_metrics    <- unique(summary_stats$Statistic)
			dummy_df        <- as.data.frame(matrix(NA, nrow = 1, ncol = length(curr_metrics)))
			names(dummy_df) <- curr_metrics
			dummy_df        <- .prettyLabels(dummy_df)
			metric_map      <- setNames(names(dummy_df), curr_metrics)
			summary_stats$Statistic <- metric_map[summary_stats$Statistic]
		}
	}

	if (isTRUE(mock)) {
		param_col_final <- if (isTRUE(prettyLabels)) "Parameter" else "parameter"

		modality_cols <- list()
		if (!stratifyColumns && !is.null(splitCovariates)) {
			for (sc in splitCovariates) modality_cols[[sc]] <- NA_character_
		}

		summary_stats <- .mockOutputData(
			df           = summary_stats,
			digits       = roundDigits,
			excludeCols  = c(param_col_final, "Statistic", "Section"),
			modalityCols = if (length(modality_cols)) modality_cols else NULL,
			naFillAxis   = if (stratifyColumns || !useWideFormat) "row" else "column")

		summary_stats <- .mockIndividualSection(summary_stats)
	}

	if (includeId && "Section" %in% names(summary_stats)) {
		attr(summary_stats, "table_type") <- "nca_parameters"
		attr(summary_stats, "format")     <- "long"
	}
	attr(summary_stats, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(summary_stats, "r_source")       <- .getRScriptPath()
	return(as.data.frame(summary_stats))
}



#-------------------------------------------------------------------------------
#' tableNCARatios
#'
#' Create a summary table of individual NCA ratio results with optional
#' stratification by splitCovariates (e.g., DOSE, VISIT).
#'
#' The function:
#'  1. Pulls individual ratios (`lixoftConnectors::getNCAIndividualRatios()`) and ratio info (`lixoftConnectors::getNCARatios()`).
#'  2. Identifies candidate grouping variables (all columns except id and ratio columns).
#'  3. Reshapes the data into long format (Parameter, Ratio).
#'  4. Summarises across subjects to provide descriptive statistics (N, Min, Q1, Mean, Median, Q3, Max).
#'  5. Returns results either in wide format (one row per Parameter × group) or long format (Statistic/Value pairs).
#'
#' @param projectFile Character string specifying the path to a PKanalix project file (.pkx).
#'   If `NULL` (default), uses the currently loaded project.
#' @param splitCovariates character vector of column names to stratify by (must be in indivratio but not id/ratio columns).
#' @param stratifyColumns Logical; if `TRUE`, creates a stratified table layout with
#'   separate columns for each covariate combination plus an "Overall" column. If `FALSE`
#'   (default), splitCovariates appear as separate columns on the left side of the table.
#'   Only applicable when `splitCovariates` is specified. Only compatible with
#'   `useWideFormat = FALSE`.
#' @param covariateLabels Character vector of custom labels for splitCovariates, must be the
#'   same length as `splitCovariates`. If `NULL` (default), the original covariate names are
#'   used. These labels appear in column headers when `stratifyColumns = TRUE`.
#' @param ratioParams Character vector of ratio parameter names to include. If `NULL`
#'   (default), all available ratio parameters are used.
#' @param metrics Character vector specifying which summary statistics to include in the
#'   output table. If `NULL` (default), all available metrics are included.
#' @param useWideFormat logical; if TRUE (default) results are wide; if FALSE, results are pivoted to long format.
#' @param includeId Logical. If `TRUE` and `useWideFormat = FALSE`, adds individual
#'   subject ratio values below the summary statistics in the "Statistic" column.
#'   Default is `FALSE`. Only works when `useWideFormat = FALSE` and
#'   `stratifyColumns = FALSE`.
#' @param filterCovariates Named list where names are categorical covariate names
#'   and values are vectors of allowed values, e.g. `list(SEX = "Male")`. If `NULL`
#'   (default), no covariate filtering is applied.
#' @param filterIds Character or numeric vector of subject IDs to include. If `NULL`
#'   (default), no ID filtering is applied.
#' @param roundDigits number of digits to round numeric statistics (default = 3).
#' @param roundMethod Character string specifying the rounding method. Options are:
#'   - `"monolixSuiteRounding"` (default): Adaptive rounding that keeps all digits before the
#'     decimal untouched and rounds only the fractional part to the specified number of
#'     significant digits (adds extra digits for values < 1)
#'   - `"significantFigures"`: Standard significant figures rounding - zeroes non-significant
#'     digits before the decimal for large numbers
#'   - `"significantFiguresPreserved"`: Modified significant figures rounding that never zeroes
#'     digits before the decimal - once the integer part already has as many digits as requested,
#'     the value is simply rounded to the nearest whole number
#'   - `"none"`: No rounding performed
#' @param roundByParameter Optional per-parameter rounding overrides on top of the
#'   global `roundDigits`/`roundMethod`. Either a named numeric vector of digits
#'   (`c(AUClast_ratio = 2, Cmax_ratio = 4)`; method falls back to `roundMethod`), or a
#'   data.frame with columns `parameter`, `digits` and/or `method`
#'   (`NA`/missing entries fall back to the global settings). Parameters not
#'   listed keep the global settings. Unknown parameter names are ignored with a
#'   message. Default `NULL` (global rounding everywhere).
#' @param prettyLabels Logical; if `TRUE` (default), converts column names to
#'   human-readable labels.
#' @param customAliases Named vector or 2-column dataframe mapping parameter names
#'   to custom alias labels. Ratios are dimensionless, so no `units` argument is offered.
#' @param mock Logical; if `TRUE`, masks the table for documentation / example
#'   outputs: metric values become placeholders (e.g. `"xx.xx"`), and
#'   `splitCovariates` values become generic labels (`"Category 1"`, ...). The
#'   parameter name and `Statistic`/`Section` labels are kept, except that with
#'   `includeId = TRUE` the individual subject IDs listed under `Statistic`
#'   are relabeled (`"id1"`, `"id2"`, ...), since those are real identifiers.
#'   Default `FALSE`.
#'
#' @return A data.frame with summary statistics for each ratio parameter, optionally stratified.
#'
#' @examples
#' # Overall summary (all subjects, no stratification):
#' tableNCARatios()
#'
#' # Stratified by DOSE, results in wide format:
#' tableNCARatios(splitCovariates = "DOSE", useWideFormat = TRUE)
#'
#' # Stratified by DOSE and VISIT, results in long format:
#' tableNCARatios(splitCovariates = c("DOSE", "VISIT"), useWideFormat = FALSE)
#'
#' \dontrun{
#' # Restrict to specific subjects and covariate values
#' tableNCARatios(filterCovariates = list(SEX = "Male"), filterIds = c(1, 2, 17))
#'
#' # Masked table for documentation / example outputs
#' tableNCARatios(mock = TRUE)
#' }
#'
#' @export
tableNCARatios <- function(projectFile      = NULL,
													 splitCovariates  = NULL,
													 stratifyColumns  = FALSE,
													 covariateLabels  = NULL,
													 ratioParams      = NULL,
													 metrics          = NULL,
													 useWideFormat    = TRUE,
													 includeId        = FALSE,
													 filterCovariates = NULL,
													 filterIds        = NULL,
													 roundDigits      = 3,
													 roundMethod      = c("monolixSuiteRounding", "significantFigures", "significantFiguresPreserved", "none"),
													 roundByParameter = NULL,
													 prettyLabels     = TRUE,
													 customAliases    = NULL,
													 mock             = FALSE) {



	roundMethod <- match.arg(roundMethod)

	# ==========================================================================
	# VALIDATION & SETUP
	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "ncaRatio")

	.validateFilters(filterCovariates, filterIds, resultType = "ncaRatio")

	.validateCovariateSelection(splitCovariates, ratio = TRUE)
	stratifyColumns <- .validateStratificationArgs(splitCovariates, stratifyColumns)
	covariateLabels <- .validateCovariateLabels(splitCovariates, covariateLabels)

	# Normalize custom alias mapping
	aliases_df <- .normalizeAliasMapping(customAliases, "aliases")

	# Validate set parameters
	available_params <- .validateNCAParametersSelection(ratioParams, ratio = TRUE)  # Early validation

	# Normalize + validate per-parameter rounding overrides (keyed by original ratio names)
	round_overrides <- .normalizeRoundOverrides(roundByParameter, roundDigits, roundMethod)
	round_overrides <- .validateRoundOverrides(round_overrides, available_params, context = "tableNCARatios")

	# Validate: stratifyColumns only works with long format
	if (stratifyColumns && useWideFormat) {
		message("stratifyColumns = TRUE requires long format, but useWideFormat = TRUE was set; ignoring stratifyColumns. To stratify, set useWideFormat = FALSE.")
		stratifyColumns <- FALSE
	}
	# Validate combination of includeId and useWideFormat setting
	if (includeId && useWideFormat) {
		warning("includeId = TRUE only works with useWideFormat = FALSE. Setting includeId to FALSE.")
		includeId <- FALSE
	}
	# Validate includeId and stratifyColumns setting
	if (includeId && stratifyColumns) {
		warning("includeId = TRUE is not compatible with stratifyColumns = TRUE. Setting includeId to FALSE.")
		includeId <- FALSE
	}

	ratio_info <- lixoftConnectors::getNCARatios()
	ratio_cols <- names(ratio_info)

	# Determine which ratio columns to pivot
	if (!is.null(ratioParams)) {
		# Pivot only requested parameters (already validated)
		cols_to_pivot <- intersect(ratioParams, ratio_cols)
	} else {
		# Pivot all ratio columns
		cols_to_pivot <- ratio_cols
	}

	# Candidate group-by columns: in dataframe but not id and not ratio(s)
	indivratio      <- lixoftConnectors::getNCAIndividualRatios()

	cov_info        <- lixoftConnectors::getCovariateInformation()
	strat_cov_names <- cov_info$name[cov_info$type %in% c("stratificationcategorical",
																												"additionalstratificationcategorical")]
	if (length(strat_cov_names) > 0) {
		invariance     <- .getOccasionInvariantCovariates(cov_info, covariates = strat_cov_names)
		safe_cov_names <- invariance$invariant
		dropped_covs   <- invariance$varying

		if (length(dropped_covs) > 0) {
			message(sprintf(
				"Skipping occasion-varying stratification covariate(s) for ratio merge: %s (not constant within subject).",
				paste(dropped_covs, collapse = ", ")))
		}

		if (length(safe_cov_names) > 0) {
			# unique() collapses the per-occasion rows (identical for invariant
			# covariates) to one row per id, preventing merge row explosion.
			strat_cov_df <- unique(cov_info$covariate[, c("id", safe_cov_names), drop = FALSE])
			indivratio   <- merge(indivratio, strat_cov_df, by = "id", all.x = TRUE, sort = FALSE)
		}
	}
	# --------------------------------------------------------------------------

	# Apply filters
	indivratio <- .applyFilters(indivratio, filterCovariates, filterIds, id_col = "id")

	groupby_candidates <- setdiff(names(indivratio), c("id", ratio_cols))

	# ==========================================================================
	# DATA PREPARATION
	# Long form over ALL ratio columns
	long_df <- tidyr::pivot_longer(
		indivratio,
		cols      = dplyr::all_of(cols_to_pivot),
		names_to  = "Parameter",
		values_to = "Ratio")

	# ==========================================================================
	# SUMMARIZE & FORMAT

	if (stratifyColumns && !is.null(splitCovariates)) {
		# Default to GeomCV for stratified tables if no metrics specified
		if (is.null(metrics)) {
			metrics <- "GeomCV"
			message("No metric specified for stratified table. Defaulting to 'GeomCV'. To show multiple metrics specify metrics explicitly.")
		}

		# Summarize stratified data
		summary_stats <- .calculateDescriptiveStats(
			data      = long_df,
			value_col = "Ratio",
			id_col    = "id",
			group_by  = c(splitCovariates, "Parameter"))

		# Apply rounding (honoring per-parameter overrides)
		summary_stats <- .applyRounding(
			df        = summary_stats,
			digits    = roundDigits,
			method    = roundMethod,
			exclude   = c("Parameter", splitCovariates),
			overrides = round_overrides,
			paramCol  = "Parameter",
			paramAxis = "row")

		# Add combined metrics
		summary_stats <- .combinedStats(summary_stats)

		# Apply custom aliases (if provided) - no units for ratios (dimensionless)
		if (!is.null(aliases_df)) {
			names(summary_stats)[names(summary_stats) == "Parameter"] <- "parameter"
			summary_stats <- .addUnitsAliases(
				summary_stats,
				add_units   = FALSE,
				add_aliases = TRUE,
				units_df    = NULL,
				aliases_df  = aliases_df)
			names(summary_stats)[names(summary_stats) == "parameter"] <- "Parameter"
		}

		# Validate metrics
		descriptor_cols <- c("Parameter", splitCovariates)  # N/Nobs are optional metrics
		selected_metrics <- .validateAndSelectMetrics(
			available_cols = names(summary_stats),
			user_metrics   = metrics,
			descriptors    = descriptor_cols)

		# Get overall stats
		overall_stats <- .calculateDescriptiveStats(
			data      = long_df,
			value_col = "Ratio",
			id_col    = "id",
			group_by  = "Parameter")

		# Apply rounding to overall (honoring per-parameter overrides)
		overall_stats <- .applyRounding(
			df        = overall_stats,
			digits    = roundDigits,
			method    = roundMethod,
			exclude   = "Parameter",
			overrides = round_overrides,
			paramCol  = "Parameter",
			paramAxis = "row")

		# Add combined metrics to overall
		overall_stats <- .combinedStats(overall_stats)

		# Apply custom aliases to overall (if provided) - no units for ratios (dimensionless)
		if (!is.null(aliases_df)) {
			names(overall_stats)[names(overall_stats) == "Parameter"] <- "parameter"
			overall_stats <- .addUnitsAliases(
				overall_stats,
				add_units   = FALSE,
				add_aliases = TRUE,
				units_df    = NULL,
				aliases_df  = aliases_df)
			names(overall_stats)[names(overall_stats) == "parameter"] <- "Parameter"
		}

		# Build stratified table using helper
		summary_tbl <- .buildStratifiedTable(
			summary_stats    = summary_stats,
			overall_stats    = overall_stats,
			splitCovariates       = splitCovariates,
			selected_metrics = selected_metrics,
			param_col        = "Parameter",
			covariateLabels = covariateLabels)

	} else {
		# -----------------------------------------------------------------------
		# CASE: No stratification
		# -----------------------------------------------------------------------

		# Calculate summary stats
		if (length(splitCovariates) > 0) {
			summary_tbl <- .calculateDescriptiveStats(
				data      = long_df,
				value_col = "Ratio",
				id_col    = "id",
				group_by  = c(splitCovariates, "Parameter"))
		} else {
			summary_tbl <- .calculateDescriptiveStats(
				data      = long_df,
				value_col = "Ratio",
				id_col    = "id",
				group_by  = "Parameter")
		}

		# Apply rounding (honoring per-parameter overrides)
		summary_tbl <- .applyRounding(
			df        = summary_tbl,
			digits    = roundDigits,
			method    = roundMethod,
			exclude   = c("Parameter", splitCovariates),
			overrides = round_overrides,
			paramCol  = "Parameter",
			paramAxis = "row")

		# Add combined metrics
		summary_tbl <- .combinedStats(summary_tbl)

		# Apply custom aliases (if provided) - no units for ratios (dimensionless)
		if (!is.null(aliases_df)) {
			names(summary_tbl)[names(summary_tbl) == "Parameter"] <- "parameter"
			summary_tbl <- .addUnitsAliases(
				summary_tbl,
				add_units   = FALSE,
				add_aliases = TRUE,
				units_df    = NULL,
				aliases_df  = aliases_df)
			names(summary_tbl)[names(summary_tbl) == "parameter"] <- "Parameter"
		}

		# Validate & select metrics
		# Note: .calculateDescriptiveStats() returns both N and Nobs (both are optional metrics)
		descriptor_cols <- c("Parameter", splitCovariates)

		selected_metrics <- .validateAndSelectMetrics(
			available_cols = names(summary_tbl),
			user_metrics   = metrics,
			descriptors    = descriptor_cols)

		summary_tbl <- summary_tbl[, c(descriptor_cols, selected_metrics), drop = FALSE]

		# Pivot to vertical format if requested
		if (useWideFormat == FALSE) {
			# Use helper function for vertical pivot
			param_col   <- intersect(names(summary_tbl), c("parameter", "Parameter"))
			summary_tbl <- .pivotToVertical(summary_tbl, param_col, splitCovariates)
		}
	}

	# ==========================================================================
	# ADD INDIVIDUAL IDs (if requested and useWideFormat = FALSE)

	if (!useWideFormat && includeId && !stratifyColumns) {
		summary_tbl <- .addIndividuals(
			summary_stats   = summary_tbl,
			splitCovariates = splitCovariates,
			resultType      = "ncaRatio",
			params          = ratioParams,
			roundDigits     = roundDigits,
			roundMethod     = roundMethod,
			roundOverrides  = round_overrides,
			units           = FALSE,
			units_df        = NULL,
			aliases         = FALSE,
			aliases_df      = aliases_df,
			indiv_data      = indivratio)
	}

	# ==========================================================================
	# PRETTY LABELS

	if (isTRUE(prettyLabels)) {
		summary_tbl <- .prettyLabels(summary_tbl)

		# Apply pretty labels to Statistic column content if it exists
		if ("Statistic" %in% names(summary_tbl)) {
			# Only apply to metrics (SUMMARY section), not to individual IDs
			if ("Section" %in% names(summary_tbl)) {
				summary_metrics <- unique(summary_tbl$Statistic[summary_tbl$Section == "SUMMARY"])
			} else {
				summary_metrics <- unique(summary_tbl$Statistic)
			}

			# Create mapping for metrics
			dummy_df <- as.data.frame(matrix(NA, nrow = 1, ncol = length(summary_metrics)))
			names(dummy_df) <- summary_metrics
			dummy_df <- .prettyLabels(dummy_df)
			metric_map <- setNames(names(dummy_df), summary_metrics)

			# Apply only to SUMMARY rows (not individual IDs)
			if ("Section" %in% names(summary_tbl)) {
				summary_tbl$Statistic[summary_tbl$Section == "SUMMARY"] <-
					metric_map[summary_tbl$Statistic[summary_tbl$Section == "SUMMARY"]]
			} else {
				summary_tbl$Statistic <- metric_map[summary_tbl$Statistic]
			}
		}
	}

	# ==========================================================================
	# SORT BY COVARIATES (for vertical tables with numeric-valued categorical splitCovariates)

	if (!useWideFormat && !stratifyColumns && !is.null(splitCovariates) && length(splitCovariates) > 0) {
		# Only sort if Section doesn't exist
		# (if Section exists, .addIndividuals() already sorted correctly)
		if (!"Section" %in% names(summary_tbl)) {
			summary_tbl <- summary_tbl %>%
				dplyr::arrange(dplyr::across(
					dplyr::all_of(splitCovariates),
					~{
						num <- suppressWarnings(as.numeric(as.character(.x)))
						if(all(is.na(num))) .x else num
					}
				))
		}
	}

	if (isTRUE(mock)) {
		param_col_final <- intersect(names(summary_tbl), c("Parameter", "parameter"))[1]

		modality_cols <- list()
		if (!stratifyColumns && !is.null(splitCovariates)) {
			for (sc in splitCovariates) modality_cols[[sc]] <- NA_character_
		}

		summary_tbl <- .mockOutputData(
			df           = summary_tbl,
			digits       = roundDigits,
			excludeCols  = c(param_col_final, "Statistic", "Section"),
			modalityCols = if (length(modality_cols)) modality_cols else NULL,
			naFillAxis   = if (stratifyColumns || !useWideFormat) "row" else "column")

		summary_tbl <- .mockIndividualSection(summary_tbl)
	}

	# ==========================================================================
	# SET ATTRIBUTES for style_tfl
	# To recognize this as NCA ratio table with sections
	if (includeId && "Section" %in% names(summary_tbl)) {
		attr(summary_tbl, "table_type") <- "nca_ratio"
		attr(summary_tbl, "format")     <- "long"  # Always long when includeId=TRUE
	}
	attr(summary_tbl, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(summary_tbl, "r_source")       <- .getRScriptPath()
	return(as.data.frame(summary_tbl))

}

################################################################################
#' tableBEResults
#'
#' Generates a summary table of bioequivalence (BE) analysis results, including
#' geometric mean ratios (GMR), confidence intervals, and BE conclusions for all
#' test vs. reference comparisons.
#'
#' @param projectFile Character string specifying the path to a PKanalix project file (.pkx).
#'   If `NULL` (default), uses the currently loaded project.
#'
#' @param BEParams Character vector of parameter names to include in the output.
#'   If `NULL` (default), all BE parameters are included.
#'
#' @param roundDigits Integer specifying the number of decimal places for rounding
#'   numeric values. Default is `3`.
#'
#' @param roundMethod Character string specifying the rounding method. Options are:
#'   - `"monolixSuiteRounding"` (default): Adaptive rounding that keeps all digits before the
#'     decimal untouched and rounds only the fractional part to the specified number of
#'     significant digits (adds extra digits for values < 1)
#'   - `"significantFigures"`: Standard significant figures rounding - zeroes non-significant
#'     digits before the decimal for large numbers
#'   - `"significantFiguresPreserved"`: Modified significant figures rounding that never zeroes
#'     digits before the decimal - once the integer part already has as many digits as requested,
#'     the value is simply rounded to the nearest whole number
#'   - `"none"`: No rounding performed
#'
#' @param units Logical; if `TRUE` (default), appends units to parameter names
#'   (e.g., "ln(AUClast) (h*ng/mL)"). Units are retrieved from project settings.
#'
#' @param aliases Logical; if `TRUE` (default), uses parameter aliases instead of
#'   raw parameter names (e.g., "AUC[[sub:0-tlast]]" instead of "AUClast").
#'
#' @param customUnits Named vector or 2-column dataframe mapping parameter names
#'   to custom unit labels. Example: `c(Cmax = "ng/mL", AUClast = "h*ng/mL")`.
#'   If provided, these override units from the PKanalix project.
#'
#' @param customAliases Named vector or 2-column dataframe mapping parameter names
#'   to custom alias labels. Example: `c(Cmax = "C[[sub:max]]", AUClast = "AUC[[sub:0-tlast]]")`.
#'   If provided, these override aliases from the PKanalix project.
#'   Supports special formatting syntax: `[[sub:text]]` for subscript, `[[sup:text]]` for
#'   superscript, and `~text~` for italic.
#'
#' @param BELimits Logical; if `TRUE`, includes a column showing the BE acceptance
#'   criteria interval (e.g., "(80;125)"). Default is `FALSE`. Limits are retrieved
#'   from the project's BE settings.
#'
#' @param BEConclusion Logical; if `TRUE` (default), includes a conclusion column
#'   indicating "Bioequivalent" or "Not bioequivalent" based on the analysis results.
#'
#' @param prettyLabels Logical; if `TRUE` (default), converts column names to
#'   human-readable labels (e.g., "Ratio (%)", "Confidence Interval").
#'
#' @param mock Logical; if `TRUE`, masks the table for documentation / example
#'   outputs: the ratio, confidence interval and BE-limits columns become
#'   placeholders (e.g. `"xx.xx"`, `"(xx.xx;xx.xx)"`); `Comparison`, the
#'   parameter name, and `Conclusion` (the BE outcome, which would otherwise
#'   reveal a real result derived from the masked numbers) are relabeled
#'   generically per unique value (`"Comparison 1"`, `"Parameter 1"`,
#'   `"Conclusion 1"`, ...). The same parameter keeps the same label across
#'   all its comparison rows. Default `FALSE`.
#'
#' @return A data.frame containing BE results with columns for comparison, parameter,
#'   GMR (ratio), confidence interval, and optionally BE limits and conclusion.
#'   Parameters analyzed on log scale are indicated with "ln()" prefix.
#'
#' @details
#' The function retrieves BE results from `lixoftConnectors::getBioequivalenceResults()`
#' and combines all test vs. reference comparisons into a single table. Log-transformed
#' parameters are automatically identified and labeled with "ln()" prefix.
#'
#' @export
tableBEResults <- function(projectFile    = NULL,
													 BEParams       = NULL,
													 roundDigits    = 3,
													 roundMethod    = c("monolixSuiteRounding", "significantFigures", "significantFiguresPreserved", "none"),
													 units          = TRUE,
													 aliases        = TRUE,
													 customUnits   = NULL,
													 customAliases = NULL,
													 BELimits       = FALSE,
													 BEConclusion   = TRUE,
													 prettyLabels   = TRUE,
													 mock           = FALSE){

	roundMethod <- match.arg(roundMethod)

	# Ensure the correct project is loaded
	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "be")

	# Validate set parameters
	.validateNCAParametersSelection(BEParams, ratio = FALSE, be = TRUE)

	# Normalize custom alias/unit mappings
	aliases_df <- .normalizeAliasMapping(customAliases, "aliases")
	units_df   <- .normalizeAliasMapping(customUnits, "units")
	#-----------------------------------------------------------------------------
	# GET BE RESULTS AND SETTINGS
	be_results  <- lixoftConnectors::getBioequivalenceResults()
	be_settings <- lixoftConnectors::getBioequivalenceSettings()

	# Get reference treatment
	reference <- be_settings$linearmodelfactors$reference

	# Get BE limits from settings
	be_lower <- be_settings$bioequivalencelimits[1]
	be_upper <- be_settings$bioequivalencelimits[2]

	# Get log transformation info
	param_info        <- be_settings$computedbioequivalenceparameters
	log_transform_map <- setNames(param_info$logTransform, param_info$parameters)

	#-----------------------------------------------------------------------------
	# COMBINE ALL COMPARISONS INTO ONE TABLE
	ci_results <- be_results$confidenceIntervals

	# Modular: iterate over all test treatments
	result <- purrr::map_dfr(names(ci_results), function(test_treatment) {
		df   <- ci_results[[test_treatment]]

		df %>%
			dplyr::transmute(
				Original_Parameter= Parameter,
				parameter         = Parameter,
				Comparison        = paste0(test_treatment, " vs.",reference),
				Ratio             = Ratio,
				CILower           = CILower,
				CIUpper           = CIUpper,
				BE                = BE)
	})

	# Filter to selected BE parameters (if specified)
	if (!is.null(BEParams)) {
		result <- result %>% dplyr::filter(Original_Parameter %in% BEParams)
	}

	# Apply aliases only (no units)
	result <- .addUnitsAliases(
		result,
		add_units   = FALSE,
		add_aliases = aliases,
		units_df    = NULL,
		aliases_df  = aliases_df)

	# Apply ln() wrapper
	result <- .applyLogTransformLabel(result, log_transform_map)

	# Add units and remove Original_Parameter
	if (units) {
		result <- .addUnitsOnly(result, units_df = units_df)
	} else {
		result$Original_Parameter <- NULL
	}

	# style_tfl(result)
	result <- .round_num(
		to_round        = result,
		digits          = roundDigits,
		rounding_method = roundMethod,
		exclude         = c("parameter", "Comparison", "BE"))

	result <- result %>% dplyr::mutate(CI = paste0("(",CILower,";",CIUpper,")")) %>%
		dplyr::select(Comparison, parameter, Ratio, CI, BE) %>%
		dplyr::arrange(Comparison)

	if(isTRUE(BELimits)){
		result <- result %>%
			dplyr::mutate(Criteria.interval = paste0("(",be_lower,";",be_upper,")")) %>%
			dplyr::relocate(Criteria.interval, .before = BE)
	}
	if(isTRUE(BEConclusion)){
		result <- result %>% dplyr::mutate(Conclusion = dplyr::case_when(BE == 0 ~ "Not bioequivalent",
																																		 BE == 1 ~ "Bioequivalent"))
	}
	result <- result %>% dplyr::select(-BE)

	if (isTRUE(prettyLabels)) {
		result <- .prettyLabels(result)
	}

	if (isTRUE(mock)) {
		param_col     <- if (isTRUE(prettyLabels)) "Parameter" else "parameter"
		modality_cols <- stats::setNames(list(list(prefix = "Comparison "),
																					 list(prefix = "Parameter ")),
																			c("Comparison", param_col))
		if (isTRUE(BEConclusion)) {
			modality_cols[["Conclusion"]] <- list(prefix = "Conclusion ")
		}

		result <- .mockOutputData(result, digits = roundDigits,
															modalityCols = modality_cols,
															naFillAxis   = "column")
	}

	attr(result, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(result, "r_source")       <- .getRScriptPath()
	return(result)
}



################################################################################
# Helpers for Listings

#' listingAbbreviation
#'
#' Builds an abbreviation listing for the computed NCA parameters of a PKanalix
#' project. The full list of computed parameters is always kept (it is never
#' reduced to those that happen to have an alias). When \code{aliases = TRUE}
#' the \code{Abbreviation} column shows each parameter's alias where one exists
#' and the original parameter name where it does not, so a mix of aliases and
#' raw names is expected when aliases are only defined for some parameters. The
#' listing also carries an (initially empty) \code{Full term} column that can be
#' pre-populated from a user-supplied mapping. Rows are de-duplicated and sorted.
#'
#' @param projectFile Character path to the PKanalix project (\code{.pkx}). If
#'   \code{NULL}, the currently loaded project is used.
#' @param aliases Logical; if \code{TRUE} (default) parameter names in the
#'   \code{Abbreviation} column are replaced by their alias where available,
#'   falling back to the original name otherwise. Aliases come from
#'   \code{customAliases} when supplied, otherwise they are fetched from the
#'   project. HTML \code{<sub>text</sub>} markup in aliases is converted for
#'   flextable rendering. If \code{FALSE}, the original parameter names are used.
#' @param customAliases Optional custom alias mapping used instead of the
#'   project aliases. Either a named vector (e.g.
#'   \code{c(Cmax = "C<sub>max</sub>")}) or a 2-column data frame whose first
#'   column holds parameter names and second column the aliases.
#' @param fullTerms Optional mapping used to fill the \code{Full term} column.
#'   Keyed on the original parameter names. Same accepted forms as
#'   \code{customAliases}: a named vector (e.g.
#'   \code{c(Cmax = "Maximum concentration")}) or a 2-column data frame
#'   (parameter, full term). Parameters without an entry stay empty. If
#'   \code{NULL} (default), the \code{Full term} column is left empty.
#'
#' @return A data frame with columns \code{Abbreviation} (alias where available,
#'   original parameter name otherwise) and \code{Full term}, de-duplicated and
#'   sorted. Rename the returned columns afterwards if different labels are
#'   desired.
#'
#' @examples
#' # listingAbbreviation()
#' # listingAbbreviation(aliases = FALSE)
#' # listingAbbreviation(fullTerms = c(Cmax = "Maximum concentration",
#' #                                   AUClast = "Area under the curve to last obs"))
#' @export
listingAbbreviation <- function(projectFile   = NULL,
																aliases       = TRUE,
																customAliases = NULL,
																fullTerms     = NULL) {
	# Ensure the correct project is loaded
	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	uparams <- unique(lixoftConnectors::getNCASettings()$computedncaparameters)
	abbrev_listing <- data.frame(parameter = uparams, stringsAsFactors = FALSE)
	abbrev_listing$abbrev <- abbrev_listing$parameter

	if (isTRUE(aliases)) {
		aliases_df <- .normalizeAliasMapping(customAliases, "aliases")

		if (is.null(aliases_df)) {
			# Fetch from PKanalix project (existing behavior)
			NCAParamSummary_info <- lixoftConnectors::getNCAParameterStatistics(parameters = uparams)$information %>%
				tibble::rownames_to_column(var = "parameter")

			if ("aliases" %in% names(NCAParamSummary_info)) {
				aliases_df <- NCAParamSummary_info %>%
					dplyr::select(parameter, aliases)
			} else {
				warning("Aliases were requested (aliases=TRUE) but are not available in the current project. Keeping original parameter names.")
				aliases_df <- NULL
			}
		}

		if (!is.null(aliases_df)) {
			# Apply HTML conversion so users can use <sub>text</sub> syntax
			aliases_df <- .convertHtmlAliasesForFlextable(aliases_df)
			abbrev_listing <- abbrev_listing %>%
				dplyr::left_join(aliases_df, by = "parameter") %>%
				dplyr::mutate(abbrev = dplyr::coalesce(
					dplyr::na_if(as.character(aliases), ""), parameter)) %>%
				dplyr::select(-aliases)
		}
	}

	# ---------------------------------------------------------------------------
	# Full term column always present: empty unless a mapping is supplied.
	# The mapping is keyed on the original parameter names.
	fullterm_df <- .normalizeAliasMapping(fullTerms, "fullterm")
	if (!is.null(fullterm_df)) {
		abbrev_listing <- abbrev_listing %>%
			dplyr::left_join(fullterm_df, by = "parameter")
	} else {
		abbrev_listing$fullterm <- NA_character_
	}

	abbrev_listing <- abbrev_listing %>%
		dplyr::arrange(parameter) %>%
		dplyr::transmute(Abbreviation = abbrev,
										 `Full term`  = fullterm) %>%
		dplyr::distinct()

	return(abbrev_listing)
}

#-------------------------------------------------------------------------------
#' listingNCAConcentration
#'
#' Creates a long-format listing of individual concentrations by time, with
#' optional stratification columns (\code{splitCovariates}). Summary metric rows are
#' removed, and the listing is left-joined with used concentration data
#' (including censoring flags). Occasion-level identifiers are concatenated to
#' form unique subject IDs when applicable.
#'
#' @param projectFile Character string. Path to the PKanalix project file (.pkx).
#'   If NULL, uses the currently loaded project.
#'
#' @param splitCovariates Character vector of covariate names to include as
#'   stratification columns (e.g., \code{c("TREAT","FOOD")}); \code{NULL} for no
#'   stratification. Occasion columns present in the project are appended
#'   automatically (see Details).
#'
#' @param useNominalTime Logical; if \code{TRUE}, the function attempts to use
#'   nominal time for the concentration listing. If nominal time is not available
#'   in the project (checked via \code{lixoftConnectors::getData()} and \code{headerTypes ==
#'   "nominaltime"}), the function will fall back to actual time and issue a
#'   warning. Default is \code{FALSE}.
#'
#' @param filterCovariates Named list where names are categorical covariate names
#'   and values are vectors of allowed values, e.g.
#'   `list(SEX = "Male", DOSE = c("100", "200"))`. Rows not matching are dropped.
#'   If `NULL` (default), no covariate filtering is applied.
#'
#' @param filterIds Character or numeric vector of subject IDs to include. If
#'   `NULL` (default), no ID filtering is applied.
#'
#' @param roundDigits Integer specifying the number of decimal places for rounding
#'   numeric values. Default is `3`.
#'
#' @param roundMethod Character string specifying the rounding method. Options are:
#'   - `"none"` (default): No rounding performed
#'   - `"monolixSuiteRounding"`: Adaptive rounding that keeps all digits before the
#'     decimal untouched and rounds only the fractional part to the specified number of
#'     significant digits (adds extra digits for values < 1)
#'   - `"significantFigures"`: Standard significant figures rounding - zeroes non-significant
#'     digits before the decimal for large numbers
#'   - `"significantFiguresPreserved"`: Modified significant figures rounding that never zeroes
#'     digits before the decimal - once the integer part already has as many digits as requested,
#'     the value is simply rounded to the nearest whole number
#'
#' @param mock Logical; if `TRUE`, masks the listing for documentation / example
#'   outputs: id -> `"id1"`, categorical covariates / occasions -> `"Category 1"`
#'   / `"occ1"`, and all numeric columns (time, Concentration, UsedConc,
#'   Censoring) -> `"xx.xx"`. Default `FALSE`.
#'
#' @details
#'   The function first resets stratification via
#'   \code{lixoftConnectors::setNCAResultsStratification()} and optionally splits results by
#'   \code{splitCovariates}.
#'
#'   - **Actual time:** Always extracted via
#'   \code{lixoftConnectors::getNCAConcentrations(displayIndividualProfiles = TRUE,
#'   useNominalTime = FALSE)}. This guarantees we can join later with
#'   \code{lixoftConnectors::getPointsIncludedForLambdaZ()}, which only provides actual times.
#'
#'   - **Nominal time:** If \code{useNominalTime = TRUE} and nominal time is
#'   available, the function also extracts nominal profiles. Since actual and
#'   nominal use the same wide column grid, both tables are pivoted to long and
#'   aligned per-subject using an artificial row index (\code{row_id}) that
#'   preserves column order. This ensures each actual concentration is paired
#'   with exactly one nominal time point.
#'
#'   - **Used concentrations:** After long-formatting, the table is joined with
#'   \code{lixoftConnectors::getPointsIncludedForLambdaZ()}, which indicates which concentration–
#'   time points were used in the lambda-z calculation and provides censoring
#'   flags.
#'
#'   - If occasion-level splitCovariates exist, they are concatenated to the subject
#'   ID using \code{sep = "#"} to ensure unique identifiers.
#'
#' @return A data frame in long format containing (if present) \code{splitCovariates},
#'   \code{id}, \code{ActualTime}, \code{NominalTime} (if available),
#'   \code{Concentration}, \code{UsedConc}, and \code{Censoring}.
#'
#' @examples
#' # Default: actual time only
#' listingNCAConcentration()
#'
#' # With splitCovariates
#' listingNCAConcentration(splitCovariates = c("TREAT", "FOOD"))
#'
#' # Include nominal time if available
#' listingNCAConcentration(splitCovariates = c("TREAT", "FOOD"), useNominalTime = TRUE)
#'
#' \dontrun{
#' # Restrict to specific subjects and covariate values
#' listingNCAConcentration(
#'   filterCovariates = list(TREAT = "A"),
#'   filterIds        = c(1, 2, 17)
#' )
#'
#' # Masked listing for documentation / example outputs
#' listingNCAConcentration(mock = TRUE)
#' }
#'
#' @export
listingNCAConcentration <- function(projectFile      = NULL,
																		splitCovariates  = NULL,
																		useNominalTime   = FALSE,
																		filterCovariates = NULL,
																		filterIds        = NULL,
																		roundDigits      = 3,
																		roundMethod      = c("none", "monolixSuiteRounding", "significantFigures", "significantFiguresPreserved"),
																		mock             = FALSE) {

	roundMethod <- match.arg(roundMethod)

	# Ensure the correct project is loaded
	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "nca")

	# Validate filter arguments
	.validateFilters(filterCovariates, filterIds, resultType = "nca")

	# Validate set splitCovariates
	.validateCovariateSelection(splitCovariates, ratio = FALSE)

	# Get occasion variables from data and add to stratification
	project_data <- lixoftConnectors::getData()
	occ_vars     <- project_data$header[project_data$headerTypes == "occ"]

	# Add occasion variables to splitCovariates
	if (length(occ_vars) > 0) {
		splitCovariates <- unique(c(splitCovariates, occ_vars))
	}

	has_time    <- any(project_data$headerTypes == "time")
	has_nomtime <- any(project_data$headerTypes == "nominaltime")

	# Determine which time to use for extraction
	if (!has_time && has_nomtime) {
		# Only nominal time available - must use it
		use_nominal_for_actual <- TRUE
		if (!useNominalTime) {
			message("Note: No actual time available in project. Nominal time will be used.")
		}
	} else {
		# Has actual time (standard case)
		use_nominal_for_actual <- FALSE
	}

	# reinitialize stratification connector
	lixoftConnectors::setNCAResultsStratification()

	# Set stratification
	if (!is.null(splitCovariates)) {
		lixoftConnectors::setNCAResultsStratification(split = splitCovariates)
	}

	# Extract time data
	if (use_nominal_for_actual) {
		# Only nominal time exists - use it as "actual time"
		df_actual <- lixoftConnectors::getNCAConcentrations(displayIndividualProfiles = TRUE, useNominalTime = TRUE) # Must use TRUE when only nominal exists
	} else {
		# Has actual time - extract normally
		df_actual <- lixoftConnectors::getNCAConcentrations(displayIndividualProfiles = TRUE, useNominalTime = FALSE)
	}
	# Always extract ACTUAL time version (baseline)
	df_actual     <- .fixNCAConcentrationsColumnSwap(df_actual, splitCovariates)
	metrics       <- lixoftConnectors::getNCAConcentrations()$metric
	metric_labels <- as.character(metrics[!is.na(metrics)])

	# Process actual time data
	indiv_actual <- df_actual %>%
		dplyr::filter(!(subject %in% metric_labels)) %>% # drop summary rows
		dplyr::mutate(dplyr::across(setdiff(names(.), c("subject", splitCovariates)), as.character)) %>%
		tidyr::pivot_longer(
			cols = setdiff(names(.), c("subject", splitCovariates)), # pivot from wide format to long
			names_to = "ActualTimeLabel",
			values_to = "Concentration") %>%
		dplyr::group_by(subject, dplyr::across(dplyr::all_of(splitCovariates))) %>%
		dplyr::filter(Concentration != "") %>% # drop empty cells
		dplyr::mutate(row_id = dplyr::row_number()) %>% # assign row index (per subject)
		dplyr::ungroup() %>%
		dplyr::mutate(
			Time = as.numeric(ActualTimeLabel), # numeric actual time
			id   = as.character(subject)) %>%
		dplyr::relocate(dplyr::any_of(splitCovariates), id, Time, Concentration, row_id) %>%
		dplyr::select(-subject)


	# Handle nominal time extraction
	if (useNominalTime) {
		if (use_nominal_for_actual) {
			# Only nominal time exists - already used as actual time
			indiv <- indiv_actual %>% dplyr::select(-row_id, -ActualTimeLabel)

		} else if (has_nomtime) {
			# Has both actual and nominal time - extract nominal separately
			df_nom <- lixoftConnectors::getNCAConcentrations(
				displayIndividualProfiles = TRUE,
				useNominalTime = TRUE)
			df_nom <- .fixNCAConcentrationsColumnSwap(df_nom, splitCovariates)

			indiv_nom <- df_nom %>%
				dplyr::filter(!(subject %in% metric_labels)) %>%
				dplyr::mutate(dplyr::across(setdiff(names(.), c("subject", splitCovariates)), as.character)) %>%
				tidyr::pivot_longer(
					cols = setdiff(names(.), c("subject", splitCovariates)),
					names_to = "NominalTimeLabel",
					values_to = "dummyConc") %>%
				dplyr::group_by(subject, dplyr::across(dplyr::all_of(splitCovariates))) %>%
				dplyr::filter(dummyConc != "") %>%
				dplyr::mutate(row_id = dplyr::row_number()) %>% # align by row_id (same column order)
				dplyr::ungroup() %>%
				dplyr::mutate(
					NominalTime = as.numeric(NominalTimeLabel),
					id = as.character(subject)) %>%
				dplyr::relocate(dplyr::any_of(splitCovariates), id, NominalTime, row_id) %>%
				dplyr::select(-subject)

			# Join actual + nominal
			indiv <- indiv_actual %>%
				dplyr::left_join(indiv_nom, by = c("id", splitCovariates, "row_id")) %>%
				dplyr::select(-row_id, -ActualTimeLabel, -NominalTimeLabel, -dummyConc)
		} else {
			# User requested nominal time but it's not available ← ADD THIS CASE
			warning("No nominal time available in project; only actual time will be included.")
			indiv <- indiv_actual %>% dplyr::select(-row_id, -ActualTimeLabel)
		}

	} else {
		# No nominal time requested
		indiv <- indiv_actual %>% dplyr::select(-row_id, -ActualTimeLabel)
	}

	# Get used concentrations & censoring flags (can't use getNCAData bc of bug)
	UsedConc <- lixoftConnectors::getPointsIncludedForLambdaZ() %>%
		dplyr::rename(Time = time, UsedConc = concentration) %>%
		dplyr::mutate(id = as.character(id))

	# Handle occasion vars (if present: concat them into id)
	id_cols <- c("id", "Time", "UsedConc", "BLQ", "includedForLambdaZ")
	occasion_vars <- setdiff(names(UsedConc), id_cols)
	if (length(occasion_vars) > 0) {
		UsedConc <- UsedConc %>%
			dplyr::mutate(dplyr::across(dplyr::all_of(occasion_vars), as.character)) %>%
			dplyr::mutate(id = do.call(paste, c(dplyr::across(c("id", dplyr::all_of(occasion_vars))), sep = "#"))) %>%
			dplyr::select(-dplyr::all_of(occasion_vars))
	}

	# Final join: (indiv conc & used conc) + cleanup
	indiv <- indiv %>%
		dplyr::left_join(UsedConc, by = c("id", "Time")) %>%
		dplyr::mutate(
			id            = sub("#.*", "", id), # Remove # and everything after it
			Concentration = as.character(Concentration),
			Censoring     = dplyr::case_when(is.na(BLQ) & Concentration != "" ~ 1, TRUE ~ BLQ)) %>%
		dplyr::mutate(dplyr::across(dplyr::all_of(splitCovariates), as.character)) %>%
		dplyr::select(-BLQ, -includedForLambdaZ)

	# Rename Time column. Depends on ifactual time is available in dataset
	if (use_nominal_for_actual) {
		indiv <- indiv %>% dplyr::rename(NominalTime = Time)
	} else {
		indiv <- indiv %>% dplyr::rename(ActualTime = Time)
	}

	# Continue cleanup
	# Primary sort by user-specified split covariates (so modalities are grouped),
	# then the original id / occ / time ordering. occ_vars are excluded here because
	# they were appended to splitCovariates above and are already sorted below.
	split_sort <- intersect(setdiff(splitCovariates, occ_vars), names(indiv))
	indiv <- indiv %>%
		dplyr::relocate(dplyr::any_of(c("ActualTime", "NominalTime")), .before = Concentration) %>%
		dplyr::relocate(dplyr::any_of(occ_vars), .after = id) %>%
		dplyr::mutate(.id_numeric = suppressWarnings(as.numeric(id))) %>% # Try converting to numeric
		dplyr::arrange(
			dplyr::across(dplyr::all_of(split_sort)), # split covariates first -> group modalities
			# Sort by numeric if possible, otherwise by character
			dplyr::coalesce(.id_numeric, Inf), # Numeric IDs first, non-numeric at end
			id,                                # Then by character (for non-numeric IDs)
			!!!rlang::syms(occ_vars),
			dplyr::across(dplyr::any_of(c("ActualTime", "NominalTime")))) %>%
		dplyr::select(-.id_numeric)  # Remove helper column

	# Apply filters
	indiv <- .applyFilters(indiv, filterCovariates, filterIds, id_col = "id")

	# Get IDs from NCA data
	nca_ids <- unique(lixoftConnectors::getNCAIndividualParameters()$parameters$id)

	# Check if extracted IDs match
	extracted_ids <- unique(as.numeric(indiv$id))
	all(extracted_ids %in% nca_ids)  # Should return TRUE

	# Apply rounding only to Concentration and UsedConc
	indiv <- indiv %>%
		dplyr::mutate(
			.conc_numeric = suppressWarnings(as.numeric(Concentration)), # Pre-compute numeric conversion (with warning suppression)
			Concentration = dplyr::case_when(
				is.na(.conc_numeric) ~ Concentration,
				TRUE ~ as.character(.round_num(.conc_numeric,
																			 digits = roundDigits,
																			 rounding_method = roundMethod))),
			UsedConc = .round_num(UsedConc,
														digits = roundDigits,
														rounding_method = roundMethod)) %>%
		dplyr::select(-.conc_numeric)

	if (isTRUE(mock)) {
		col_types <- stats::setNames(project_data$headerTypes, project_data$header)
		col_types["id"] <- "id"

		indiv <- .mockDataListing(indiv, colTypes = col_types, digits = roundDigits)
	}

	attr(indiv, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(indiv, "r_source")       <- .getRScriptPath()
	return(as.data.frame(indiv))
}

#-------------------------------------------------------------------------------
#' NCA Individual Parameters Listing
#'
#' Creates a listing table of individual NCA parameter values from a PKanalix project.
#'
#' @param projectFile Character string. Path to the PKanalix project file (.pkx).
#'   If NULL, uses the currently loaded project.
#' @param splitCovariates Character vector. Categorical covariates (type "catcov")
#'   to include and sort by. If specified, the table is sorted by these
#'   splitCovariates in the order provided.
#' @param ncaParams Character vector. NCA parameters to include. If NULL (default),
#'   all computed NCA parameters are included.
#' @param filterCovariates Named list where names are categorical covariate names
#'   and values are vectors of allowed values, e.g.
#'   `list(SEX = "Male", DOSE = c("100", "200"))`. Rows not matching are dropped.
#'   If `NULL` (default), no covariate filtering is applied.
#' @param filterIds Character or numeric vector of subject IDs to include. If
#'   `NULL` (default), no ID filtering is applied.
#' @param roundDigits Numeric. Number of digits for rounding (default: 3).
#' @param roundMethod Character. Rounding method: "monolixSuiteRounding" (default),
#'   "significantFigures", "significantFiguresPreserved", or "none".
#' @param units Logical. If TRUE (default), parameter units are added to column names.
#' @param aliases Logical. If TRUE (default), parameter names are replaced with their aliases.
#' @param customUnits Named vector or 2-column dataframe mapping parameter names
#'   to custom unit labels. Example: `c(Cmax = "ng/mL", AUClast = "h*ng/mL")`.
#'   If provided, these override units from the PKanalix project.
#' @param customAliases Named vector or 2-column dataframe mapping parameter names
#'   to custom alias labels. Example: `c(Cmax = "C[[sub:max]]", AUClast = "AUC[[sub:0-tlast]]")`.
#'   If provided, these override aliases from the PKanalix project. Supports special
#'   formatting syntax: `[[sub:text]]` for subscript, `[[sup:text]]` for superscript,
#'   and `~text~` for italic.
#' @param mock Logical; if `TRUE`, masks the listing for documentation / example
#'   outputs: id -> `"id1"`, occasions -> `"occ1"`, categorical / stratification
#'   covariates -> `"Category 1"`, and all numeric columns (continuous covariates,
#'   NCA parameter values) -> `"xx.xx"`. Default `FALSE`.
#'
#' @return A data frame with one row per subject, containing the `id` column,
#'   `splitCovariates` (if specified, placed to the left of `id`), any
#'   stratification-only / additional-stratification categorical covariates
#'   (merged in automatically when defined in the project), and the selected
#'   (or all) NCA parameter columns, with names formatted per `units`/`aliases`.
#'
#' @details
#' The function validates that NCA analysis has been run and that specified splitCovariates
#' and parameters exist in the project. Parameter names can be formatted with units
#' and/or aliases for improved readability. Stratification-only and
#' additional-stratification categorical covariates (PKanalix) are merged in
#' automatically when available, independent of `splitCovariates`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic listing with all parameters
#' params <- listingNCAParameters(projectFile = "path/to/project.pkx")
#'
#' # With covariate stratification and parameter filtering
#' params <- listingNCAParameters(
#'   projectFile = "path/to/project.pkx",
#'   splitCovariates = c("SEX", "DOSE"),
#'   ncaParams = c("AUC", "Cmax", "Tmax"),
#'   roundDigits = 2,
#'   units = TRUE,
#'   aliases = TRUE
#' )
#'
#' # Restrict to specific subjects and covariate values
#' params <- listingNCAParameters(
#'   filterCovariates = list(SEX = "Male"),
#'   filterIds        = c(1, 2, 17)
#' )
#'
#' # Masked listing for documentation / example outputs
#' params <- listingNCAParameters(mock = TRUE)
#' }
listingNCAParameters <- function(projectFile      = NULL,
																 splitCovariates  = NULL,
																 ncaParams        = NULL,
																 filterCovariates = NULL,
																 filterIds        = NULL,
																 roundDigits      = 3,
																 roundMethod      = c("monolixSuiteRounding", "significantFigures", "significantFiguresPreserved", "none"),
																 units            = TRUE,
																 aliases          = TRUE,
																 customUnits      = NULL,
																 customAliases    = NULL,
																 mock             = FALSE){


	roundMethod <- match.arg(roundMethod)
	# Ensure the correct project is loaded
	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "nca")

	# Validate filter arguments
	.validateFilters(filterCovariates, filterIds, resultType = "nca")

	# Validate set splitCovariates
	.validateCovariateSelection(splitCovariates, ratio = FALSE)
	# Validate set parameters
	.validateNCAParametersSelection(ncaParams, ratio = FALSE)

	# Normalize custom alias/unit mappings
	aliases_df <- .normalizeAliasMapping(customAliases, "aliases")
	units_df   <- .normalizeAliasMapping(customUnits, "units")
	#-----------------------------------------------------------------------------

	indivNCA <- lixoftConnectors::getNCAIndividualParameters()$parameter

	project_data        <- lixoftConnectors::getData()
	all_splitCovariates <- project_data$header[project_data$headerTypes %in% c("catcov", "contcov", "occ")]
	occ_vars            <- project_data$header[project_data$headerTypes == "occ"]
	cov_info            <- lixoftConnectors::getCovariateInformation()
	strat_cov_names     <- cov_info$name[cov_info$type %in% c("stratificationcategorical",
																														"additionalstratificationcategorical")]
	if (length(strat_cov_names) > 0) {
		join_keys    <- c("id", intersect(occ_vars,
																			intersect(names(indivNCA), names(cov_info$covariate))))
		strat_cov_df <- unique(cov_info$covariate[, c(join_keys, strat_cov_names), drop = FALSE])
		indivNCA     <- merge(indivNCA, strat_cov_df, by = join_keys, all.x = TRUE, sort = FALSE)
	}
	# ---------------------------------------------------------------------------

	# Apply filters
	indivNCA <- .applyFilters(indivNCA, filterCovariates, filterIds, id_col = "id")

	# Filter to selected parameters if ncaParams is specified
	if (!is.null(ncaParams)) {
		# Keep id, all splitCovariates, merged strat covariates, and selected parameters only
		cols_to_keep <- c("id", all_splitCovariates, strat_cov_names, ncaParams)
		# Only keep columns that exist in the data
		cols_to_keep <- intersect(cols_to_keep, names(indivNCA))
		indivNCA <- indivNCA %>% dplyr::select(dplyr::all_of(cols_to_keep))
	}

	# Apply rounding to parameter columns (BEFORE renaming)
	indivNCA <- .round_num(
		to_round        = indivNCA,
		digits          = roundDigits,
		rounding_method = roundMethod,
		exclude         = c("id", all_splitCovariates, strat_cov_names))

	# Identify parameter columns (exclude id, all splitCovariates and merged strat covariates)
	param_cols <- setdiff(names(indivNCA), c("id", all_splitCovariates, strat_cov_names))

	# Create temporary data frame with parameter names
	temp_df <- data.frame(parameter = param_cols, stringsAsFactors = FALSE)

	# Apply units/aliases formatting
	if (isTRUE(units) || isTRUE(aliases)) {
		temp_df_formatted <- .addUnitsAliases(
			temp_df,
			add_units   = units,
			add_aliases = aliases,
			units_df    = units_df,
			aliases_df  = aliases_df)
	}

	# Create mapping: original name -> formatted name
	name_mapping <- setNames(temp_df_formatted$parameter, param_cols)

	# Rename parameter columns in indivNCA
	names(indivNCA) <- ifelse(names(indivNCA) %in% param_cols,
														name_mapping[names(indivNCA)],
														names(indivNCA))

	if (!is.null(splitCovariates)) {
		# Get remaining columns (parameters and non-selected splitCovariates)
		remaining_cols <- setdiff(names(indivNCA), c("id", splitCovariates))

		# Reorder: splitCovariates (left of id, grouping keys leftmost), then id, then rest
		indivNCA <- indivNCA %>%
			dplyr::select(dplyr::all_of(splitCovariates), "id", dplyr::all_of(remaining_cols)) %>%
			dplyr::arrange(!!!rlang::syms(splitCovariates))
	}

	if (isTRUE(mock)) {
		col_types <- stats::setNames(project_data$headerTypes, project_data$header)
		col_types["id"]            <- "id"
		col_types[strat_cov_names] <- "catcov"

		indivNCA <- .mockDataListing(indivNCA, colTypes = col_types, digits = roundDigits)
	}

	attr(indivNCA, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(indivNCA, "r_source")       <- .getRScriptPath()
	return(indivNCA)
}
#-------------------------------------------------------------------------------
#' NCA Ratios Individual Listing
#'
#' Creates a listing table of individual NCA ratio parameter values from a PKanalix project.
#'
#' @param projectFile Character string. Path to the PKanalix project file (.pkx).
#'   If NULL, uses the currently loaded project.
#' @param splitCovariates Character vector. Categorical covariates (type "catcov")
#'   to include and sort by. If specified, the table is sorted by these
#'   splitCovariates in the order provided. Occasion-varying covariates are
#'   rejected, since ratios are computed across occasions and cannot be
#'   stratified by a covariate that changes within subject.
#' @param ratioParams Character vector. Ratio parameters to include. If NULL (default),
#'   all available ratio parameters are included.
#' @param filterCovariates Named list where names are categorical covariate names
#'   and values are vectors of allowed values, e.g.
#'   `list(SEX = "Male", DOSE = c("100", "200"))`. Rows not matching are dropped.
#'   If `NULL` (default), no covariate filtering is applied.
#' @param filterIds Character or numeric vector of subject IDs to include. If
#'   `NULL` (default), no ID filtering is applied.
#' @param roundDigits Numeric. Number of digits for rounding (default: 3).
#' @param roundMethod Character. Rounding method: "monolixSuiteRounding" (default),
#'   "significantFigures", "significantFiguresPreserved", or "none".
#' @param aliases Logical. If TRUE (default), parameter names are replaced with their aliases.
#' @param customAliases Named vector or 2-column dataframe mapping parameter names
#'   to custom alias labels. Example: `c(Cmax_ratio = "C[[sub:max]] ratio")`.
#'   If provided, these override aliases from the PKanalix project. Supports special
#'   formatting syntax: `[[sub:text]]` for subscript, `[[sup:text]]` for superscript,
#'   and `~text~` for italic.
#' @param mock Logical; if `TRUE`, masks the listing for documentation / example
#'   outputs: id -> `"id1"`, categorical / stratification covariates ->
#'   `"Category 1"`, and all numeric columns (continuous covariates, ratio
#'   parameter values) -> `"xx.xx"`. Default `FALSE`.
#'
#' @return A data frame with one row per subject, containing the `id` column,
#'   `splitCovariates` (if specified, placed to the left of `id`), any
#'   occasion-invariant stratification-only / additional-stratification
#'   categorical covariates (merged in automatically when defined in the
#'   project), and the selected (or all) ratio parameter columns, with names
#'   formatted per `aliases`. Ratio parameters are dimensionless, so no `units`
#'   argument is offered.
#'
#' @details
#' The function validates that:
#' - NCA analysis has been run (required for ratios)
#' - NCA ratio module is configured
#' - Specified splitCovariates and parameters exist in the project
#'
#' Stratification-only and additional-stratification categorical covariates
#' (PKanalix) are merged in automatically when available, independent of
#' `splitCovariates`. Occasion-varying stratification covariates are skipped
#' for this merge (with a message) since they are not constant within subject
#' and ratios are computed across occasions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic listing with all ratios
#' ratios <- listingNCARatios(projectFile = "path/to/project.pkx")
#'
#' # With covariate stratification and parameter filtering
#' ratios <- listingNCARatios(
#'   projectFile = "path/to/project.pkx",
#'   splitCovariates = c("SEX", "DOSE"),
#'   ratioParams = c("AUC_ratio", "Cmax_ratio"),
#'   roundDigits = 2
#' )
#'
#' # Restrict to specific subjects and covariate values
#' ratios <- listingNCARatios(
#'   filterCovariates = list(SEX = "Male"),
#'   filterIds        = c(1, 2, 17)
#' )
#'
#' # Masked listing for documentation / example outputs
#' ratios <- listingNCARatios(mock = TRUE)
#' }
listingNCARatios <- function(projectFile      = NULL,
														 splitCovariates  = NULL,
														 ratioParams      = NULL,
														 filterCovariates = NULL,
														 filterIds        = NULL,
														 roundDigits      = 3,
														 roundMethod      = c("monolixSuiteRounding", "significantFigures", "significantFiguresPreserved", "none"),
														 aliases          = TRUE,
														 customAliases   = NULL,
														 mock             = FALSE){

	roundMethod <- match.arg(roundMethod)
	# Ensure the correct project is loaded
	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "ncaRatio")

	# Validate filter arguments (rejects occasion-varying filter keys for ratios)
	.validateFilters(filterCovariates, filterIds, resultType = "ncaRatio")

	# Validate set splitCovariates (rejects occasion-varying split covariates for ratios)
	.validateCovariateSelection(splitCovariates, ratio = TRUE)
	# Validate set parameters
	.validateNCAParametersSelection(ratioParams, ratio = TRUE)

	# Normalize custom alias mapping
	aliases_df        <- .normalizeAliasMapping(customAliases, "aliases")

	indivRatios       <- lixoftConnectors::getNCAIndividualRatios()

	cov_info          <- lixoftConnectors::getCovariateInformation()
	strat_cov_names   <- cov_info$name[cov_info$type %in% c("stratificationcategorical", "additionalstratificationcategorical")]
	merged_strat_covs <- character(0)
	if (length(strat_cov_names) > 0) {
		invariance   <- .getOccasionInvariantCovariates(cov_info, covariates = strat_cov_names)
		dropped_covs <- invariance$varying

		if (length(dropped_covs) > 0) {
			message(sprintf(
				"Skipping occasion-varying stratification covariate(s) for ratio merge: %s (not constant within subject).",
				paste(dropped_covs, collapse = ", ")))
		}

		if (length(invariance$invariant) > 0) {
			merged_strat_covs <- invariance$invariant
			strat_cov_df      <- unique(cov_info$covariate[, c("id", merged_strat_covs), drop = FALSE])
			indivRatios       <- merge(indivRatios, strat_cov_df, by = "id", all.x = TRUE, sort = FALSE)
		}
	}
	# ---------------------------------------------------------------------------

	# Apply filters
	indivRatios <- .applyFilters(indivRatios, filterCovariates, filterIds, id_col = "id")

	# Get ALL available splitCovariates from data
	project_data   <- lixoftConnectors::getData()
	all_splitCovariates <- project_data$header[project_data$headerTypes %in% c("catcov","contcov", "occ")]
	#intersection between all splitCovariates available in data and in ratios listing
	all_splitCovariates <- intersect(all_splitCovariates, names(indivRatios))

	# Filter to selected parameters if ratioParams is specified
	if (!is.null(ratioParams)) {
		# Keep id, all splitCovariates, merged strat covariates, and selected parameters only
		cols_to_keep <- c("id", all_splitCovariates, merged_strat_covs, ratioParams)
		# Only keep columns that exist in the data
		cols_to_keep <- intersect(cols_to_keep, names(indivRatios))
		indivRatios <- indivRatios %>% dplyr::select(dplyr::all_of(cols_to_keep))
	}

	# Apply rounding to parameter columns (BEFORE renaming)
	indivRatios <- .round_num(
		to_round        = indivRatios,
		digits          = roundDigits,
		rounding_method = roundMethod,
		exclude         = c("id", all_splitCovariates, merged_strat_covs))

	# Identify parameter columns (exclude id, all splitCovariates and merged strat covariates)
	param_cols <- setdiff(names(indivRatios), c("id", all_splitCovariates, merged_strat_covs))

	# Create temporary data frame with parameter names
	temp_df <- data.frame(parameter = param_cols, stringsAsFactors = FALSE)

	# Apply units/aliases formatting
	temp_df_formatted <- .addUnitsAliases(
		temp_df,
		add_units   = FALSE,
		add_aliases = aliases,
		units_df    = NULL,
		aliases_df  = aliases_df)

	# Create mapping: original name -> formatted name
	name_mapping <- setNames(temp_df_formatted$parameter, param_cols)

	# Rename parameter columns in indivNCA
	names(indivRatios) <- ifelse(names(indivRatios) %in% param_cols,
															 name_mapping[names(indivRatios)],
															 names(indivRatios))

	if (!is.null(splitCovariates)) {
		# Get remaining columns (parameters and non-selected splitCovariates)
		remaining_cols <- setdiff(names(indivRatios), c("id", splitCovariates))

		# Reorder: splitCovariates (left of id, grouping keys leftmost), then id, then rest
		indivRatios <- indivRatios %>%
			dplyr::select(dplyr::all_of(splitCovariates), "id", dplyr::all_of(remaining_cols)) %>%
			dplyr::arrange(!!!rlang::syms(splitCovariates))
	}

	if (isTRUE(mock)) {
		col_types <- stats::setNames(project_data$headerTypes, project_data$header)
		col_types["id"]              <- "id"
		col_types[merged_strat_covs] <- "catcov"

		indivRatios <- .mockDataListing(indivRatios, colTypes = col_types, digits = roundDigits)
	}

	attr(indivRatios, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(indivRatios, "r_source")       <- .getRScriptPath()
	return(indivRatios)
}
