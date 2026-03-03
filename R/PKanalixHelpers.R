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
#' stratification by covariates. Transforms concentration data from wide to long format
#' and provides summary statistics across time points.
#'
#' @param projectFile Character string specifying the path to a PKanalix project file (.pkx).
#'   If `NULL` (default), uses the currently loaded project. The function will load the
#'   specified project if it differs from the currently active one.
#'
#' @param covariates Character vector of covariate names to stratify results by
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
#' @param stratify_columns Logical; if `TRUE`, creates a stratified table layout with
#'   separate columns for each covariate combination plus an "Overall" column. If `FALSE`
#'   (default), covariates appear as separate columns on the left side of the table.
#'   Only applicable when `covariates` is specified.
#'
#' @param covariate_labels Character vector of custom labels for covariates, must be the
#'   same length as `covariates`. If `NULL` (default), the original covariate names are
#'   used. These labels appear in column headers when `stratify_columns = TRUE`.
#'
#' @param roundDigits Integer specifying the number of decimal places for rounding
#'   numeric values. Default is `3`.
#'
#' @param roundMethod Character string specifying the rounding method. Options are:
#'   - `"monolix"` (default): Adaptive rounding that adds extra digits for values < 1
#'   - `"signif"`: Standard significant figures rounding
#'   - `"none"`: No rounding performed
#'
#' @param prettyLabels Logical; if `TRUE` (default), converts column names to
#'   human-readable labels (e.g., "CV" becomes "CV (%)", "geoMean" becomes "Geometric Mean").
#'   If `FALSE`, uses raw column names.
#'
#' @details
#' The function resets stratification via `lixoftConnectors::setNCAResultsStratification()` and
#' optionally splits results by `covariates`. Concentration data are retrieved
#' with `lixoftConnectors::getNCAConcentrations()`, using `useNominalTime` only if nominal
#' time is set and available. Covariate column order is corrected with
#' `fixNCAConcentrationsColumnSwap()`. Only summary rows (statistics) are kept,
#' excluding individual profiles. Data are reshaped into long format, numeric values
#' are rounded, and the results are returned in wide format with one row per
#' timepoint/covariate combination.
#'
#' @return A data frame containing concentration summary statistics organized by time points
#'         and optionally stratified by covariates.
#'
#' @examples
#' # Overall summary (no stratification)
#' # tableNCAConcentration()
#'
#' # Stratified by treatment, all metrics
#' # tableNCAConcentration(covariates = "TREAT")
#'
#' # Stratified by multiple covariates with column layout
#' # tableNCAConcentration(
#' #   covariates = c("DOSE", "FOOD"),
#' #   stratify_columns = TRUE
#' # )
#'
#' # Custom metrics and labels
#' # tableNCAConcentration(
#' #   covariates = c("SEX", "AGE_CAT"),
#' #   metrics = c("GeomCV", "MeanSD"),
#' #   covariate_labels = c("Sex", "Age Category"),
#' #   stratify_columns = TRUE
#' # )
#'
tableNCAConcentration <- function(projectFile      = NULL,
																	covariates       = NULL,
																	useNominalTime   = TRUE,
																	metrics          = NULL,
																	stratify_columns = FALSE,
																	covariate_labels = NULL,
																	roundDigits      = 3,
																	roundMethod      = c("monolix", "signif", "none"),
																	prettyLabels     = TRUE) {

	roundMethod <- match.arg(roundMethod)

	# ==========================================================================
	# 1. VALIDATION & SETUP
	# ==========================================================================

	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "nca")

	# Validate covariates, stratification and covariate label arguments
	.validateCovariateSelection(covariates, ratio = FALSE)
	stratify_columns <- .validateStratificationArgs(covariates, stratify_columns)
	covariate_labels <- .validateCovariateLabels(covariates, covariate_labels)

	# ==========================================================================
	# Fetch Stratified Data (if covariates exist)
	if (!is.null(covariates)) {
		raw_strat <- .prepareNCASummaryStats(
			project = projectFile, NCAparams = NULL, covariates = covariates, roundDigits = roundDigits, roundMethod = roundMethod,
			units = FALSE, aliases = FALSE, by_time = TRUE, useNominalTime = useNominalTime)
	} else {
		raw_strat <- NULL
	}

	# Fetch Overall Data (Always needed if we want an Overall column or if no covariates exist)
	raw_overall <- .prepareNCASummaryStats(
		project = projectFile, NCAparams = NULL, covariates = NULL, roundDigits = roundDigits, roundMethod = roundMethod,
		units = FALSE, aliases = FALSE, by_time = TRUE, useNominalTime = useNominalTime)

	if (is.null(raw_overall) && is.null(raw_strat)) stop("No NCA results available.")

	# Define Time Column
	time_col <- "Time"

	# ==========================================================================
	# TABLE FORMATTING
	if (stratify_columns && !is.null(covariates)) {
		# ----------------------------------------------------------------------
		# CASE: Stratification (Vertical columns for covariates + Overall)
		# ----------------------------------------------------------------------

		# Validate Metrics existence
		available_cols   <- names(raw_strat)
		descriptor_cols  <- c(time_col, covariates, "Nobs")
		selected_metrics <- .validateAndSelectMetrics(available_cols, metrics, descriptor_cols)

		# Prepare Labels
		if (is.null(covariate_labels)) labels <- covariates else labels <- covariate_labels

		# Process Stratified Data (Create Header)
		# IMPORTANT: strictly select only what we need to avoid dupes
		df_prepared <- raw_strat %>%
			dplyr::select(dplyr::all_of(time_col), dplyr::all_of(covariates), dplyr::all_of(selected_metrics)) %>%
			dplyr::rowwise() %>%
			dplyr::mutate(header = {
				values <- dplyr::c_across(dplyr::all_of(covariates))
				# Logic: "Label=Value"
				pieces <- paste0(labels, "=", as.character(values))
				paste(pieces, collapse = "\n")
			}) %>%
			dplyr::ungroup()

		# 4. Process Overall Data
		df_overall_clean <- raw_overall %>%
			dplyr::select(dplyr::all_of(time_col), dplyr::all_of(selected_metrics))


		# 5. PIVOT LOGIC
		#    Distinguish between Single Metric vs. Multiple Metrics

		if (length(selected_metrics) > 1) {
			# MULTIPLE METRICS:
			# Target Format: Time | Statistic | Stratum1 | Stratum2 | ... | Overall

			# Pivot Stratified
			df_strat_wide <- df_prepared %>%
				dplyr::select(dplyr::all_of(time_col), header, dplyr::all_of(selected_metrics)) %>%
				dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
				tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Value") %>%
				tidyr::pivot_wider(names_from = header, values_from = Value)

			# Pivot Overall
			df_overall_long <- df_overall_clean %>%
				dplyr::mutate(dplyr::across(dplyr::all_of(selected_metrics), as.character)) %>%
				tidyr::pivot_longer(cols = dplyr::all_of(selected_metrics), names_to = "Statistic", values_to = "Overall")

			# Join
			final_df <- df_strat_wide %>%
				dplyr::left_join(df_overall_long, by = c(time_col, "Statistic"))
		} else {
			# SINGLE METRIC:
			# Target Format: Time | Stratum1 | Stratum2 | ... | Overall
			# (No "Statistic" column needed, or kept implicitly if desired, but typically removed for single metric tables)

			metric_name <- selected_metrics[1]

			# Pivot Stratified
			df_strat_wide <- df_prepared %>%
				dplyr::select(dplyr::all_of(time_col), header, dplyr::all_of(metric_name)) %>%
				tidyr::pivot_wider(names_from = header, values_from = dplyr::all_of(metric_name))

			# Prepare Overall
			df_overall_renamed <- df_overall_clean %>%
				dplyr::rename(Overall = dplyr::all_of(metric_name))

			# Join
			final_df <- df_strat_wide %>%
				dplyr::left_join(df_overall_renamed, by = time_col)
		}
	} else {
		# ----------------------------------------------------------------------
		# CASE: No Stratification (Just Overall / List by Time)
		# ----------------------------------------------------------------------

		# Use raw_strat if covariates present (but stratify=FALSE), else raw_overall
		source_df <- if (!is.null(covariates)) raw_strat else raw_overall

		descriptor_cols <- c(time_col, covariates) # Keep covariates as columns on the left
		selected_metrics <- .validateAndSelectMetrics(names(source_df), metrics, descriptor_cols)

		# Filter columns
		cols_to_keep <- intersect(c(descriptor_cols, selected_metrics), names(source_df))
		final_df <- source_df[, cols_to_keep, drop = FALSE]

		# Optional: If multiple metrics requested but stratify=FALSE,
		# data is usually wide (Metric1, Metric2 columns).
		# If you want "Statistic" column stacking, add pivot here.
		# Assuming standard behavior is wide for simple lists:
		if (length(selected_metrics) > 1) {
			# Optional: Pivot to long format "Statistic" if preferred for non-stratified too
			# final_df <- final_df %>%
			#   tidyr::pivot_longer(cols=dplyr::all_of(selected_metrics), names_to="Statistic", values_to="Value")
		}
	}

	# ==========================================================================
	# 4. PRETTY LABELS & SORTING
	if (isTRUE(prettyLabels)) {
		final_df <- .prettyLabels(final_df)

		# Apply pretty labels to "Statistic" content if it exists
		if ("Statistic" %in% names(final_df)) {
			curr_metrics <- unique(final_df$Statistic)
			# Create dummy to map labels
			dummy_df <- as.data.frame(matrix(NA, nrow = 1, ncol = length(curr_metrics)))
			names(dummy_df) <- curr_metrics
			dummy_df <- .prettyLabels(dummy_df)
			metric_map <- setNames(names(dummy_df), curr_metrics)

			final_df$Statistic <- metric_map[final_df$Statistic]
		}
	}

	# Sort by Time numerically if possible
	if (time_col %in% names(final_df)) {
		final_df <- final_df %>%
			dplyr::arrange(suppressWarnings(as.numeric(!!rlang::sym(time_col))))
	}

	return(as.data.frame(final_df))
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
#' @param covariates Character vector of covariate names to stratify by
#'   (e.g., `"TREAT"`, `"FOOD"`, or `c("TREAT","FOOD")`). If `NULL`
#'   (default), no stratification is applied and overall summaries are returned.
#'
#' @param NCAparams Character vector of NCA parameter names to include. If
#'   `NULL` (default), all parameters currently enabled in the project are used
#'   (i.e., `lixoftConnectors::getNCASettings()$computedncaparameters`).
#'
#' @param metrics Character vector specifying which summary statistics to include in the
#'   output table. If `NULL` (default), all available metrics are included. Common options
#'   include: `"mean"`, `"SD"`, `"CV"`, `"geoMean"`, `"geoCV"`, `"geoSD"`, `"MeanSD"`,
#'   `"GeomCV"`, `"GeomSD"`, `"median"`, `"min"`, `"max"`, `"P05"`, `"P95"`, `"Nobs"`.
#'
#' @param horizontal Logical. If `TRUE` (default), returns a "wide" table with
#'   one row per covariate level (or overall) and one column per statistic for
#'   each parameter, i.e., PKanalix-like structure. If `FALSE`, returns a
#'   "vertical" table where \code{Statistic} forms the first column and the
#'   parameters form the subsequent columns (useful for reporting).
#'
#' @param stratify_columns Logical; if `TRUE`, creates a stratified table layout with
#'   separate columns for each covariate combination plus an "Overall" column. If `FALSE`
#'   (default), covariates appear as separate columns on the left side of the table.
#'   Only applicable when `covariates` is specified.
#'
#' @param covariate_labels Character vector of custom labels for covariates, must be the
#'   same length as `covariates`. If `NULL` (default), the original covariate names are
#'   used. These labels appear in column headers when `stratify_columns = TRUE`.
#'
#' @param include_id Logical. If `TRUE` and `horizontal = FALSE`, adds individual
#'   subject IDs and their parameter values below the summary statistics in the
#'   "Statistic" column. When stratified by covariates, individual IDs are grouped
#'   by their covariate values and listed after the corresponding summary metrics.
#'   Default is `FALSE`. Only works when `horizontal = FALSE`.
#'
#' @param roundDigits Integer specifying the number of decimal places for rounding
#'   numeric values. Default is `3`.
#'
#' @param roundMethod Character string specifying the rounding method. Options are:
#'   - `"monolix"` (default): Adaptive rounding that adds extra digits for values < 1
#'   - `"signif"`: Standard significant figures rounding
#'   - `"none"`: No rounding performed
#'
#' @param units Logical. If `TRUE` (default), units are appended to parameter names.
#'
#' @param aliases Logical. If `TRUE` (default), parameter aliases (friendly names) are used
#'   instead of internal parameter codes.
#'
#' @param prettyLabels Logical; if `TRUE` (default), converts column names to
#'   human-readable labels (e.g., "CV" becomes "CV (%)", "geoMean" becomes "Geometric Mean").
#'   If `FALSE`, uses raw column names.
#'
#' @return A data frame combining standard NCA statistics with additional descriptive statistics
#'         (Q1, Q3, median, max) for the specified parameters. If `include_id = TRUE`,
#'         also includes individual parameter values for each subject.
#'
#' @examples
#' # Overall (no stratification), all computed parameters, wide layout
#' # tableNCAParameters()
#'
#' # Stratified by a categorical covariate, selected parameters, vertical layout
#' # tableNCAParameters(
#' #   covariates = "FORM",
#' #   NCAparams  = c("AUCINF_obs", "AUClast", "Cmax"),
#' #   roundDigits = 2,
#' #   horizontal = FALSE
#' # )
#'
#' # Vertical layout with individual IDs included
#' # tableNCAParameters(
#' #   horizontal = FALSE,
#' #   include_id = TRUE,
#' #   NCAparams  = c("Cmax", "AUClast", "Lambda_z")
#' # )
#'
#' # Stratified with individual IDs
#' # tableNCAParameters(
#' #   covariates = "TREAT",
#' #   horizontal = FALSE,
#' #   include_id = TRUE
#' # )
#'
tableNCAParameters <- function(projectFile      = NULL,
															 covariates       = NULL,
															 NCAparams        = NULL,
															 metrics          = NULL,
															 horizontal       = TRUE,
															 stratify_columns = FALSE,
															 covariate_labels = NULL,
															 include_id       = FALSE,
															 roundDigits      = 3,
															 roundMethod      = c("monolix", "signif", "none"),
															 units            = TRUE,
															 aliases          = TRUE,
															 prettyLabels     = TRUE) {

	roundMethod <- match.arg(roundMethod)

	# ==========================================================================
	# VALIDATION & SETUP

	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "nca")

	# Validate covariates, stratification and covariate label arguments
	.validateCovariateSelection(covariates, ratio = FALSE)
	stratify_columns <- .validateStratificationArgs(covariates, stratify_columns)
	covariate_labels <- .validateCovariateLabels(covariates, covariate_labels)
	# Validate set parameters
	.validateNCAParametersSelection(NCAparams, ratio = FALSE)
	# Validate include_id parameter
	if (include_id && horizontal) {
		warning("include_id = TRUE only works with horizontal = FALSE. Setting include_id to FALSE.")
		include_id <- FALSE
	}

	if (include_id && stratify_columns) {
		warning("include_id = TRUE is not yet supported with stratify_columns = TRUE. Setting include_id to FALSE.")
		include_id <- FALSE
	}

	# ==========================================================================
	# TABLE FORMATTING

	if (stratify_columns) {
		if (!horizontal) {
			message("Note: stratify_columns=TRUE uses a different table format. The 'horizontal' argument is ignored.")
		}

		# Determine Metrics
		if (is.null(metrics)) {
			user_metrics <- "GeomCV"
			message("No metric specified for stratified table. Defaulting to 'GeomCV'. To show multiple metrics specify metrics explicitly.")
		} else {
			user_metrics <- metrics
		}

		# Get stratified data
		summary_stats <- .prepareNCASummaryStats(projectFile, NCAparams, covariates, roundDigits, roundMethod, units, aliases, by_time = FALSE, useNominalTime = FALSE)

		if (is.null(summary_stats)) stop("No NCA results available.")

		# Identify parameter column
		param_col <- "parameter"
		if (!(param_col %in% names(summary_stats))) {
			param_col <- intersect(names(summary_stats), c("parameter", "Parameter"))[1]
		}

		# Validate metrics
		descriptor_cols  <- c(param_col, covariates, "Nobs")
		selected_metrics <- .validateAndSelectMetrics(
			available_cols = names(summary_stats),
			user_metrics   = user_metrics,
			descriptors    = descriptor_cols)

		# Prepare overall stats if covariates provided
		if (!is.null(covariates)) {
			overall_stats <- .prepareNCASummaryStats(projectFile, NCAparams, NULL, roundDigits, roundMethod, units, aliases, by_time = FALSE, useNominalTime = FALSE)

			# Safety check
			if (!all(selected_metrics %in% names(overall_stats))) {
				stop("Mismatch in metrics between Stratified and Overall results.")
			}
		} else {
			overall_stats <- NULL
		}

		# Use internal helper to build stratified table
		summary_stats <- .buildStratifiedTable(
			summary_stats    = summary_stats,
			overall_stats    = overall_stats,
			covariates       = covariates,
			selected_metrics = selected_metrics,
			param_col        = param_col,
			covariate_labels = covariate_labels)
	} else {
		# Non-stratified or horizontal mode
		summary_stats <- .prepareNCASummaryStats(projectFile, NCAparams, covariates, roundDigits, roundMethod, units, aliases, by_time = FALSE, useNominalTime = FALSE)
		if (is.null(summary_stats)) stop("No NCA results available.")

		# Identify parameter column
		param_col <- "parameter"
		if (!(param_col %in% names(summary_stats))) {
			param_col <- intersect(names(summary_stats), c("parameter", "Parameter"))[1]
		}

		# Determine metrics
		descriptor_cols  <- c(param_col, covariates, "Nobs")
		selected_metrics <- .validateAndSelectMetrics(
			available_cols = names(summary_stats),
			user_metrics   = metrics,
			descriptors    = descriptor_cols)

		# Filter dataframe
		cols_to_keep  <- intersect(c(descriptor_cols, selected_metrics), names(summary_stats))
		summary_stats <- summary_stats[, cols_to_keep, drop = FALSE]

		if (!horizontal) {
			# Use internal helper for vertical pivot
			summary_stats <- .pivotToVertical(
				summary_stats = summary_stats,
				param_col     = param_col,
				covariates    = covariates)
		}
	}

	# ==========================================================================
	# ADD INDIVIDUAL IDs (if requested and horizontal = FALSE)

	if (!horizontal && include_id) {
		summary_stats <- .addIndividualsNCAParameters(
			summary_stats = summary_stats,
			covariates    = covariates,
			NCAparams     = NCAparams,
			roundDigits   = roundDigits,
			roundMethod   = roundMethod,
			units         = units,
			aliases       = aliases)
	}

	# ==========================================================================
	# SORT BY COVARIATES (for vertical tables with numeric-valued categorical covariates)

	if (!horizontal && !stratify_columns && !is.null(covariates) && length(covariates) > 0) {
		summary_stats <- summary_stats %>%
			dplyr::arrange(dplyr::across(
				dplyr::all_of(covariates),
				~{
					num <- suppressWarnings(as.numeric(.x))
					if(all(is.na(num))) .x else num
				}
			))
	}

	# ==========================================================================
	# PRETTY LABELS & FINALIZATION

	if (isTRUE(prettyLabels)) {
		summary_stats <- .prettyLabels(summary_stats)

		# Apply pretty labels to the content of the 'Statistic' column if it exists
		if ("Statistic" %in% names(summary_stats)) {
			curr_metrics    <- unique(summary_stats$Statistic)
			dummy_df        <- as.data.frame(matrix(NA, nrow = 1, ncol = length(curr_metrics)))
			names(dummy_df) <- curr_metrics
			dummy_df        <- .prettyLabels(dummy_df)
			metric_map      <- setNames(names(dummy_df), curr_metrics)

			summary_stats$Statistic <- metric_map[summary_stats$Statistic]
		}
	}
	# ==========================================================================
	# SET ATTRIBUTES for print_table_flex
	# To recognize this as NCA table with sections
	if (include_id && "Section" %in% names(summary_stats)) {
		attr(summary_stats, "table_type") <- "nca"
		attr(summary_stats, "format")     <- "long"  # Always long when include_id=TRUE
	}
	return(as.data.frame(summary_stats))
}


#-------------------------------------------------------------------------------
#' tableNCARatios
#'
#' Create a summary table of individual NCA ratio results with optional
#' stratification by covariates (e.g., DOSE, VISIT).
#'
#' The function:
#'  1. Pulls individual ratios (`lixoftConnectors::getNCAIndividualRatios()`) and ratio info (`lixoftConnectors::getNCARatios()`).
#'  2. Identifies candidate grouping variables (all columns except id and ratio columns).
#'  3. Reshapes the data into long format (Parameter, Ratio).
#'  4. Summarises across subjects to provide descriptive statistics (N, Min, Q1, Mean, Median, Q3, Max).
#'  5. Returns results either in wide format (one row per Parameter × group) or long format (Statistic/Value pairs).
#'
#' @param covariates character vector of column names to stratify by (must be in indivratio but not id/ratio columns).
#' @param digits number of digits to round numeric statistics (default = 3).
#' @param horizontal logical; if TRUE (default) results are wide; if FALSE, results are pivoted to long format.
#'
#' @return A data.frame with summary statistics for each ratio parameter, optionally stratified.
#'
#' @examples
#' # Overall summary (all subjects, no stratification):
#' tableNCARatios()
#'
#' # Stratified by DOSE, results in wide format:
#' tableNCARatios(covariates = "DOSE", horizontal = TRUE)
#'
#' # Stratified by DOSE and VISIT, results in long format:
#' tableNCARatios(covariates = c("DOSE", "VISIT"), horizontal = FALSE)
#'
tableNCARatios <- function(projectFile      = NULL,
													 covariates       = NULL,
													 stratify_columns = FALSE,
													 covariate_labels = NULL,
													 ratioParams      = NULL,
													 metrics          = NULL,
													 horizontal       = TRUE,
													 roundDigits      = 3,
													 roundMethod      = c("monolix", "signif", "none"),
													 prettyLabels     = TRUE) {
	roundMethod <- match.arg(roundMethod)

	# ==========================================================================
	# VALIDATION & SETUP
	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "ncaRatio")

	# Validate covariates, stratification and covariate label arguments
	.validateCovariateSelection(covariates, ratio = TRUE)
	stratify_columns <- .validateStratificationArgs(covariates, stratify_columns)
	covariate_labels <- .validateCovariateLabels(covariates, covariate_labels)

	# Validate set parameters
	.validateNCAParametersSelection(ratioParams, ratio = TRUE)  # Early validation

	ratio_info <- lixoftConnectors::getNCARatios()
	ratio_cols <- names(ratio_info)

	# Determine which ratio columns to pivot (ADD THIS BLOCK)
	if (!is.null(ratioParams)) {
		# Pivot only requested parameters (already validated)
		cols_to_pivot <- intersect(ratioParams, ratio_cols)
	} else {
		# Pivot all ratio columns
		cols_to_pivot <- ratio_cols
	}

	# Candidate group-by columns: in dataframe but not id and not ratio(s)
	indivratio <- lixoftConnectors::getNCAIndividualRatios()
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

	if (stratify_columns && !is.null(covariates)) {
		# -----------------------------------------------------------------------
		# CASE: Stratified columns (covariates as column headers with Overall)
		if (!horizontal) {
			message("Note: stratify_columns=TRUE uses a different table format. The 'horizontal' argument is ignored.")
		}

		# Default to GeomCV for stratified tables if no metrics specified
		if (is.null(metrics)) {
			metrics <- "GeomCV"
			message("No metric specified for stratified table. Defaulting to 'GeomCV'. To show multiple metrics specify metrics explicitly.")
		}

		# Summarize stratified data
		summary_stats <- .summarizeStats(
			df         = long_df,
			group_vars = c(covariates, "Parameter"),
			value_col  = "Ratio",
			id_col     = "id") %>%
			dplyr::arrange(dplyr::across(dplyr::all_of(c(covariates, "Parameter"))))

		# Apply rounding
		summary_stats <- .round_num(
			to_round        = summary_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = c("Parameter", covariates))

		# Add combined metrics
		summary_stats <- .combinedStats(summary_stats)

		# Rename N to Nobs for consistency with helper
		if ("N" %in% names(summary_stats)) {
			summary_stats <- summary_stats %>% dplyr::rename(Nobs = N)
		}

		# Validate metrics
		descriptor_cols <- c("Parameter", covariates, "Nobs")
		selected_metrics <- .validateAndSelectMetrics(
			available_cols = names(summary_stats),
			user_metrics   = metrics,
			descriptors    = descriptor_cols)

		# Get overall stats
		overall_stats <- .summarizeStats(
			df         = long_df,
			group_vars = "Parameter",
			value_col  = "Ratio",
			id_col     = "id") %>%
			dplyr::arrange(Parameter)

		# Apply rounding to overall
		overall_stats <- .round_num(
			to_round        = overall_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = "Parameter")

		# Add combined metrics to overall
		overall_stats <- .combinedStats(overall_stats)

		# Rename N to Nobs for consistency
		if ("N" %in% names(overall_stats)) {
			overall_stats <- overall_stats %>% dplyr::rename(Nobs = N)
		}

		# Build stratified table using helper
		summary_tbl <- .buildStratifiedTable(
			summary_stats    = summary_stats,
			overall_stats    = overall_stats,
			covariates       = covariates,
			selected_metrics = selected_metrics,
			param_col        = "Parameter",
			covariate_labels = covariate_labels)

	} else {
		# -----------------------------------------------------------------------
		# CASE: No stratification or regular row-based grouping
		# Create summary table & summarise across subjects
		if (length(covariates) > 0) {
			summary_tbl <- .summarizeStats(
				df         = long_df,
				group_vars = c(covariates, "Parameter"),
				value_col  = "Ratio",
				id_col     = "id") %>%
				dplyr::arrange(dplyr::across(dplyr::all_of(c(covariates, "Parameter"))))
		} else {
			summary_tbl <- .summarizeStats(
				df         = long_df,
				group_vars = "Parameter",
				value_col  = "Ratio",
				id_col     = "id") %>%
				dplyr::arrange(Parameter)
		}

		# Apply rounding
		summary_tbl <- .round_num(
			to_round        = summary_tbl,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = c("Parameter", covariates))

		# Add combined metrics
		summary_tbl <- .combinedStats(summary_tbl)

		# Filter to specific metrics (if requested)
		descriptor_cols <- c("Parameter", covariates, "N")

		selected_metrics <- .validateAndSelectMetrics(
			available_cols = names(summary_tbl),
			user_metrics   = metrics,
			descriptors    = descriptor_cols)

		summary_tbl <- summary_tbl[, c(descriptor_cols, selected_metrics), drop = FALSE]

		# Pivot to vertical if requested
		if (horizontal == FALSE) {
			# Use helper function for vertical pivot
			param_col   <- intersect(names(summary_tbl), c("parameter", "Parameter"))
			summary_tbl <- .pivotToVertical(summary_tbl, param_col, covariates)
		}
	}

	# ==========================================================================
	# PRETTY LABELS

	if (isTRUE(prettyLabels)) {
		summary_tbl <- .prettyLabels(summary_tbl)

		# Apply pretty labels to Statistic column content if it exists
		if ("Statistic" %in% names(summary_tbl)) {
			curr_metrics <- unique(summary_tbl$Statistic)
			dummy_df <- as.data.frame(matrix(NA, nrow = 1, ncol = length(curr_metrics)))
			names(dummy_df) <- curr_metrics
			dummy_df <- .prettyLabels(dummy_df)
			metric_map <- setNames(names(dummy_df), curr_metrics)

			summary_tbl$Statistic <- metric_map[summary_tbl$Statistic]
		}
	}

	return(as.data.frame(summary_tbl))
}





################################################################################
# Helpers for Listings

#' listingAbbreviation
#'
#' Produces a two-column abbreviation listing initialized from an input table,
#' returning unique abbreviations and an empty \code{Full Term} column for later
#' completion, sorted alphabetically.
#'
#' @param data A data frame containing an abbreviation column or a \code{parameter}
#'   column that will be mapped to the abbreviation list.
#' @param abbrev_col Character scalar; name of the input column that holds the
#'   abbreviations. Defaults to \code{"Abbreviation"}. If not present, the
#'   function will construct it from \code{data$parameter}.
#'
#' @return A data frame with columns \code{Abbreviation} and \code{Full Term},
#'   de-duplicated and sorted.
#'
#' @examples
#' # listingAbbreviation(NCAParamSummary_info)
#' # listingAbbreviation(df, abbrev_col = "abbr")
# listingAbbreviation	 <- function(data, abbrev_col = "Abbreviation") {
#' #  dt <- data.frame(Abbreviation = data$parameter, stringsAsFactors = FALSE)
#' #  abbrev_listing <- dt %>%
#' #    distinct(!!rlang::sym(abbrev_col)) %>%
#' #    dplyr::rename(Abbreviation = !!rlang::sym(abbrev_col)) %>%
#' #    dplyr::mutate(`Full Term` = NA_character_) %>%
#' #    dplyr::arrange(Abbreviation)
#' #  return(abbrev_listing)
# }
listingAbbreviation <- function(data,
																abbrev_col = "Abbreviation",
																fullterm_col = "Full Term") {
	# Validate: data must have a 'parameter' column
	if (!"parameter" %in% names(data)) {
		stop("Input data must contain a column named 'parameter'.")
	}

	# Create dataframe with abbreviations from parameter column
	dt <- data.frame(Abbreviation = data$parameter, stringsAsFactors = FALSE)

	# Build listing with optional renaming
	abbrev_listing <- dt %>%
		distinct(Abbreviation) %>%
		dplyr::mutate(`Full Term` = NA_character_) %>%
		dplyr::arrange(Abbreviation)

	# Rename columns dynamically according to user input
	abbrev_listing <- abbrev_listing %>%
		dplyr::rename(
			!!abbrev_col := Abbreviation,
			!!fullterm_col := `Full Term`
		)

	return(abbrev_listing)
}

#-------------------------------------------------------------------------------
#' listingNCAConcentration
#'
#' Creates a long-format listing of individual concentrations by time, with
#' optional stratification columns (\code{covariates}). Summary metric rows are
#' removed, and the listing is left-joined with used concentration data
#' (including censoring flags). Occasion-level identifiers are concatenated to
#' form unique subject IDs when applicable.
#'
#' @param covariates Character vector of covariate names to include as
#'   stratification columns (e.g., \code{c("TREAT","FOOD")}); \code{NULL} for no
#'   stratification.
#'
#' @param useNominalTime Logical; if \code{TRUE}, the function attempts to use
#'   nominal time for the concentration listing. If nominal time is not available
#'   in the project (checked via \code{lixoftConnectors::getData()} and \code{headerTypes ==
#'   "nominaltime"}), the function will fall back to actual time and issue a
#'   warning. Default is \code{FALSE}.
#'
#' @param roundDigits Integer specifying the number of decimal places for rounding
#'   numeric values. Default is `3`.
#'
#' @param roundMethod Character string specifying the rounding method. Options are:
#'   - `"monolix"` (default): Adaptive rounding that adds extra digits for values < 1
#'   - `"signif"`: Standard significant figures rounding
#'   - `"none"`: No rounding performed
#'
#' @details
#'   The function first resets stratification via
#'   \code{lixoftConnectors::setNCAResultsStratification()} and optionally splits results by
#'   \code{covariates}.
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
#'   - If occasion-level covariates exist, they are concatenated to the subject
#'   ID using \code{sep = "#"} to ensure unique identifiers.
#'
#' @return A data frame in long format containing (if present) \code{covariates},
#'   \code{id}, \code{ActualTime}, \code{NominalTime} (if available),
#'   \code{Concentration}, \code{UsedConc}, and \code{Censoring}.
#'
#' @examples
#' # Default: actual time only
#' listingNCAConcentration()
#'
#' # With covariates
#' listingNCAConcentration(covariates = c("TREAT", "FOOD"))
#'
#' # Include nominal time if available
#' listingNCAConcentration(covariates = c("TREAT", "FOOD"), useNominalTime = TRUE)
#'
listingNCAConcentration <- function(projectFile    = NULL,
																		covariates     = NULL,
																		useNominalTime = FALSE,
																		roundDigits    = 3,
																		roundMethod    = "none") {

	# Ensure the correct project is loaded
	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "nca")
	# Validate set covariates
	.validateCovariateSelection(covariates, ratio = FALSE)

	# Get occasion variables from data and add to stratification
	project_data <- lixoftConnectors::getData()
	occ_vars     <- project_data$header[project_data$headerTypes == "occ"]

	# Add occasion variables to covariates
	if (length(occ_vars) > 0) {
		covariates <- unique(c(covariates, occ_vars))
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
	if (!is.null(covariates)) {
		lixoftConnectors::setNCAResultsStratification(split = covariates)
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
	df_actual     <- .fixNCAConcentrationsColumnSwap(df_actual, covariates)
	metrics       <- lixoftConnectors::getNCAConcentrations()$metric
	metric_labels <- as.character(metrics[!is.na(metrics)])

	# Process actual time data
	indiv_actual <- df_actual %>%
		dplyr::filter(!(subject %in% metric_labels)) %>% # drop summary rows
		dplyr::mutate(dplyr::across(setdiff(names(.), c("subject", covariates)), as.character)) %>%
		tidyr::pivot_longer(
			cols = setdiff(names(.), c("subject", covariates)), # pivot from wide format to long
			names_to = "ActualTimeLabel",
			values_to = "Concentration") %>%
		dplyr::group_by(subject, dplyr::across(dplyr::all_of(covariates))) %>%
		dplyr::filter(Concentration != "") %>% # drop empty cells
		dplyr::mutate(row_id = row_number()) %>% # assign row index (per subject)
		dplyr::ungroup() %>%
		dplyr::mutate(
			Time = as.numeric(ActualTimeLabel), # numeric actual time
			id   = as.character(subject)) %>%
		dplyr::relocate(dplyr::any_of(covariates), id, Time, Concentration, row_id) %>%
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
			df_nom <- .fixNCAConcentrationsColumnSwap(df_nom, covariates)

			indiv_nom <- df_nom %>%
				dplyr::filter(!(subject %in% metric_labels)) %>%
				dplyr::mutate(dplyr::across(setdiff(names(.), c("subject", covariates)), as.character)) %>%
				tidyr::pivot_longer(
					cols = setdiff(names(.), c("subject", covariates)),
					names_to = "NominalTimeLabel",
					values_to = "dummyConc") %>%
				dplyr::group_by(subject, dplyr::across(dplyr::all_of(covariates))) %>%
				dplyr::filter(dummyConc != "") %>%
				dplyr::mutate(row_id = row_number()) %>% # align by row_id (same column order)
				dplyr::ungroup() %>%
				dplyr::mutate(
					NominalTime = as.numeric(NominalTimeLabel),
					id = as.character(subject)) %>%
				dplyr::relocate(dplyr::any_of(covariates), id, NominalTime, row_id) %>%
				dplyr::select(-subject)

			# Join actual + nominal
			indiv <- indiv_actual %>%
				dplyr::left_join(indiv_nom, by = c("id", covariates, "row_id")) %>%
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
		dplyr::mutate(dplyr::across(dplyr::all_of(covariates), as.character)) %>%
		dplyr::select(-BLQ, -includedForLambdaZ)

	# Rename Time column. Depends on ifactual time is available in dataset
	if (use_nominal_for_actual) {
		indiv <- indiv %>% dplyr::rename(NominalTime = Time)
	} else {
		indiv <- indiv %>% dplyr::rename(ActualTime = Time)
	}

	# Continue cleanup
	indiv <- indiv %>%
		dplyr::relocate(dplyr::any_of(c("ActualTime", "NominalTime")), .before = Concentration) %>%
		dplyr::relocate(dplyr::any_of(occ_vars), .after = id) %>%
		dplyr::mutate(.id_numeric = suppressWarnings(as.numeric(id))) %>% # Try converting to numeric
		dplyr::arrange(
			# Sort by numeric if possible, otherwise by character
			dplyr::coalesce(.id_numeric, Inf), # Numeric IDs first, non-numeric at end
			id,                                # Then by character (for non-numeric IDs)
			!!!rlang::syms(occ_vars),
			dplyr::across(dplyr::any_of(c("ActualTime", "NominalTime")))) %>%
		dplyr::select(-.id_numeric)  # Remove helper column

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


	return(as.data.frame(indiv))
}

#-------------------------------------------------------------------------------
#' NCA Individual Parameters Listing
#'
#' Creates a listing table of individual NCA parameter values from a PKanalix project.
#'
#' @param projectFile Character string. Path to the PKanalix project file (.pkx).
#'   If NULL, uses the currently loaded project.
#' @param covariates Character vector. Covariates to include and sort by. If specified,
#'   the table is sorted by these covariates in the order provided.
#' @param NCAparams Character vector. NCA parameters to include. If NULL (default),
#'   all computed NCA parameters are included.
#' @param roundDigits Numeric. Number of digits for rounding (default: 3).
#' @param roundMethod Character. Rounding method: "monolix" (default), "signif", or "none".
#' @param units Logical. If TRUE (default), parameter units are added to column names.
#' @param aliases Logical. If TRUE (default), parameter names are replaced with their aliases.
#'
#' @return A data frame containing individual NCA parameter values with columns for ID,
#'   covariates (if specified), and NCA parameters.
#'
#' @details
#' The function validates that NCA analysis has been run and that specified covariates
#' and parameters exist in the project. Parameter names can be formatted with units
#' and/or aliases for improved readability.
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
#'   covariates = c("SEX", "DOSE"),
#'   NCAparams = c("AUC", "Cmax", "Tmax"),
#'   roundDigits = 2,
#'   units = TRUE,
#'   aliases = TRUE
#' )
#' }
listingNCAParameters <- function(projectFile = NULL,
																 covariates  = NULL,
																 NCAparams   = NULL,
																 roundDigits = 3,
																 roundMethod = c("monolix", "signif", "none"),
																 units       = TRUE,
																 aliases     = TRUE){

	roundMethod <- match.arg(roundMethod)
	# Ensure the correct project is loaded
	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "nca")
	# Validate set covariates
	.validateCovariateSelection(covariates, ratio = FALSE)
	# Validate set parameters
	.validateNCAParametersSelection(NCAparams, ratio = FALSE)

	indivNCA <- lixoftConnectors::getNCAIndividualParameters()$parameter

	# Get ALL available covariates from data
	project_data <- lixoftConnectors::getData()
	all_covariates <- project_data$header[project_data$headerTypes %in% c("catcov","contcov", "occ")]

	# Filter to selected parameters if NCAparams is specified
	if (!is.null(NCAparams)) {
		# Keep id, all covariates, and selected parameters only
		cols_to_keep <- c("id", all_covariates, NCAparams)
		# Only keep columns that exist in the data
		cols_to_keep <- intersect(cols_to_keep, names(indivNCA))
		indivNCA <- indivNCA %>% dplyr::select(dplyr::all_of(cols_to_keep))
	}

	# Apply rounding to parameter columns (BEFORE renaming)
	indivNCA <- .round_num(
		to_round        = indivNCA,
		digits          = roundDigits,
		rounding_method = roundMethod,
		exclude         = c("id", all_covariates))

	# Identify parameter columns (exclude id and all covariates present in data)
	param_cols <- setdiff(names(indivNCA), c("id", all_covariates))

	# Create temporary data frame with parameter names
	temp_df <- data.frame(parameter = param_cols, stringsAsFactors = FALSE)

	# Apply units/aliases formatting
	temp_df_formatted <- .addUnitsAliases(temp_df, add_units = units, add_aliases = aliases)

	# Create mapping: original name -> formatted name
	name_mapping <- setNames(temp_df_formatted$parameter, param_cols)

	# Rename parameter columns in indivNCA
	names(indivNCA) <- ifelse(names(indivNCA) %in% param_cols,
														name_mapping[names(indivNCA)],
														names(indivNCA))

	if (!is.null(covariates)) {
		# Get remaining columns (parameters and non-selected covariates)
		remaining_cols <- setdiff(names(indivNCA), c("id", covariates))

		# Reorder: id, then covariates in specified order, then rest
		indivNCA <- indivNCA %>%
			dplyr::select("id", dplyr::all_of(covariates), dplyr::all_of(remaining_cols)) %>%
			dplyr::arrange(!!!rlang::syms(covariates))
	}

	return(indivNCA)
}
#-------------------------------------------------------------------------------
#' NCA Ratios Individual Listing
#'
#' Creates a listing table of individual NCA ratio parameter values from a PKanalix project.
#'
#' @param projectFile Character string. Path to the PKanalix project file (.pkx).
#'   If NULL, uses the currently loaded project.
#' @param covariates Character vector. Covariates to include and sort by. If specified,
#'   the table is sorted by these covariates in the order provided.
#' @param ratioParams Character vector. Ratio parameters to include. If NULL (default),
#'   all available ratio parameters are included.
#' @param roundDigits Numeric. Number of digits for rounding (default: 3).
#' @param roundMethod Character. Rounding method: "monolix" (default), "signif", or "none".
#' @param aliases Logical. If TRUE (default), parameter names are replaced with their aliases.
#'
#' @return A data frame containing individual NCA ratio values with columns for ID,
#'   covariates (if specified), and ratio parameters.
#'
#' @details
#' The function validates that:
#' - NCA analysis has been run (required for ratios)
#' - NCA ratio module is configured
#' - Specified covariates and parameters exist in the project
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
#'   covariates = c("SEX", "DOSE"),
#'   ratioParams = c("AUC_ratio", "Cmax_ratio"),
#'   roundDigits = 2
#' )
#' }
listingNCARatios <- function(projectFile = NULL,
														 covariates  = NULL,
														 ratioParams = NULL,
														 roundDigits = 3,
														 roundMethod = c("monolix", "signif", "none"),
														 aliases     = TRUE){

	roundMethod <- match.arg(roundMethod)
	# Ensure the correct project is loaded
	.checkProjectFile(projectFile, "pkanalix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "pkanalix", resultType = "ncaRatio")
	# Validate set covariates
	.validateCovariateSelection(covariates, ratio = TRUE)
	# Validate set parameters
	.validateNCAParametersSelection(ratioParams, ratio = TRUE)

	indivRatios <- lixoftConnectors::getNCAIndividualRatios()

	# Get ALL available covariates from data
	project_data   <- lixoftConnectors::getData()
	all_covariates <- project_data$header[project_data$headerTypes %in% c("catcov","contcov", "occ")]
	#intersection between all covariates available in data and in ratios listing
	all_covariates <- intersect(all_covariates, names(indivRatios))

	# Filter to selected parameters if ratioParams is specified
	if (!is.null(ratioParams)) {
		# Keep id, all covariates, and selected parameters only
		cols_to_keep <- c("id", all_covariates, ratioParams)
		# Only keep columns that exist in the data
		cols_to_keep <- intersect(cols_to_keep, names(indivRatios))
		indivRatios <- indivRatios %>% dplyr::select(dplyr::all_of(cols_to_keep))
	}

	# Apply rounding to parameter columns (BEFORE renaming)
	indivRatios <- .round_num(
		to_round        = indivRatios,
		digits          = roundDigits,
		rounding_method = roundMethod,
		exclude         = c("id", all_covariates))

	# Identify parameter columns (exclude id and all covariates present in data)
	param_cols <- setdiff(names(indivRatios), c("id", all_covariates))

	# Create temporary data frame with parameter names
	temp_df <- data.frame(parameter = param_cols, stringsAsFactors = FALSE)

	# line 111 to 120 usefull in case aliases would be available for ratio parameters
	# Apply units/aliases formatting
	temp_df_formatted <- .addUnitsAliases(temp_df, add_units = FALSE, add_aliases = aliases)

	# Create mapping: original name -> formatted name
	name_mapping <- setNames(temp_df_formatted$parameter, param_cols)

	# Rename parameter columns in indivNCA
	names(indivRatios) <- ifelse(names(indivRatios) %in% param_cols,
															 name_mapping[names(indivRatios)],
															 names(indivRatios))

	if (!is.null(covariates)) {
		# Get remaining columns (parameters and non-selected covariates)
		remaining_cols <- setdiff(names(indivRatios), c("id", covariates))

		# Reorder: id, then covariates in specified order, then rest
		indivRatios <- indivRatios %>%
			dplyr::select("id", dplyr::all_of(covariates), dplyr::all_of(remaining_cols)) %>%
			dplyr::arrange(!!!rlang::syms(covariates))
	}

	return(indivRatios)
}
