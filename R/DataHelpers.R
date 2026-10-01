#' Creates a data frame summarizing the continuous covariates in the dataset with descriptive statistics.
#'
#' @param projectFile path to project file (Monolix: .mlxtran or PKanalix: .pkx) or NULL to use the currently loaded project
#' @param customAliases Named vector or 2-column dataframe mapping dataset covariate names to
#'   custom display labels. Example: `c(WEIGHT = "Weight", AGE = "Age")`.
#'   Covariates not in the mapping keep their original names.
#' @param customUnits Named vector or 2-column dataframe mapping dataset covariate names to
#'   custom unit labels. Example: `c(WEIGHT = "kg", HEIGHT = "cm")`.
#'   Units are appended in parentheses: "Weight (kg)".
#' @param splitCovariates vector of categorical covariates to stratify by, or NULL
#' @param metrics vector of metrics to display, or NULL to display all available metrics
#' @param useWideFormat Logical. If `TRUE` (default), returns a "wide" table format where
#'   statistics appear as columns and covariates appear as rows.
#'   If `FALSE`, returns a "long" table format where statistics appear as rows
#'   (useful for vertical reporting layouts).
#'   Note: `stratifyColumns = TRUE` is only compatible with `useWideFormat = FALSE`.
#' @param stratifyColumns Logical; if `TRUE`, creates a stratified table layout with
#'   separate columns for each covariate combination plus an "Overall" column. If `FALSE`
#'   (default), splitCovariates appear as separate columns on the left side of the table.
#'   Only applicable when `splitCovariates` is specified.
#' @param covariateLabels Character vector of custom labels for splitCovariates, must be the
#'   same length as `splitCovariates`. If `NULL` (default), the original covariate names are
#'   used. These labels appear in column headers when `stratifyColumns = TRUE`.
#' @param filterCovariates Named list where names are categorical covariate names
#'   (type "categorical", "additionalstratificationcategorical" or "stratificationcategorical"
#'   in `getCovariateInformation()`) and values are vectors of allowed values.
#'   Example: `list(SEX = "Male", AVISIT = c("Week 1","Week 2"))`. If `NULL`, no
#'   covariate filtering is applied.
#' @param filterIds Character or numeric vector of subject IDs to include. If `NULL`,
#'   no ID filtering is applied.
#' @param roundDigits number of digits to display
#' @param roundMethod "monolixSuiteRounding" to use MonolixSuite rounding where digits specifies
#'   the number of significant digits of the fractional part (integer part untouched),
#'   "significantFigures" for digits to specify the number of significant figures using the
#'   standard/textbook definition (zeroes non-significant digits before the decimal for large
#'   numbers), or "significantFiguresPreserved" for the same but never zeroing digits before the
#'   decimal - once the integer part already has as many digits as requested, the value is simply
#'   rounded to the nearest whole number
#' @param prettyLabels Logical; if `TRUE` (default), converts column names to
#'   human-readable labels (e.g., "CV" becomes "CV (%)", "geoMean" becomes "Geometric Mean").
#' @param mock Logical; if `TRUE`, masks the table for documentation / example
#'   outputs: data values become placeholders (e.g. `"xx.xx"`) and real category
#'   codes of `splitCovariates` become generic labels (`"Category 1"`, ...).
#'   Covariate names and statistic labels are kept. Default `FALSE`.
#'
#' @returns a data.frame
#' @seealso [getDescriptiveStatsMetrics()]
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # Overall summary of all continuous covariates
#' tableContinuousCovariateSummary()
#'
#' # Stratify by a categorical covariate
#' tableContinuousCovariateSummary(splitCovariates = "ATRTNCA")
#'
#' # Filter to a subset of visits and select a few metrics
#' tableContinuousCovariateSummary(
#'     splitCovariates  = "ATRTNCA",
#'     filterCovariates = list(AVISIT = c("CYCLE 1 DAY 8", "CYCLE 1 DAY 15")),
#'     metrics          = c("N", "mean", "SD", "median", "min", "max")
#' )
#'
#' # With custom aliases and units
#' tableContinuousCovariateSummary(
#'     customAliases = c(WEIGHT = "Weight", HEIGHT = "Height", AGE = "Age"),
#'     customUnits   = c(WEIGHT = "kg", HEIGHT = "cm", AGE = "years")
#' )
#' }
tableContinuousCovariateSummary <- function(projectFile      = NULL,
																						customAliases    = NULL,
																						customUnits      = NULL,
																						splitCovariates  = NULL,
																						metrics          = NULL,
																						useWideFormat  = TRUE,
																						stratifyColumns = FALSE,
																						covariateLabels = NULL,
																						filterCovariates = NULL,
																						filterIds        = NULL,
																						roundDigits      = 2,
																						roundMethod      = c("monolixSuiteRounding", "significantFigures", "significantFiguresPreserved", "none"),
																						prettyLabels     = TRUE,
																						mock             = FALSE)
{

	roundMethod <- match.arg(roundMethod)

	.checkProjectFile(projectFile, c("monolix", "pkanalix"))
	.ensureProjectLoaded(projectFile)

	aliases_df <- .normalizeAliasMapping(customAliases, "aliases")
	units_df   <- .normalizeAliasMapping(customUnits, "units")

	# Validate stratifyColumns and covariateLabels (using same logic as PKanalixHelpers).
	# allowed_types restricts splitCovariates to categorical-like types from
	# getCovariateInformation() (incl. PKanalix-derived stratification covariates).
	cat_types        <- c("categorical",
												"additionalstratificationcategorical",
												"stratificationcategorical")
	stratifyColumns <- .validateStratificationArgs(splitCovariates, stratifyColumns, allowed_types = cat_types)
	covariateLabels <- .validateCovariateLabels(splitCovariates, covariateLabels)

	# Validate covariate and id filters (categorical-like types as defined above)
	.validateFilters(filterCovariates, filterIds, resultType = "covariateSummary")

	# Validate: stratifyColumns only works with long format
	if (stratifyColumns && useWideFormat) {
		message("stratifyColumns = TRUE requires long format, but useWideFormat = TRUE was set; ignoring stratifyColumns. To stratify, set useWideFormat = FALSE.")
		stratifyColumns <- FALSE
	}

	cov_info <- lixoftConnectors::getCovariateInformation()

	# Apply covariate and ID filters to the covariate dataframe before pivoting
	cov_info$covariate <- .applyFilters(cov_info$covariate, filterCovariates, filterIds, id_col = "id")

	# Prepare data: pivot continuous covariates to long format.
	# Include PKanalix-derived continuous types so the table also works on projects
	# where only stratification-continuous covariates are available.
	cont_types <- c("continuous", "additionalstratificationcontinuous", "stratificationcontinuous")
	cont_covs  <- cov_info$name[cov_info$type %in% cont_types]
	if (length(cont_covs) == 0) {
		stop("No continuous covariates found in the project (type 'continuous', 'additionalstratificationcontinuous' or 'stratificationcontinuous').",
				 call. = FALSE)
	}
	has_id    <- "id" %in% names(cov_info$covariate)

	cols_to_select <- c(cont_covs, splitCovariates)
	if (has_id) cols_to_select <- c("id", cols_to_select)

	cov_data <- cov_info$covariate %>%
		dplyr::select(dplyr::all_of(cols_to_select)) %>%
		tidyr::pivot_longer(
			cols = dplyr::all_of(cont_covs),
			names_to = "Covariate",
			values_to = "Value")

	# ==========================================================================
	# CALCULATE DESCRIPTIVE STATISTICS

	if (stratifyColumns && !is.null(splitCovariates)) {
		# ----------------------------------------------------------------------
		# STRATIFIED CASE: Calculate stats for each covariate combination
		group_by_vars <- c("Covariate", splitCovariates)

		summary_stats <- .calculateDescriptiveStats(
			data      = cov_data,
			value_col = "Value",
			id_col    = if (has_id) "id" else NULL,
			group_by  = group_by_vars)

		# Apply rounding
		summary_stats <- .round_num(
			to_round        = summary_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = c("Covariate", splitCovariates))

		# Add combined metrics (MeanSD, GeomCV, GeomSD)
		summary_stats <- .combinedStats(summary_stats)

		# Also calculate overall stats (no covariate grouping)
		overall_stats <- .calculateDescriptiveStats(
			data      = cov_data,
			value_col = "Value",
			id_col    = if (has_id) "id" else NULL,
			group_by  = "Covariate")

		# Apply rounding to overall_stats
		overall_stats <- .round_num(
			to_round        = overall_stats,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = "Covariate")

		# Add combined metrics
		overall_stats <- .combinedStats(overall_stats)

		# Validate and select metrics
		descriptor_cols  <- c("Covariate", splitCovariates)
		selected_metrics <- .validateAndSelectMetrics(
			available_cols = names(summary_stats),
			user_metrics   = metrics,
			descriptors    = descriptor_cols)

		# Use internal helper to build stratified table
		cov_summary <- .buildStratifiedTable(
			summary_stats    = summary_stats,
			overall_stats    = overall_stats,
			splitCovariates  = splitCovariates,
			selected_metrics = selected_metrics,
			param_col        = "Covariate",
			covariateLabels = covariateLabels)

		# Apply custom aliases and units using covariate names
		if (!is.null(aliases_df) || !is.null(units_df)) {
			cov_summary <- .applyCustomAliasesUnits(cov_summary,
																							 param_col  = "Covariate",
																							 aliases_df = aliases_df,
																							 units_df   = units_df)
		}

	} else {
		# ----------------------------------------------------------------------
		# NON-STRATIFIED CASE
		group_by_vars <- c("Covariate", splitCovariates)

		cov_summary <- .calculateDescriptiveStats(
			data      = cov_data,
			value_col = "Value",
			id_col    = if (has_id) "id" else NULL,
			group_by  = group_by_vars)

		# Apply rounding
		cov_summary <- .round_num(
			to_round        = cov_summary,
			digits          = roundDigits,
			rounding_method = roundMethod,
			exclude         = c("Covariate", splitCovariates))

		# Add combined metrics
		cov_summary <- .combinedStats(cov_summary)

		# Select metrics
		descriptor_cols <- c("Covariate", splitCovariates)

		selected_metrics <- .validateAndSelectMetrics(
			available_cols = names(cov_summary),
			user_metrics   = metrics,
			descriptors    = descriptor_cols)

		cov_summary <- cov_summary[, c(descriptor_cols, selected_metrics), drop = FALSE]

		# Apply custom aliases and units using covariate names
		if (!is.null(aliases_df) || !is.null(units_df)) {
			cov_summary <- .applyCustomAliasesUnits(cov_summary,
																							 param_col  = "Covariate",
																							 aliases_df = aliases_df,
																							 units_df   = units_df)
		}

		# ==========================================================================
		# FORMAT: WIDE vs LONG

		if (!useWideFormat) {
			# LONG FORMAT: transpose so metrics appear as rows
			# Use helper function for vertical pivot (similar to tableNCAParameters)
			cov_summary <- .pivotToVertical(
				summary_stats = cov_summary,
				param_col     = "Covariate",
				splitCovariates = splitCovariates)
		}
	}

	# ==========================================================================
	# APPLY PRETTY LABELS

	if (prettyLabels) {
		cov_summary <- .prettyLabels(cov_summary)

		# Apply pretty labels to the content of the 'Statistic' column if it exists
		if ("Statistic" %in% names(cov_summary)) {
			curr_metrics    <- unique(cov_summary$Statistic)
			dummy_df        <- as.data.frame(matrix(NA, nrow = 1, ncol = length(curr_metrics)))
			names(dummy_df) <- curr_metrics
			dummy_df        <- .prettyLabels(dummy_df)
			metric_map      <- setNames(names(dummy_df), curr_metrics)

			cov_summary$Statistic <- metric_map[cov_summary$Statistic]
		}
	}

	# ==========================================================================
	# MOCKING

	if (isTRUE(mock)) {
		modality_cols <- list()
		if (!stratifyColumns && !is.null(splitCovariates)) {
			for (sc in splitCovariates) modality_cols[[sc]] <- NA_character_
		}

		cov_summary <- .mockOutputData(
			df           = cov_summary,
			digits       = roundDigits,
			excludeCols  = c("Covariate", "Statistic"),
			modalityCols = if (length(modality_cols)) modality_cols else NULL,
			naFillAxis   = if (stratifyColumns || !useWideFormat) "row" else "column")
	}

	# ==========================================================================
	# RETURN

	attr(cov_summary, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(cov_summary, "r_source")       <- .getRScriptPath()
	return(as.data.frame(cov_summary))
}

################################################################################
#' Available descriptive-statistics metrics for summary tables
#'
#' Returns the names of every metric that the descriptive-statistics summary
#' tables can display, i.e. the values accepted by the `metrics` argument of
#' summary tables such as [tableContinuousCovariateSummary()],
#' [tableNCAParameters()] and [tableNCARatios()].
#'
#' The names and their order mirror exactly how the metrics are computed: first
#' the per-group statistics from `.calculateDescriptiveStats()`
#' (N, Nobs, min ... harmMean), then the combined display metrics from
#' `.combinedStats()` (MeanSD, MeanCV, GeomCV, GeomSD).
#'
#' @returns Character vector of available metric names, in calculation order.
#' @seealso [tableContinuousCovariateSummary()]
#' @export
#'
#' @examples
#' getDescriptiveStatsMetrics()
getDescriptiveStatsMetrics <- function() {
	return(c(
		# .calculateDescriptiveStats() — per-group statistics (in calculation order)
		"N", "Nobs", "min", "P05", "Q1", "mean", "SD", "SE", "CV", "median", "Q3", "P95", "max", "geoMean", "geoSD", "geoCV", "harmMean",
		# .combinedStats() — combined display metrics (in calculation order)
		"MeanSD", "MeanCV", "GeomCV", "GeomSD"))
}
################################################################################
#' Creates a table summarizing the frequency of categorical covariates in the dataset.
#'
#' @param projectFile path to project file (Monolix: .mlxtran or PKanalix: .pkx) or
#'   NULL to use the currently loaded project.
#' @param covDisplayLabels (optional) data.frame with columns covariate and label for
#'   the dataset covariate name.
#' @param categoryDisplayLabels (optional) data.frame with columns covariate, category,
#'   and label for the dataset categorical covariate categories.
#' @param splitCovariates Character vector of categorical covariates to stratify by.
#'   Allowed types from `getCovariateInformation()`: "categorical",
#'   "additionalstratificationcategorical", "stratificationcategorical". Default NULL.
#' @param stratifyColumns Logical; if `TRUE`, creates a stratified table layout with
#'   separate columns for each covariate combination plus an "Overall" column. If
#'   `FALSE` (default), `splitCovariates` appear as separate columns on the left side
#'   of the table. Only applicable when `splitCovariates` is specified.
#' @param covariateLabels Character vector of custom labels for `splitCovariates`,
#'   must be the same length as `splitCovariates`. If `NULL` (default), the original
#'   covariate names are used. These labels appear in column headers when
#'   `stratifyColumns = TRUE`.
#' @param filterCovariates Named list where names are categorical covariate names
#'   (type "categorical", "additionalstratificationcategorical" or
#'   "stratificationcategorical" in `getCovariateInformation()`) and values are
#'   vectors of allowed values. Example:
#'   `list(SEX = "Male", AVISIT = c("Week 1","Week 2"))`. If `NULL`, no covariate
#'   filtering is applied.
#' @param filterIds Character or numeric vector of subject IDs to include. If `NULL`,
#'   no ID filtering is applied.
#' @param roundDigits number of digits to display.
#' @param roundMethod "monolixSuiteRounding" to use MonolixSuite rounding where digits specifies
#'   the number of significant digits of the fractional part (integer part untouched),
#'   "significantFigures" for digits to specify the number of significant figures using the
#'   standard/textbook definition (zeroes non-significant digits before the decimal for large
#'   numbers), or "significantFiguresPreserved" for the same but never zeroing digits before the
#'   decimal - once the integer part already has as many digits as requested, the value is simply
#'   rounded to the nearest whole number.
#' @param mock Logical; if `TRUE`, masks the table for documentation / example
#'   outputs: data values become placeholders (e.g. `"xx.xx"`) and real category
#'   codes become generic labels (`"Category 1"`, ...). Covariate names are kept.
#'   Default `FALSE`.
#'
#' @returns a data.frame
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # Overall frequency of all categorical covariates
#' tableCategoricalCovariateSummary()
#'
#' # Stratify by one categorical covariate (long format, splitter as left column)
#' tableCategoricalCovariateSummary(splitCovariates = "ATRTNCA")
#'
#' # Stratified-columns layout: one column per stratum + Overall
#' tableCategoricalCovariateSummary(
#'     splitCovariates  = "ATRTNCA",
#'     stratifyColumns = TRUE,
#'     covariateLabels = "Treatment"
#' )
#' }
tableCategoricalCovariateSummary <- function(projectFile           = NULL,
																						 covDisplayLabels      = NULL,
																						 categoryDisplayLabels = NULL,
																						 splitCovariates       = NULL,
																						 stratifyColumns      = FALSE,
																						 covariateLabels      = NULL,
																						 filterCovariates      = NULL,
																						 filterIds             = NULL,
																						 roundDigits           = 2,
																						 roundMethod           = c("monolixSuiteRounding", "significantFigures", "significantFiguresPreserved", "none"),
																						 mock                  = FALSE) {


	roundMethod <- match.arg(roundMethod)

	# ==========================================================================
	# VALIDATION & SETUP

	.checkProjectFile(projectFile, c("monolix", "pkanalix"))
	.ensureProjectLoaded(projectFile)

	# covDisplayLabels validation
	if (!is.null(covDisplayLabels) && !is.data.frame(covDisplayLabels)) {
		stop("The argument 'covDisplayLabels' must be a dataframe.", call. = FALSE)
	}
	if (!is.null(covDisplayLabels) &&
			!identical(names(covDisplayLabels), c("covariate", "label"))) {
		stop(paste0(
			"The argument 'covDisplayLabels' must contain the headers 'covariate' and 'label.' Headers found are: ",
			paste(names(covDisplayLabels), collapse = ", ")),
			call. = FALSE)
	}

	# categoryDisplayLabels validation
	if (!is.null(categoryDisplayLabels) && !is.data.frame(categoryDisplayLabels)) {
		stop("The argument 'categoryDisplayLabels' must be a dataframe.", call. = FALSE)
	}
	if (!is.null(categoryDisplayLabels) &&
			!identical(names(categoryDisplayLabels), c("covariate", "category", "label"))) {
		stop(paste0(
			"The argument 'categoryDisplayLabels' must contain the headers 'covariate,' 'category,' and 'label.' Headers found are: ",
			paste(names(categoryDisplayLabels), collapse = ", ")),
			call. = FALSE)
	}

	# Categorical types accepted for splitting and filtering (incl. PKanalix-derived)
	cat_types <- c("categorical",
								 "additionalstratificationcategorical",
								 "stratificationcategorical")

	# Drop accidental duplicate split covariates (e.g. the same occasion passed
	# twice). Duplicates would create duplicate columns on select()/pivot and error.
	if (!is.null(splitCovariates) && anyDuplicated(splitCovariates)) {
		dups <- unique(splitCovariates[duplicated(splitCovariates)])
		warning("Duplicate splitCovariates ignored: ", paste(dups, collapse = ", "), call. = FALSE)
		splitCovariates <- unique(splitCovariates)
	}

	# Allow_occasions = TRUE so occasion index columns (e.g. AVISIT_OCC) can be passed to splitCovariates to resolve occasion-varying covariates.
	stratifyColumns <- .validateStratificationArgs(splitCovariates, stratifyColumns, allowed_types = cat_types, allow_occasions = TRUE)
	final_labels     <- .validateCovariateLabels(splitCovariates, covariateLabels)
	.validateFilters(filterCovariates, filterIds, resultType = "covariateSummary")

	# ==========================================================================
	# GET DATA AND APPLY FILTERS

	cov_info <- lixoftConnectors::getCovariateInformation()
	cov_info$covariate <- .applyFilters(cov_info$covariate, filterCovariates, filterIds, id_col = "id")

	cat_covs <- cov_info$name[cov_info$type %in% cat_types]
	if (length(cat_covs) == 0) {
		stop("No categorical covariates found in the project (type 'categorical', 'additionalstratificationcategorical' or 'stratificationcategorical').",
				 call. = FALSE)
	}

	# Occasion index columns (in $covariate but not a covariate name)
	occ_cols <- setdiff(names(cov_info$covariate), c("id", cov_info$name))

	if (!is.null(splitCovariates) &&
			length(setdiff(cat_covs, splitCovariates)) == 0) {
		stop("All categorical covariates were used as stratifiers; none remain to summarize.",
				 call. = FALSE)
	}

	df_cov            <- cov_info$covariate
	cols_to_char      <- union(cat_covs, intersect(splitCovariates, occ_cols))
	df_cov            <- df_cov %>% dplyr::mutate(dplyr::across(dplyr::all_of(cols_to_char), as.character))
	vars_to_summarize <- setdiff(cat_covs, splitCovariates)

	# A summarized covariate that is not constant within (id + splitCovariates) will mix occasions and inflate its frequencies.
	# Tell user which occasion variable(s) to add to splitCovariates to fix it.
	occ_dep <- .getCovariateOccasionDependency(cov_info, covariates = vars_to_summarize, group_by = splitCovariates)
	occ_dep <- occ_dep[vapply(occ_dep, length, integer(1)) > 0]
	if (length(occ_dep) > 0) {
		lines <- vapply(names(occ_dep), function(cv) {
			sprintf("  - '%s' varies along occasion %s", cv,
							paste(sprintf("'%s'", occ_dep[[cv]]), collapse = " or "))
		}, character(1))
		warning(paste0(
			"The following covariate(s) are not constant within subject for the current ",
			"stratification, so their frequencies mix occasions and may be inflated:\n",
			paste(lines, collapse = "\n"),
			"\nAdd the named occasion variable(s) to `splitCovariates` to obtain a correct table."),
			call. = FALSE)
	}
	# ---------------------------------------------------------------------------

	# ==========================================================================
	# CALCULATE STATISTICS

	if (stratifyColumns) {
		# --- STRATIFIED (WIDE) -------------------------------------------------
		# .validateStratificationArgs guarantees splitCovariates is non-NULL here.
		df_long <- df_cov %>%
			dplyr::select(id, dplyr::all_of(c(splitCovariates, vars_to_summarize))) %>%
			tidyr::pivot_longer(cols = dplyr::all_of(vars_to_summarize),
													names_to = "variable", values_to = "value")

		# Overall summary: ignores splitCovariates — .calculateCategoricalFrequencies groups only by `variable`
		overall <- df_long %>%
			.calculateCategoricalFrequencies() %>%
			.format_frequency_table(roundDigits, roundMethod, exclude_cols = c("variable", "Modalities", "N")) %>%
			dplyr::select(-dplyr::any_of("N")) %>%
			dplyr::rename(Overall = `N (%)`)

		stratified_df <- df_long %>%
			dplyr::rowwise() %>%
			dplyr::mutate(header = {
				pieces <- purrr::map2_chr(final_labels, splitCovariates,
																	~ paste0(.x, "=",
																					 as.character(dplyr::pick(dplyr::all_of(.y)))))
				paste(pieces, collapse = "\n")
			}) %>%
			dplyr::ungroup()

		stratified_result <- stratified_df %>%
			dplyr::group_by(header) %>%
			dplyr::do(stats = .calc_format_stratum(., roundDigits, roundMethod,
																						 exclude_cols = c("variable", "Modalities", "N"))) %>%
			tidyr::unnest(stats) %>%
			dplyr::select(-dplyr::any_of("N")) %>%   # drop per-stratum N so (variable, Modalities)
			tidyr::pivot_wider(names_from = header, values_from = `N (%)`)  # rows collapse on widening

		summary_stats <- dplyr::left_join(stratified_result, overall,
																			by = c("variable", "Modalities"))
		names(summary_stats)[names(summary_stats) == "variable"] <- "Subject characteristics"

	} else {
		# --- LONG FORMAT --------------------------------------------------------
		df_long <- df_cov %>%
			dplyr::select(id, dplyr::all_of(c(vars_to_summarize, splitCovariates))) %>%
			tidyr::pivot_longer(cols = dplyr::all_of(vars_to_summarize),
													names_to = "variable", values_to = "value")

		result_raw <- df_long %>%
			dplyr::group_by(!!!rlang::syms(splitCovariates), variable, value) %>%
			dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
			dplyr::left_join(
				df_long %>%
					dplyr::filter(!is.na(value)) %>%
					dplyr::group_by(!!!rlang::syms(splitCovariates), variable) %>%
					dplyr::summarise(N = dplyr::n_distinct(id), .groups = "drop"),
				by = c(splitCovariates, "variable")
			) %>%
			dplyr::mutate(Percentage = (Count / N) * 100) %>%
			dplyr::select(!!!rlang::syms(splitCovariates), variable,
										Modalities = value, Count, Percentage, N)

		summary_stats <- result_raw %>%
			.format_frequency_table(roundDigits, roundMethod,
															c(splitCovariates, "variable", "Modalities", "Count", "N")) %>%
			dplyr::select(!!!rlang::syms(splitCovariates),
										`Subject characteristics` = variable, Modalities,
										`Frequency\nN (%)` = `N (%)`) %>%
			dplyr::arrange(dplyr::across(dplyr::any_of(splitCovariates)),
										 `Subject characteristics`, Modalities)

		# Apply covariateLabels to split columns (positional mapping)
		if (!is.null(splitCovariates)) {
			col_idx <- match(splitCovariates, names(summary_stats))
			ok      <- !is.na(col_idx)
			names(summary_stats)[col_idx[ok]] <- final_labels[ok]
		}
	}

	# ==========================================================================
	# APPLY DISPLAY LABEL RENAMES
	# Order matters: categoryDisplayLabels MUST run before covDisplayLabels because
	# its lookup keys on the original covariate names still in "Subject characteristics".

	if (!is.null(categoryDisplayLabels)) {
		key_lookup <- paste0(categoryDisplayLabels$covariate, "||",
												 categoryDisplayLabels$category)
		key_row    <- paste0(as.character(summary_stats[["Subject characteristics"]]), "||",
												 as.character(summary_stats$Modalities))
		match_idx  <- match(key_row, key_lookup)
		summary_stats$Modalities[!is.na(match_idx)] <-
			categoryDisplayLabels$label[stats::na.omit(match_idx)]
	}

	if (!is.null(covDisplayLabels)) {
		label_idx <- match(summary_stats[["Subject characteristics"]],
											 covDisplayLabels$covariate)
		summary_stats[["Subject characteristics"]][!is.na(label_idx)] <-
			covDisplayLabels$label[stats::na.omit(label_idx)]
	}

	# ==========================================================================
	# MOCKING

	if (isTRUE(mock)) {
		modality_cols <- list(Modalities = "Subject characteristics")
		if (!stratifyColumns && !is.null(splitCovariates)) {
			for (lbl in final_labels[nzchar(final_labels)]) {
				modality_cols[[lbl]] <- NA_character_
			}
		}

		summary_stats <- .mockOutputData(
			df           = summary_stats,
			digits       = roundDigits,
			excludeCols  = "Subject characteristics",
			modalityCols = modality_cols)
	}

	# ==========================================================================
	# RETURN

	attr(summary_stats, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(summary_stats, "r_source")       <- .getRScriptPath()
	return(as.data.frame(summary_stats))

}

################################################################################
#' tableExclusions()
#'
#' Summarises the observations and subjects excluded from the analysis (as
#' identified by \code{\link{listingExclusions}}), optionally stratified and/or
#' filtered.
#'
#' @param projectFile Optional path to the project to load.
#' @param splitCovariates Character vector of columns to stratify the summary by.
#'   Unlike other tables this is not restricted to covariates: any column present
#'   in the dataset may be used (including columns tagged "ignore" in the GUI,
#'   such as Exclusion_reason); only non-existent names are rejected. A warning is
#'   emitted if a chosen column has too many modalities per subject (see
#'   \code{maxSubjectRatio}). If NULL, an overall summary is provided.
#' @param filterCovariates Named list where names are existing dataset columns
#'   (any column, not only covariates) and values are vectors of allowed values,
#'   e.g. \code{list(VISIT = c("DAY 1", "DAY 7"))}. If NULL, no covariate filtering.
#' @param filterIds Character or numeric vector of subject IDs to include. If
#'   NULL, no ID filtering.
#' @param useWideFormat Logical flag indicating table layout. If TRUE
#'   (default), metrics are returned as columns (wide). If FALSE, metrics are
#'   pivoted into rows (long, Statistic-Value format).
#' @param maxSubjectRatio Ratio (n_modalities / n_subjects) above which a
#'   chosen grouping variable triggers a "might not be a suitable grouping
#'   variable" warning. Default 0.5.
#' @param mock Logical; if `TRUE`, masks the table for documentation / example
#'   outputs: the count/summary values become placeholders (e.g. `"xx.xx"`) and
#'   `splitCovariates` grouping values become generic labels (`"Category 1"`, ...).
#'   Default `FALSE`.
#'
#' @return A data frame summarizing excluded observations and subjects.
#'
#' @examples
#' \dontrun{
#' # Overall summary of exclusions (no stratification)
#' tableExclusions()
#'
#' # Stratified summary by visit
#' tableExclusions(splitCovariates = "VISIT")
#'
#' # Stratified by visit & food, filtered to two visits, vertical format
#' tableExclusions(splitCovariates = c("VISIT", "FOOD"),
#'                 filterCovariates = list(VISIT = c("DAY 1", "DAY 7")),
#'                 useWideFormat = FALSE)
#'
#' # Restrict to specific subjects
#' tableExclusions(splitCovariates = "Exclusion_reason",
#'                 filterIds = c(1, 2, 17))
#' }
#'
#' @export
tableExclusions <- function(projectFile       = NULL,
														splitCovariates   = NULL,
														filterCovariates  = NULL,
														filterIds         = NULL,
														useWideFormat   = TRUE,
														maxSubjectRatio = 0.5,
														mock            = FALSE) {

	.checkProjectFile(projectFile, c("pkanalix", "monolix"))
	.ensureProjectLoaded(projectFile)

	listing <- listingExclusions(projectFile = projectFile)
	# No exclusions: listingExclusions() already messaged; nothing to summarise.
	if (is.null(listing)) {
		return(invisible(NULL))
	}
	dataUsed <- lixoftConnectors::getInterpretedData()

	hd   <- lixoftConnectors::getData()
	cols <- hd$header
	typ  <- hd$headerTypes
	hdr  <- tibble::tibble(col = cols, type = typ)
	obs  <- hdr$col[hdr$type == "observation"]
	id   <- hdr$col[hdr$type == "id"]

	# Any column present in listingExclusions may be used as a grouping / filter key
	# Only variables that that do not exist are rejected (typo protection). .groupableExclusionColumns()
	available <- names(listing)
	suggested <- .groupableExclusionColumns(hdr, listing = listing) # suggest the likely candidates in error messages.

	# Validate requested splitCovariates
	if (!is.null(splitCovariates)) {
		bad <- setdiff(splitCovariates, available)
		if (length(bad)) {
			stop(
				"Grouping column(s) not found in the dataset: ", paste(bad, collapse = ", "),
				"\nSuggested grouping columns: ", paste(suggested, collapse = ", "),
				"\n(any existing column may be used: ", paste(available, collapse = ", "), ")"
			)
		}
	} else {
		splitCovariates <- character(0)
	}

	# Warn if a chosen grouping variable has too many modalities per subject.
	.warnUnsuitableGrouping(dataUsed, splitCovariates, id_col = id,
													maxSubjectRatio = maxSubjectRatio)

	# Validate requested filterCovariates keys
	if (!is.null(filterCovariates)) {
		if (!is.list(filterCovariates) || is.null(names(filterCovariates)) || any(names(filterCovariates) == "")) {
			stop("filterCovariates must be a named list. Example: list(VISIT = c('DAY 1', 'DAY 7'))",
					 call. = FALSE)
		}
		bad_f <- setdiff(names(filterCovariates), available)
		if (length(bad_f)) {
			stop(
				"Filter column(s) not found in the dataset: ", paste(bad_f, collapse = ", "),
				"\n(any existing column may be used: ", paste(available, collapse = ", "), ")"
			)
		}
	}

	# Validate requested filterIds
	if (!is.null(filterIds)) {
		if (!is.vector(filterIds) || is.list(filterIds)) {
			stop("filterIds must be a character or numeric vector of subject IDs.", call. = FALSE)
		}
		if (length(filterIds) == 0) {
			stop("filterIds cannot be an empty vector.", call. = FALSE)
		}
		unknown_ids <- setdiff(as.character(filterIds), as.character(listing[[id]]))
		if (length(unknown_ids) == length(filterIds)) {
			warning("None of the filterIds IDs are present among the excluded subjects: ",
							paste(filterIds, collapse = ", "), call. = FALSE)
		}
	}

	# Apply filters
	listing <- .applyFilters(listing,
													 filterCovariates = filterCovariates,
													 filterIds        = filterIds,
													 id_col           = id)

	listing_clean <- listing %>%
		dplyr::filter(obs != "." & !is.na(obs))

	subject_group_vars <- c(id, splitCovariates)

	per_subject <- listing_clean %>%
		dplyr::group_by(dplyr::across(dplyr::all_of(subject_group_vars))) %>%
		dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")

	# Create summary table
	if (length(splitCovariates) > 0) {
		summary_tbl <- as.data.frame(per_subject) %>%
			dplyr::group_by(dplyr::across(dplyr::all_of(splitCovariates))) %>%
			dplyr::summarise(
				`Nb. of Subjects`          = dplyr::n_distinct(.data[[id]]),
				`Total Nb. of Obs.`        = sum(n_obs),
				`Mean Nb. of Obs./Subject` = round(mean(n_obs), 2),
				`Min. Nb. of Obs./Subject` = min(n_obs),
				`Max. Nb. of Obs./Subject` = max(n_obs),
				.groups = "drop") %>%
			dplyr::relocate(dplyr::all_of(splitCovariates), .before = 1) %>%
			dplyr::arrange(dplyr::across(dplyr::all_of(splitCovariates)))
	} else {
		# Overall summary (no grouping)
		summary_tbl <- as.data.frame(per_subject) %>%
			dplyr::summarise(
				`Nb. of Subjects`          = dplyr::n_distinct(.data[[id]]),
				`Total Nb. of Obs.`        = sum(n_obs),
				`Mean Nb. of Obs./Subject` = round(mean(n_obs), 2),
				`Min. Nb. of Obs./Subject` = min(n_obs),
				`Max. Nb. of Obs./Subject` = max(n_obs)) %>%
			dplyr::arrange(dplyr::across(dplyr::all_of(splitCovariates)))
	}

	if (useWideFormat == FALSE) {
		stat_cols <- setdiff(names(summary_tbl), splitCovariates)

		summary_tbl <- summary_tbl %>%
			tidyr::pivot_longer(
				cols = dplyr::all_of(stat_cols),
				names_to = "Statistic",
				values_to = "Value"
			)
	}

	# ==========================================================================
	# MOCKING

	if (isTRUE(mock)) {
		modality_cols <- list()
		for (sc in splitCovariates) modality_cols[[sc]] <- NA_character_

		summary_tbl <- .mockOutputData(
			df           = summary_tbl,
			digits       = 2,
			excludeCols  = "Statistic",
			modalityCols = if (length(modality_cols)) modality_cols else NULL,
			naFillAxis   = if (useWideFormat) "column" else "row")
	}

	attr(summary_tbl, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(summary_tbl, "r_source")       <- .getRScriptPath()
	return(as.data.frame(summary_tbl))

}
################################################################################
# Helpers for Data Listings
#-------------------------------------------------------------------------------
#' listingExclusions()
#'
#' Identifies and returns rows from the raw dataset that were excluded during data filtering
#' in the current NCA project (PKanalix) or PKPD project (Monolix). Compares raw data with
#' interpreted data to find discrepancies and returns the excluded observations as a data frame for review.
#'
#' @param projectFile Optional path to the project to load. If NULL, the currently
#'   loaded project is used.
#' @param mock Logical; if `TRUE`, masks the listing for documentation / example
#'   outputs, by column data type (from `getData()`): id -> `"id1"`, obsid ->
#'   `"obsid1"`, categorical covariates -> `"Category 1"`, occasions -> `"occ1"`,
#'   dates -> `"xx/xx/xxxx"`, and all numeric columns (times, observations,
#'   continuous covariates, ...) -> `"xx.xx"`. Missing markers (`"."`) are kept.
#'   Default `FALSE`.
#'
#' @return If exclusions are found, returns a data frame containing the excluded rows from
#'         the raw dataset. If no exclusions are found, returns NULL and outputs a message.
#'
#' @export
#' @examples
#' # Example usage:
#' # excluded_data <- listingExclusions()
# df <- listingExclusions()
listingExclusions <- function(projectFile = NULL, mock = FALSE) {
    # Ensure the correct project is loaded if explicitly provided
    .checkProjectFile(projectFile, c("pkanalix", "monolix"))
    .ensureProjectLoaded(projectFile)

    dataFile <- NULL # Tracks the raw data file path to ensure consistency
    colNameDataRaw <- NULL

    # Determine which software is being used
    software <- lixoftConnectors::getLixoftConnectorsState()$software

    data <- lixoftConnectors::getData()

    if (nchar(data$dataFile) < 2) {
        dataFileProject <- lixoftConnectors::getFormatting()$dataFile
    } else {
        dataFileProject <- data$dataFile
    }
    if (is.null(dataFile)) {
        dataFile <- dataFileProject
        dataFileExt <- tools::file_ext(dataFile)

        # Read raw data, handling CSV/TXT and Excel (XLSX/XLS) formats
        if (tolower(dataFileExt) %in% c("csv", "txt")) {
            dataRaw <- utils::read.csv(dataFile, header = TRUE, sep = .getDelim(dataFile))
        } else if (tolower(dataFileExt) %in% c("xlsx", "xls")) {
            if (!requireNamespace("readxl", quietly = TRUE)) {
                stop("Reading Excel data files requires the 'readxl' package. Install it with install.packages('readxl').")
            }
            # as.data.frame() so downstream row/column indexing matches the read.csv path
            dataRaw <- as.data.frame(readxl::read_excel(dataFile), stringsAsFactors = FALSE)
        } else {
            stop("Only CSV, TXT, or Excel (XLSX/XLS) format is currently supported.")
        }
    } else {
        if (basename(dataFileProject) != basename(dataFile)) {
            stop("The basename of each data set file is not the same for each project.")
        }
    }

    # Append interpreted data from each project
    dataUsed <- lixoftConnectors::getInterpretedData()

    # Identify indices for relevant columns in the raw data
    iId      <- which(data$headerTypes == "id")
    iTime    <- which(data$headerTypes == "time")
    iOcc     <- which(data$headerTypes == "occ")
    iMdv     <- which(data$headerTypes == "mdv")
    iOccUsed <- iOcc
    colNameDataRaw <- names(dataRaw)

    # check if the occasion were created using the data formatting
    if (length(iOcc) > 0) {
        for (indexOcc in 1:length(iOcc)) {
            nameOcc <- data$header[iOcc[indexOcc]]
            if (!(nameOcc %in% colNameDataRaw)) { # The OCC column was created by data formatting
                iOcc[indexOcc]     <- which(colNameDataRaw == sub(x = nameOcc, pattern = "_OCC", ""))
                iOccUsed[indexOcc] <- which(names(dataUsed) == colNameDataRaw[iOcc[indexOcc]])
            }
            iOccUsed[indexOcc] <- which(names(dataUsed) == colNameDataRaw[iOcc[indexOcc]])
        }
    }
    nbOccLevel <- length(iOcc)

    iObs <- which(data$headerTypes == "observation")

    # ---------------------------------------------------------------------------
    # Identify censoring cols (created by  Data Formatting module when BLQ/ALQ tags in the obs col are assigned an LLOQ/ULOQ value).
    # When present, BLQ rows are kept in the analysis as cens obs & must not be reported as exclusions. The cens col lives in interpreted/formatted dataset.
    iCens   <- which(data$headerTypes %in% c("cens", "censored"))
    censCol <- NULL
    if (length(iCens) > 0) {
    	censName <- data$header[iCens[1]]
    	if (censName %in% names(dataUsed)) {
    		censCol <- censName
    	}
    }

    # Retrieve scaling factors based on software type
    if (software == "pkanalix") {
        gainTime <- lixoftConnectors::getDataSettings()$scalings$time
        gainConc <- lixoftConnectors::getDataSettings()$scalings$concentration
    } else if (software == "monolix") {
        # Monolix doesn't have scaling, so use 1 (no scaling)
        gainTime <- 1
        gainConc <- 1
    } else {
        stop(paste("Unsupported software:", software))
    }

    # Initialize a vector to store indices of filtered rows
    indexFiltered <- integer(0)

    # Loop through each row in the raw data to identify filtered rows
    for (indexRaw in seq_len(nrow(dataRaw))) {
        rawLine <- dataRaw[indexRaw, ]

        # First check if this row has MDV=1 (or equivalent ignored flag)
        isMdvFiltered <- FALSE
        if (!is.null(iMdv) && length(iMdv) > 0) {
            mdvValue <- suppressWarnings(as.numeric(rawLine[iMdv]))
            if (!is.na(mdvValue) && mdvValue == 1) {
                isMdvFiltered <- TRUE
            }
        }

        # Create logical vector to find matching rows in interpreted dataset.
        # Match on id + time; compare time with small tolerance instead of
        # exact equality: raw (Excel/CSV) and interpreted times can differ by
        # floating-point rounding (most visibly at fractional timepoints such as
        # 0.08333333h) which exact "==" would wrongly report as excluded.
        # gainTime applies the PKanalix time scaling (== 1 for Monolix).
        rawTime  <- as.numeric(rawLine[iTime] * gainTime)
        usedTime <- as.numeric(dataUsed[, iTime])
        bInter   <- (dataUsed[, iId] == as.character(rawLine[iId])) &
            (abs(usedTime - rawTime) <= pmax(abs(rawTime), 1) * 1e-6)

        # Check occurrence levels if they exist
        if (nbOccLevel > 0) {
            for (indexOcc in seq_len(nbOccLevel)) {
                bInter <- bInter & (dataUsed[, iOccUsed[indexOcc]] == as.character(rawLine[iOcc[indexOcc]]))
            }
        }

        indexInter <- which(bInter)

        # If this is an MDV=1 row and no match found in interpreted data, it's filtered
        if (isMdvFiltered) {
            indexFiltered <- c(indexFiltered, indexRaw)
            next # Skip to next row
        }

        # If no match is found in interpreted data, mark as filtered
        if (length(indexInter) == 0) {
            # No match in id#Occ#OCC between the raw data and the used data
            indexFiltered <- c(indexFiltered, indexRaw)
        } else {
            # Check the value of the observation with appropriate scaling
            obsValue <- suppressWarnings(as.numeric(rawLine[iObs]))
            obsValueRaw <- as.character(rawLine[iObs]) # Keep original string value

            # Find matching rows in interpreted data for this ID/Time/Occasion combination
            matchingRows <- dataUsed[indexInter, ]

            # Check if observation values match
            foundMatch <- FALSE
            for (idx in seq_len(nrow(matchingRows))) {
                interpValue <- as.character(matchingRows[idx, iObs])
                interpValueNum <- suppressWarnings(as.numeric(interpValue))

                if (!is.na(obsValue) && !is.na(interpValueNum)) {
                    # Both are numeric - compare with scaling and tolerance
                    if (software == "pkanalix") {
                        if (abs(interpValueNum - obsValue * gainConc) <= (obsValue * gainConc) * 1e-4) {
                            foundMatch <- TRUE
                            break
                        }
                    } else if (software == "monolix") {
                        if (abs(interpValueNum - obsValue) <= obsValue * 1e-4) {
                            foundMatch <- TRUE
                            break
                        }
                    }
                } else if (is.na(obsValue) && (interpValue == "" || interpValue == "." || is.na(interpValueNum))) {
                    # Both are non-numeric/missing
                    foundMatch <- TRUE
                    break
                } else if (obsValueRaw == interpValue) {
                    # Exact string match (handles BLQ, etc.)
                    foundMatch <- TRUE
                    break
                }
            }

            if (!foundMatch) {
            	# Before flagging as excluded, check if this is a BLQ/ALQ obs that DF kept as a cens value
            	# (raw tag -> numeric LLOQ/ULOQ + non-zero censoring flag).
            	if (!.isCensoredObservation(obsValueRaw, matchingRows, iObs, censCol)) {
            		# No match in observation value between the raw data and the used data
            		indexFiltered <- c(indexFiltered, indexRaw)
            	}
            }
        }
    }

    # Output filtered rows if any are found
    if (length(indexFiltered) > 0) {
        message(paste0(length(indexFiltered), " lines were filtered in ", software))
        excl <- dataRaw[indexFiltered, ]

        if (isTRUE(mock)) {
            col_types <- stats::setNames(data$headerTypes, data$header)
            excl <- .mockDataListing(excl, colTypes = col_types)
        }

        attr(excl, "project_source") <- lixoftConnectors::getProjectSettings()$directory
        attr(excl, "r_source")       <- .getRScriptPath()
        return(excl)
    } else {
        message(paste0("All the lines are in the ", software, " project, no table of exclusions is created."))
        return(NULL)
    }
}
#-------------------------------------------------------------------------------

#' listingDosingRegimen()
#'
#' Builds a dosing-regimen listing with one row per administration record in the
#' dataset. For each dose record the admin. amount & admin. time are listed,
#' together with any regimen-describing columns that are present in the dataset
#' (occs, adm id, ss flag, ii, nb of addl doses, infusion rate/duration).
#'
#' Categorical covariates can be shown and the table stratified/sorted via
#' `splitCovariates`, and rows can be restricted via `filterCovariates` /
#' `filterIds`, mirroring the other PKanalix listing functions. Stratification-only
#' and additional-stratification categorical covariates (PKanalix) are merged in
#' automatically when available.
#'
#' @param projectFile Character scalar. Path to a PKanalix (.pkx) or Monolix
#'   (.mlxtran) project file. If NULL, the currently loaded project is used.
#' @param splitCovariates Character vector of categorical covariate / occasion
#'   names to include as columns and sort by. Validated against the project.
#' @param filterCovariates Named list of categorical covariate filters, e.g.
#'   `list(SEX = "M", DOSE = c("100", "200"))`. Rows not matching are dropped.
#' @param filterIds Vector of subject ids to keep. If NULL, no id filtering.
#' @param mock Logical; if `TRUE`, masks the listing for documentation / example
#'   outputs: id -> `"id1"`, administration id -> `"adm1"`, occasions -> `"occ1"`,
#'   categorical / stratification covariates -> `"Category 1"`, and all numeric
#'   columns (time, amount, ss, ii, addl, rate, tinf) -> `"xx.xx"`. Default `FALSE`.
#'
#' @return A data frame with columns: id, split covariates / occasion(s),
#'   administration id (if present), time, amount, and any present regimen
#'   descriptors (ss, ii, addl, rate, tinf), plus merged stratification
#'   categorical covariates (if any).
#'
#' @examples
#' \dontrun{
#' # Plain listing
#' listingDosingRegimen(projectFile = "path/to/project.pkx")
#'
#' # Stratified by SEX and filtered to oral administrations (admid == 2)
#' listingDosingRegimen(
#'   splitCovariates  = "SEX",
#'   filterCovariates = list(ADM = "2"))
#' }
#'
#' @export
listingDosingRegimen <- function(projectFile      = NULL,
																 splitCovariates  = NULL,
																 filterCovariates = NULL,
																 filterIds        = NULL,
																 mock             = FALSE) {

	# Load project & validate arguments
	.checkProjectFile(projectFile, c("pkanalix", "monolix"))
	.ensureProjectLoaded(projectFile)

	.validateCovariateSelection(splitCovariates, ratio = FALSE)
	.validateFilters(filterCovariates, filterIds, resultType = "covariateSummary")

	# Detect header-typed columns (only used if present in dataset)
	project_data <- lixoftConnectors::getData()
	headers      <- project_data$header
	types        <- project_data$headerTypes

	col_of <- function(t) headers[types == t]

	id_col     <- col_of("id")
	time_col   <- col_of("time")
	amount_col <- col_of("amount")

	if (length(id_col) == 0 || length(time_col) == 0 || length(amount_col) == 0) {
		stop("Dataset must define id, time and amount columns to build a dosing regimen listing.",
				 call. = FALSE)
	}

	occ_cols   <- col_of("occ")     # may be several
	admid_col  <- col_of("admid")
	ss_col     <- col_of("ss")
	ii_col     <- col_of("ii")
	addl_col   <- col_of("addl")
	rate_col   <- col_of("rate")
	tinf_col   <- col_of("tinf")
	evid_col   <- col_of("evid")

	# Regimen-describing columns, kept in a stable display order when present
	descriptor_cols <- c(ss_col, ii_col, addl_col, rate_col, tinf_col)

	# ---------------------------------------------------------------------------
	# Build listing (one row per dose record)
	df <- lixoftConnectors::getInterpretedData()

	# Split covariates that live directly in the dataset (e.g. catcov columns).
	split_in_df  <- intersect(splitCovariates, colnames(df))
	filter_in_df <- intersect(names(filterCovariates), colnames(df))

	keep_cols <- c(id_col, occ_cols, admid_col, split_in_df, filter_in_df, time_col, amount_col,
								 descriptor_cols, evid_col)
	keep_cols <- intersect(keep_cols, colnames(df))

	listing <- df %>%
		dplyr::select(dplyr::all_of(keep_cols))

	# Dose-row selection: prefer EVID == 1 (dosing event) when available,
	# excluding reset events (3, 4); otherwise fall back to a non-empty amount.
	if (length(evid_col) == 1 && evid_col %in% colnames(listing)) {
		evid_num <- suppressWarnings(as.numeric(listing[[evid_col]]))
		listing  <- listing[which(evid_num == 1), , drop = FALSE]
		listing[[evid_col]] <- NULL
	} else {
		amt <- listing[[amount_col]]
		listing <- listing[!is.na(amt) & amt != "", , drop = FALSE]
	}

	# Standardize id column name (matches getNCAIndividualParameters()$parameters)
	listing <- listing %>% dplyr::rename(id = dplyr::all_of(id_col))

	# ---------------------------------------------------------------------------
	# Covariates: auto-append occasions, merge stratification-only / added covs
	user_split <- splitCovariates  # user-specified covs as primary sort keys
	if (length(occ_cols) > 0) {
		splitCovariates <- unique(c(splitCovariates, occ_cols))
	}

	# Merge stratification categorical + additional stratification covs.
	cov_info <- tryCatch(lixoftConnectors::getCovariateInformation(), error = function(e) NULL)
	strat_cov_names <- character(0)
	if (!is.null(cov_info) && !is.null(cov_info$name)) {
		strat_cov_names <- cov_info$name[cov_info$type %in% c("stratificationcategorical", "additionalstratificationcategorical")]
	}
	if (length(strat_cov_names) > 0 && !is.null(cov_info$covariate)) {
		join_keys    <- c("id", intersect(occ_cols, names(cov_info$covariate)))
		join_keys    <- intersect(join_keys, names(listing))
		strat_cov_df <- unique(cov_info$covariate[, c(join_keys, strat_cov_names), drop = FALSE])
		listing      <- merge(listing, strat_cov_df, by = join_keys, all.x = TRUE, sort = FALSE)
	}

	# ---------------------------------------------------------------------------
	# Filters
	listing <- .applyFilters(listing, filterCovariates, filterIds, id_col = "id")

	filter_only <- setdiff(filter_in_df, unique(c(splitCovariates, strat_cov_names)))
	if (length(filter_only) > 0) {
		listing <- listing %>% dplyr::select(-dplyr::any_of(filter_only))
	}

	# ---------------------------------------------------------------------------
	# Sort & finalize column order.
	# Primary keys: splitCovariates (grouped modalities) then numeric-then-character id, occs, admid, time.
	split_sort <- intersect(user_split, names(listing))
	within_syms <- rlang::syms(c(occ_cols, admid_col))
	listing <- listing %>%
		dplyr::mutate(.id_numeric = suppressWarnings(as.numeric(id))) %>%
		dplyr::arrange(
			dplyr::across(dplyr::all_of(split_sort)),
			dplyr::coalesce(.id_numeric, Inf),
			id,
			!!!within_syms,
			suppressWarnings(as.numeric(.data[[time_col]]))) %>%
		dplyr::select(-.id_numeric)

	# Cross-subject split covariates sit left of id (grouping keys leftmost);
	# occs stay nested to the right of id (within-subject)
	strat_front <- intersect(setdiff(splitCovariates, occ_cols), names(listing))
	front <- c(strat_front, "id", occ_cols, admid_col, time_col, amount_col)
	listing <- listing %>%
		dplyr::relocate(dplyr::any_of(front))

	# ==========================================================================
	# MOCKING

	if (isTRUE(mock)) {
		col_types <- c(
			stats::setNames("id", "id"),
			stats::setNames(rep("occ",    length(occ_cols)),        occ_cols),
			stats::setNames(rep("admid",  length(admid_col)),       admid_col),
			stats::setNames(rep("catcov", length(split_in_df)),     split_in_df),
			stats::setNames(rep("catcov", length(strat_cov_names)), strat_cov_names))

		listing <- .mockDataListing(listing, colTypes = col_types)
	}

	attr(listing, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(listing, "r_source")       <- .getRScriptPath()
	return(as.data.frame(listing))

}
