#' tableRunHistory
#'
#' @description
#' Creates a summary table of the Monolix run history from a Sycomore project,
#' including model selection criteria (OFV/LL and BICc), parent relationships,
#' ratings, and comments.
#'
#' @param sycomoreProjectFile Character string specifying the path to a Sycomore
#'   project file (.syc). This argument is mandatory.
#'
#' @param roundDigits Integer specifying the number of digits for rounding
#'   numeric values. Default is `2`.
#'
#' @param roundingMethod Character string specifying the rounding method:
#'   - `"monolix"` (default): Adaptive rounding where digits specifies decimal places
#'   - `"signif"`: Standard significant figures rounding
#'   - `"none"`: No rounding applied
#'
#' @param sortByCriteria Character string specifying which criteria to sort by:
#'   - `"LL"` (default): Sort by log-likelihood/OFV (lowest first = best model)
#'   - `"BICc"`: Sort by BICc (lowest first = best model)
#'   - `"none"`: No sorting applied
#'
#'   When both linearization and importance sampling results are available,
#'   the function automatically detects which method to use based on the
#'   root/parent project.
#'
#' @param filterCriteria Character string specifying which criteria columns to display:
#'   - `"none"` (default): Show all criteria columns (LL and BICc)
#'   - `"LL"`: Show only LL columns (removes BICc columns)
#'   - `"BICc"`: Show only BICc columns (removes LL columns)
#'
#'   Note: When sorting by a criteria, `filterCriteria` must be `"none"` or
#'   match the sorting criteria. For example, `sortByCriteria = "LL"` with
#'   `filterCriteria = "BICc"` will produce an error.
#'
#' @param prettyLabels Logical; if `TRUE` (default), applies human-readable
#'   column labels (e.g., "LL (Imp. Sampling)" instead of "LLis").
#'
#' @return A data.frame containing:
#'   \itemize{
#'     \item \code{Run}: Project/run name
#'     \item \code{Parent}: Parent run name (empty for root projects)
#'     \item \code{LLlin}, \code{LLis}: Log-likelihood from linearization/importance sampling
#'     \item \code{BICclin}, \code{BICcis}: BICc from linearization/importance sampling
#'     \item \code{dLLlin}, \code{dLLis}: Delta LL from parent
#'     \item \code{dBICclin}, \code{dBICcis}: Delta BICc from parent
#'     \item \code{Comments}: Comments from the mlxtran file
#'     \item \code{Rating}: User rating from Sycomore
#'   }
#'   Columns with all NA values are automatically removed.
#'
#' @details
#' The function parses the Sycomore project file to extract the run hierarchy
#' and relationships, then loads each individual Monolix project to retrieve
#' the log-likelihood results and comments.
#'
#' Delta values (dLL, dBICc) represent the difference between each run and
#' its parent run, useful for comparing nested models.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage - sorted by LL, showing all criteria
#' tableRunHistory("path/to/project.syc")
#'
#' # Sort by BICc, show only BICc columns
#' tableRunHistory("path/to/project.syc", sortByCriteria = "BICc", filterCriteria = "BICc")
#'
#' # No sorting, show all criteria with 3 decimal places
#' tableRunHistory("path/to/project.syc", sortByCriteria = "none", roundDigits = 3)
#' }
tableRunHistory <- function(sycomoreProjectFile,
														roundDigits      = 2,
														roundingMethod   = c("monolix", "signif", "none"),
														sortByCriteria   = c("LL", "BICc", "none"),
														filterCriteria   = c("none", "LL", "BICc"),
														prettyLabels     = TRUE) {

	if (is.null(sycomoreProjectFile)) {
		stop("The argument 'sycomoreProjectFile' is mandatory.", call. = FALSE)
	}

	.checkProjectFile(sycomoreProjectFile, "sycomore")

	if (!file.exists(sycomoreProjectFile)) {
		stop(paste0("Could not find Sycomore project ", path.expand(sycomoreProjectFile)), call. = FALSE)
	}

	# Validate argument combinations
	sortByCriteria <- match.arg(sortByCriteria)
	filterCriteria <- match.arg(filterCriteria)
	.validateCriteriaArgs(sortByCriteria, filterCriteria)

	# read lines from sycomore file
	sycContent <- readLines(sycomoreProjectFile)

	# keep section on relationship
	rel <- sycContent[(which(sycContent == "[relationship]") + 1):length(sycContent)]

	# replace \\ by = and split by =
	spl <- strsplit(sub("\\", "=", rel, fixed = TRUE), "=", fixed = TRUE)
	# add last cell if missing (can happen if no parent)
	for (i in 1:length(spl)) {
		if (length(spl[[i]]) < 3) {
			spl[[i]] <- c(spl[[i]], "") # add empty string
		}
	}
	# from list to data.frame
	df <- data.frame(matrix(unlist(spl), ncol = 3, byrow = TRUE))
	# keep only lines about path, parent and info (others are children and redundant, or are size info)
	df <- df %>% dplyr::filter(.data$X2 %in% c("path", "parent", "info"))
	# from long format to wide format
	dfw <- stats::reshape(df,
												idvar     = "X1",
												timevar   = "X2",
												direction = "wide"
	)
	# rename columns
	dfw <- dfw %>%
		dplyr::rename(Index  = "X1",
									Path   = "X3.path",
									Parent = "X3.parent")


	# keep section on projectList
	proj <- sycContent[2:(which(sycContent == "[relationship]") - 2)]

	# replace \\ by = and split by =
	spp <- strsplit(sub("\\", "=", proj, fixed = TRUE), "=", fixed = TRUE)

	# delete line with size= which can be anywhere in the list (not always at beginning or end)
	for (i in 1:length(spp)) {
		if (spp[[i]][1] == "size") {
			indexToDelete <- i
		}
	}
	spp[[indexToDelete]] <- NULL # delete line

	# from list to data.frame
	dfp <- data.frame(matrix(unlist(spp), ncol = 3, byrow = T))
	# from long format to wide format
	dfpw <- stats::reshape(dfp,
												 idvar     = "X1",
												 timevar   = "X2",
												 direction = "wide")
	# rename columns
	dfpw <- dfpw %>%
		dplyr::rename(Index    = "X1",
									Path     = "X3.path",
									Used     = "X3.used",
									Selected = "X3.selected",
									Rating   = "X3.rating")
	# filter projects not used
	dfpw <- dfpw %>% dplyr::filter(.data$Used == "true")

	# merge the two data.frames
	dfall <- merge(dfw, dfpw, by = "Path") %>% dplyr::select(c("Path", "Parent", "Rating"))

	# from filepath to file name
	dfall <- dfall %>%
		dplyr::mutate(Run    = tools::file_path_sans_ext(basename(.data$Path)),
									Parent = tools::file_path_sans_ext(basename(.data$Parent)))

	dfall$LLlin    <- NA
	dfall$LLis     <- NA
	dfall$BICclin  <- NA
	dfall$BICcis   <- NA
	dfall$Comments <- NA

	# load all projects and extract LL and BIC + extract comments from mlxtran file
	for (i in 1:nrow(dfall)) {
		projPath <- paste0(
			dirname(sycomoreProjectFile),
			"/",
			tools::file_path_sans_ext(dfall$Path[i]))

		LLfile <- paste0(projPath, "/LogLikelihood/logLikelihood.txt")
		if (file.exists(LLfile)) {
			dfLL <- utils::read.csv(LLfile)
		} else {
			dfLL <- NULL
		}

		# extract LL and BIC
		if (!is.null(dfLL$linearization)) {
			dfall$BICclin[i] <- dfLL$linearization[dfLL$criteria == "BICc"]
			dfall$LLlin[i]   <- dfLL$linearization[dfLL$criteria == "OFV"]
		}
		if (!is.null(dfLL$importanceSampling)) {
			dfall$BICcis[i] <- dfLL$importanceSampling[dfLL$criteria == "BICc"]
			dfall$LLis[i]   <- dfLL$importanceSampling[dfLL$criteria == "OFV"]
		}
		# extract comments from mlxtran
		mlxTranContent <- readLines(paste0(projPath, ".mlxtran"))
		if (length(which(mlxTranContent == "[COMMENTS]")) == 0) {
			dfall$Comments[i] <- ""
		} else {
			# get comments
			tmp <- mlxTranContent[(which(mlxTranContent == "[COMMENTS]") + 1):length(mlxTranContent)]
			# remove first character which is ;
			tmp <- sub(".", "", tmp)
			dfall$Comments[i] <- paste(tmp, collapse = "\n")
		}
	}

	# calculate deltaLL and deltaBIC
	dfall$dLLlin   <- NA
	dfall$dLLis    <- NA
	dfall$dBICclin <- NA
	dfall$dBICcis  <- NA
	for (i in 1:nrow(dfall)) {
		if (dfall$Parent[i] != "") {
			# has a parent
			indexParent       <- which(dfall$Run == dfall$Parent[i])
			dfall$dLLlin[i]   <- dfall$LLlin[i]   - dfall$LLlin[indexParent]
			dfall$dLLis[i]    <- dfall$LLis[i]    - dfall$LLis[indexParent]
			dfall$dBICclin[i] <- dfall$BICclin[i] - dfall$BICclin[indexParent]
			dfall$dBICcis[i]  <- dfall$BICcis[i]  - dfall$BICcis[indexParent]
		}
	}

	dfall <- dfall %>%
		dplyr::select(dplyr::where(~ any(!is.na(.x)))) %>%
		dplyr::relocate("Comments", "Rating", .after = dplyr::last_col()) %>%
		dplyr::relocate("Run")
	dfall <- .round_num(dfall, roundDigits, rounding_method = roundingMethod)

	# Apply sorting if requested
	dfall <- .sortRunHistory(dfall, sortByCriteria = sortByCriteria)

	# Filter to show only selected criteria columns
	dfall <- .filterCriteriaColumns(dfall, filterCriteria = filterCriteria)

	if (isTRUE(prettyLabels)) {
		dfall <- .prettyLabels(dfall)
	}

	return(dfall)
}




#' Creates a table combining population parameter estimates with information on their uncertainties.
#' The nameMapping data.frame provides the mapping between the Monolix parameter name (`Parameter`), the parameter
#' display name (`Label`) and the parameter units (`Unit`).
#'
#' The display names or units can include special characters
#' by using the Unicode code (e.g., `\U03C9` for omega). The order of the rows also controls the order of the parameters
#' in the table. The results will always be grouped into sections of fixed effects, random effects,
#' and error parameters, in that order, with a column Section identifying the parameter type.
#' See the function [createNameMapping()] to more easily create the `nameMapping` data.frame.
#' @details
#' **Table Format:**
#' The function supports two output formats controlled by `use_wide_format`:
#'
#' - **Long format** (`use_wide_format = FALSE`, default): Each parameter (fixed effects, IIV, IOV,
#'   correlations, residual error) appears in its own row. All parameters have the same column structure.
#'
#' - **Wide format** (`use_wide_format = TRUE`): Random effects (IIV/IOV) appear as additional columns
#'   next to their corresponding fixed effects. For example, a table might have columns:
#'   Parameter, Fixed Effect Estimate, RSE (%), IIV SD, IIV RSE (%), IOV SD, IOV RSE (%).
#'   Correlations and residual error parameters remain in separate rows.
#'
#' **Section Classification:**
#'
#' The `Section` column classifies parameters into categories:
#' - `FE`: Fixed effects (population parameters)
#' - `FE_CAT`: Fixed effects by covariate category (when `include_FEcategory = TRUE`)
#' - `IIV`: Inter-individual variability (omega parameters)
#' - `IOV`: Inter-occasion variability (gamma parameters)
#' - `CORR_IIV`: Correlations between IIV parameters
#' - `CORR_IOV`: Correlations between IOV parameters
#' - `RES_ERR`: Residual error parameters
#' - `LAT`: Latent covariate probabilities
#'
#' When used with [print_table_flex()], these sections automatically generate visual section
#' headers to improve table readability.
#'
#' @param projectFile Character string specifying the path to a Monolix .mlxtran project file, or `NULL` (default) to use the currently loaded project.
#' @param nameMapping Data frame with columns `Parameter`, `Label`, and optionally `Unit`
#'   to specify the display name and units for each Monolix parameter name. If `NULL` (default),
#'   parameter names are used as-is with "_pop" suffix removed. See [createNameMapping()] for
#'   a helper function to create this mapping.
#' @param include_RSE Logical; if `TRUE` (default), includes relative standard error (RSE) columns in the table. Applies to both fixed effects and random effects in wide format.
#' @param include_IIV Logical; if `TRUE` (default), includes inter-individual variability (IIV)
#'   omega estimates in wide format tables. When `FALSE`, only fixed effect estimates are shown
#'   in wide format (CV% can still be shown if `include_CV = TRUE`).
#' @param include_IOV Logical; if `TRUE` (default), includes inter-occasion variability (IOV) gamma parameters in the table. When `FALSE`, IOV parameters are excluded from the output.
#' @param include_CI Logical; if `TRUE`, includes confidence interval columns (P2.5, P97.5) in the table. Default is `FALSE`. Applies to both fixed effects and random effects in wide format.
#' @param include_CV Logical; if `TRUE`, includes coefficient of variation (CV%) for random effects.
#'   Default is `FALSE`. In wide format, CV% is shown as an additional column alongside omega estimates
#'   when `include_IIV = TRUE`.
#' @param include_bootstrap Logical; if `TRUE`, includes bootstrap results: median estimate,
#'   confidence interval (e.g., `Boot_CI_2.5_97.5`), and RSE (if available from bootstrap).
#'   Bootstrap confidence interval levels automatically adapt to project settings (90%, 95%, 99%).
#'   Default is `FALSE`. Requires bootstrap analysis to have been run in Monolix.
#' @param use_wide_format Logical; if `TRUE`, presents random effects (IIV/IOV) in wide format
#'   with omega estimates as columns next to their corresponding fixed effects. If `FALSE` (default),
#'   uses long format where each parameter appears in its own row. Wide format creates sections:
#'   Fixed effects | Random effects with split header, plus separate headers for Correlations and
#'   Residual error.
#' @param std_err_method Character string specifying the standard error method to use. Options: `""` (default, uses whichever method the project has, preferring linear if both), `"linear"`, or `"stochasticApproximation"`.
#' @param roundDigits Integer specifying the number of digits to display. Default is 3. Interpretation depends on `roundingMethod`.
#' @param roundingMethod Character string specifying the rounding method. Options:
#'   - `"monolix"` (default): Adaptive rounding where `digits` specifies decimal places,
#'     with extra digits added for values < 1
#'   - `"signif"`: Standard significant figures rounding where `digits` specifies
#'     the number of significant figures
#'   - `"none"`: No rounding applied
#' @param prettyLabels Logical; if `TRUE` (default), applies pretty formatting to column names with newline characters for better table display (e.g., "Fixed Effect\\nEstimate", "IIV\\nSD").
#'   If `FALSE`, uses simpler column names without newlines (e.g., "FE_Estimate", "IIV_SD").
#'
#' @return A data frame containing parameter estimates and uncertainties with the following structure:
#'   \describe{
#'     \item{Parameter}{Parameter name (or Label if provided in nameMapping)}
#'     \item{Section}{Factor column identifying parameter type (FE, IIV, IOV, CORR_IIV, CORR_IOV, RES_ERR)}
#'     \item{Estimate}{Parameter estimate (fixed effects) or variability estimate (random effects)}
#'     \item{RSE}{Relative standard error (if `include_RSE = TRUE`)}
#'     \item{P2.5, P97.5}{Confidence interval bounds (if `include_CI = TRUE`)}
#'     \item{CV%}{Coefficient of variation for random effects (if `include_CV = TRUE`)}
#'   }
#'
#'   The returned data frame includes attributes:
#'   \describe{
#'     \item{table_type}{"pop" - identifies this as a population parameter table}
#'     \item{format}{"wide" or "long" - indicates the table format}
#'     \item{include_IOV}{Logical - whether IOV parameters are included}
#'   }
#'
#'   These attributes are used by [print_table_flex()] to automatically add
#'   section headers when rendering the table.
#'
#' @export
#' @seealso [createNameMapping()], [print_table_flex()]
#'
#' @examples
#' \dontrun{
#' # Basic usage with default long format
#' pop_table <- tableParameterEstimates()
#' print_table_flex(pop_table, color = c("RSE"))
#'
#' # Wide format with IIV and CV%
#' pop_table_wide <- tableParameterEstimates(
#'   use_wide_format = TRUE,
#'   include_IIV = TRUE,
#'   include_CV = TRUE)
#' print_table_flex(pop_table_wide, color = c("RSE"), theme_fn = theme_booktabs)
#'
#' # With custom name mapping and confidence intervals
#' name_map <- createNameMapping(
#'   c("Cl_pop", "V_pop", "ka_pop"),
#'   labels = c("Clearance", "Volume", "Absorption rate"),
#'   units = c("L/h", "L", "1/h"))
#' pop_table <- tableParameterEstimates(
#'   nameMapping = name_map,
#'   include_CI = TRUE,
#'   include_RSE = TRUE)
#'
#' # Exclude IOV parameters
#' pop_table_no_iov <- tableParameterEstimates(include_IOV = FALSE)
#'
#' # Simple column names (no pretty formatting)
#' pop_table_simple <- tableParameterEstimates(prettyLabels = FALSE)
#' }
tableParameterEstimates <- function(projectFile        = NULL,
																		nameMapping        = NULL,
																		include_RSE        = TRUE,
																		include_IIV        = TRUE,
																		include_IOV        = TRUE,
																		include_CI         = FALSE,
																		include_CV         = FALSE,
																		include_FEcategory = FALSE,
																		include_bootstrap  = FALSE,
																		use_wide_format    = FALSE,
																		std_err_method     = "",
																		roundDigits        = 3,
																		roundingMethod     = c("monolix", "signif", "none"),
																		prettyLabels       = TRUE) {


	# projectFile =prj10
	# nameMapping = NULL
	# include_RSE = TRUE
	# include_IIV = TRUE
	# include_IOV = TRUE
	# include_CV =TRUE
	# include_CI= TRUE
	# include_FEcategory = FALSE
	# include_bootstrap  = TRUE
	# use_wide_format = TRUE
	# std_err_method  = ""
	# roundDigits     = 3
	# roundingMethod  = "signif"
	# prettyLabels = FALSE

	roundingMethod <- match.arg(roundingMethod)

	.checkProjectFile(projectFile, "monolix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "monolix", resultType = "pop")

	availableParam <- names(lixoftConnectors::getEstimatedPopulationParameters(coefficientsOfVariation = include_CV,
																																						 withValuesByGroups      = include_FEcategory))

	if (is.null(nameMapping)) {
		nameMapping <- data.frame(Parameter = availableParam, Label = sub("_pop$", "", availableParam))
	}
	if (!is.data.frame(nameMapping)) {
		stop("The argument 'nameMapping' must be a dataframe.", call. = FALSE)
	}

	if (!("Parameter" %in% names(nameMapping)) || !("Label" %in% names(nameMapping))) {
		stop(paste0(
			"The argument 'nameMapping' must contain the headers 'Parameter', 'Label' and optionally 'Unit', in this order. Found headers are: ",
			paste(names(nameMapping), collapse = ", ")
		), call. = FALSE)
	}

	# if (is.null(availableParam) | length(availableParam) < 1) {
	#   stop("The Monolix project does not contain valid results.", call. = FALSE)
	# }

	param_renaming <- nameMapping$parameter
	wrong_param_renaming <- param_renaming[!(param_renaming %in% availableParam)]
	if (length(wrong_param_renaming) > 0) {
		warning(paste0(
			"The column 'parameter' of the argument 'nameMapping' contains the following parameters which do not appear in the project: ",
			paste(wrong_param_renaming, collapse = ", "), ". They will be ignored."
		), call. = FALSE)
	}


	pop <- data.frame(lixoftConnectors::getEstimatedPopulationParameters(coefficientsOfVariation = include_CV,
																																			 withValuesByGroups      = include_FEcategory))
	names(pop) <- c("Estimate")
	pop$Parameter <- rownames(pop)

	err <- NULL

	if (include_RSE) {
		# Validate RSE availability with method
		.validateProjectResultsAvailability(software = "monolix", resultType = "rse", method = std_err_method)
		err <- lixoftConnectors::getEstimatedStandardErrors(method = std_err_method, withValuesByGroups = include_FEcategory)
		if (length(err) == 0) {
			.TFL_message("Standard errors not available for method: ", std_err_method)
		} else {
			if (length(err) > 1) {
				.TFL_message("Standard errors available for multiple methods, selecting ", names(err)[[1]])
			}
			err <- err[[1]]
		}

		if (is.null(err)) {
			err <- data.frame(Parameter = pop$Parameter, RSE = NA)
		} else {
			err$rse <- abs(as.numeric(err$rse)) # corrects bug that RSE has negative sign
			err <- err %>%
				dplyr::select(c("parameter", "rse")) %>%
				dplyr::rename(Parameter = "parameter", RSE = "rse")
		}
	}

	CI <- NULL
	ci_col_name <- NULL

	if (include_CI) {
		# Validate RSE availability with method
		.validateProjectResultsAvailability(software = "monolix", resultType = "rse", method = std_err_method)

		interval_level <- lixoftConnectors::getStandardErrorEstimationSettings()$intervallevel

		# Calculate percentiles based on interval level
		lower_p     <- (100 - interval_level) / 2
		upper_p     <-  100 - lower_p
		lower_col   <- paste0("P", lower_p)
		upper_col   <- paste0("P", upper_p)
		ci_col_name <- paste0("CI_", lower_p, "_", upper_p)

		CI <- lixoftConnectors::getEstimatedConfidenceIntervals(method = std_err_method, withValuesByGroups = include_FEcategory)
		if (length(CI) == 0) {
			.TFL_message("Standard errors not available for method: ", std_err_method)
		} else {
			if (length(CI) > 1) {
				.TFL_message("Standard errors available for multiple methods, selecting ", names(CI)[[1]])
			}
			CI <- CI[[1]]
		}

		if (is.null(CI)) {
			CI <- data.frame(Parameter = pop$Parameter)
			CI[[lower_col]] <- NA_real_
			CI[[upper_col]] <- NA_real_

			CI[[ci_col_name]] <- NA_character_
			CI <- CI %>% dplyr::select(Parameter, !!ci_col_name)
		} else {
			# Process CI with dynamic column detection
			# Verify the detected columns match settings
			percentile_cols <- grep("^P[0-9]", names(CI), value = TRUE)

			if (length(percentile_cols) == 0) {
				warning("No percentile columns found in CI results, using settings-based names")
			}
			CI <- CI %>%
				dplyr::mutate(
					Parameter = .data$parameter,
					lower_rounded = .round_num(.data[[lower_col]], digits = roundDigits, rounding_method = roundingMethod),
					upper_rounded = .round_num(.data[[upper_col]], digits = roundDigits, rounding_method = roundingMethod),
					!!ci_col_name := paste0("(", lower_rounded, "; ", upper_rounded, ")")) %>%
				dplyr::select(Parameter, !!ci_col_name)
		}
	}

	bootstrap <- NULL

	if (include_bootstrap) {
		# Validate RSE availability with method
		.validateProjectResultsAvailability(software = "monolix", resultType = "bootstrap", method = std_err_method)

		boot_interval_level <- getBootstrapSettings()$level

		# Calculate percentiles based on interval level
		boot_lower_p     <- (100 - boot_interval_level) / 2
		boot_upper_p     <-  100 - lower_p
		boot_lower_col   <- paste0("P", lower_p)
		boot_upper_col   <- paste0("P", upper_p)
		boot_ci_col_name <- paste0("CI_", lower_p, "_", upper_p)

		bootstrap <- lixoftConnectors::getBootstrapResults()$populationSummary
		if (is.null(bootstrap)) {
			# No bootstrap results;  use default naming (could be updated later)
			bootstrap <- data.frame(
				Parameter    = pop$Parameter,
				Boot_Median  = NA_real_,
				Boot_CI      = NA_character_,
				Boot_RSE     = NA_real_)
		} else {
			has_rse <- "RSE(%)" %in% names(bootstrap)

			# Find percentile columns
			boot_percentile_cols <- grep("^P[0-9]", names(bootstrap), value = TRUE)

			# Extract numeric values from column names
			boot_percentile_values <- as.numeric(sub("^P", "", boot_percentile_cols))

			boot_lower_p     <- min(boot_percentile_values)
			boot_upper_p     <- max(boot_percentile_values)
			boot_lower_col   <- paste0("P", boot_lower_p)
			boot_upper_col   <- paste0("P", boot_upper_p)
			boot_ci_col_name <- paste0("Boot_CI_", boot_lower_p, "_", boot_upper_p)

			cols_to_select <- c(boot_lower_col, boot_upper_col, "MEDIAN")
			final_cols     <- c("Parameter", "Boot_Median", boot_ci_col_name)
			if (has_rse) {
				cols_to_select <- c(cols_to_select, "RSE(%)")
				final_cols     <- c(final_cols, "Boot_RSE")
			}
			bootstrap <- bootstrap %>%
				dplyr::select(dplyr::all_of(cols_to_select)) %>%
				dplyr::mutate(
					Parameter          = rownames(.),
					boot_lower_rounded = .round_num(.data[[boot_lower_col]], digits = roundDigits, rounding_method = roundingMethod),
					boot_upper_rounded = .round_num(.data[[boot_upper_col]], digits = roundDigits, rounding_method = roundingMethod),
					Boot_Median        = .round_num(MEDIAN,                  digits = roundDigits, rounding_method = roundingMethod),
					!!boot_ci_col_name := paste0("(", boot_lower_rounded, "; ", boot_upper_rounded, ")"))

			if (has_rse) {
				bootstrap <- bootstrap %>% dplyr::mutate(Boot_RSE = .round_num(`RSE(%)`, digits = roundDigits, rounding_method = roundingMethod))
			}
			bootstrap <- bootstrap %>% dplyr::select(dplyr::all_of(final_cols))
		}
	}

	# Get parameters WITHOUT fixed effects by category
	params_no_FEcategory<- names(lixoftConnectors::getEstimatedPopulationParameters(coefficientsOfVariation = include_CV, withValuesByGroups = FALSE))
	# Set difference gives the category-specific FE parameters
	FE_category_params <- setdiff(availableParam, params_no_FEcategory)


	pop$Section <- ""
	pop$Section[grep("_pop|beta_", pop$Parameter)]     <- "FE"
	pop$Section[grep("omega_",     pop$Parameter)]     <- "IIV"
	pop$Section[grep("^corr_",     pop$Parameter)]     <- "CORR"
	pop$Section[grep("^corr1_",    pop$Parameter)]     <- "CORR_IIV"
	pop$Section[grep("gamma_",     pop$Parameter)]     <- "IOV"
	pop$Section[grep("^corr2_",    pop$Parameter)]     <- "CORR_IOV"
	pop$Section[pop$Parameter %in% c("a", "b", "c")]   <- "RES_ERR"
	pop$Section[grep("plcat_",     pop$Parameter)]     <- "LAT"
	pop$Section[pop$Parameter %in% FE_category_params] <- "FE_CAT"

	# Restrict to just the requested parameters in specified order
	pop <- pop[match(nameMapping$Parameter, pop$Parameter), ]

	pop <- dplyr::right_join(nameMapping, pop, by = "Parameter")

	# Add CV column in long format if requested (only when NOT using wide format)
	# Must be done before RSE join to get correct column order: Estimate -> CV -> RSE -> CI
	if (include_CV && !use_wide_format) {
		pop[["CV"]] <- NA_real_
		# Extract CV values for omega and gamma parameters
		cv_params <- grep("_CV$", pop$Parameter, value = TRUE)
		if (length(cv_params) > 0) {
			# For each CV parameter, assign it to the corresponding base parameter
			for (cv_param in cv_params) {
				base_param <- sub("_CV$", "", cv_param)
				cv_value <- pop[pop$Parameter == cv_param, "Estimate"]
				if (base_param %in% pop$Parameter && length(cv_value) > 0) {
					pop[pop$Parameter == base_param, "CV"] <- cv_value
				}
			}
			# Remove the _CV parameter rows from the table
			pop <- pop[!endsWith(pop$Parameter, "_CV"), ]
		}
	}
	if (include_CI) {
		pop <- dplyr::left_join(pop, CI, by = "Parameter")
	}

	if (include_RSE) {
		pop <- dplyr::left_join(pop, err, by = "Parameter")
	}

	if (include_bootstrap) {
		pop <- dplyr::left_join(pop, bootstrap, by = "Parameter")
	}
	if (use_wide_format) {
		# Rename fixed effect columns using hierarchical naming with underscore separator
		names(pop)[names(pop) == "Estimate"] <- "FE_Estimate"
		if (include_RSE && "RSE" %in% names(pop)) {
			names(pop)[names(pop) == "RSE"] <- "FE_RSE"
		}
		if (include_CI && ci_col_name %in% names(pop)) {
			names(pop)[names(pop) == ci_col_name] <- paste0("FE_",ci_col_name)
		}
		# if (include_bootstrap && boot_ci_col_name %in% names(pop)) {
		#   names(pop)[names(pop) == boot_ci_col_name] <- boot_ci_col_name # nothing to do
		# }

		pop_idx      <- grep("_pop", pop$Parameter)
		omega_idx    <- grep("omega_", pop$Parameter)
		omega_cv_idx <- NULL

		pop_root   <- sub("_pop$", "", pop$Parameter[pop_idx])
		omega_root <- sub("^omega_", "", pop$Parameter[omega_idx])

		if (include_CV) {
			omega_cv_idx <- omega_idx[endsWith(pop$Parameter[omega_idx], "_CV")]
			omega_idx    <- setdiff(omega_idx, omega_cv_idx)
			omega_root   <- sub("^omega_", "", pop$Parameter[omega_idx]) # to not duplicate
		}

		# Add IIV columns if requested (default TRUE)
		if (include_IIV && length(omega_idx) > 0) {
			pop[["IIV_SD"]] <- NA_real_
			pop[pop_idx, "IIV_SD"] <- pop[omega_idx[match(pop_root, omega_root)], "FE_Estimate"]

			# Add IIV CV column right after SD
			if (include_CV && length(omega_cv_idx) > 0) {
				pop[["IIV_CV"]] <- NA_real_
				pop[pop_idx, "IIV_CV"] <- pop[omega_cv_idx[match(pop_root, omega_root)], "FE_Estimate"]
			}

			if (include_CI && paste0("FE_",ci_col_name) %in% names(pop)) {
				pop[[paste0("IIV_",ci_col_name)]] <- NA_character_
				pop[pop_idx, paste0("IIV_",ci_col_name)] <- pop[omega_idx[match(pop_root, omega_root)], paste0("FE_",ci_col_name)]
			}
			if (include_RSE && "FE_RSE" %in% names(pop)) {
				pop[["IIV_RSE"]] <- NA_real_
				pop[pop_idx, "IIV_RSE"] <- pop[omega_idx[match(pop_root, omega_root)], "FE_RSE"]
			}

			# Bootstrap IIV columns (same pattern/logic as regular IIV)
			if (include_bootstrap) {
				if ("Boot_Median" %in% names(pop)) {
					pop[["Boot_IIV_Median"]] <- NA_real_
					pop[pop_idx, "Boot_IIV_Median"] <- pop[omega_idx[match(pop_root, omega_root)], "Boot_Median"]
				}

				# Dynamic Boot_IIV_CI column name based on boot_ci_col_name
				if (!is.null(boot_ci_col_name) && boot_ci_col_name %in% names(pop)) {
					# Replace "Boot_CI" with "Boot_IIV_CI" to preserve the confidence level
					boot_iiv_ci_col <- sub("^Boot_CI", "Boot_IIV_CI", boot_ci_col_name)

					pop[[boot_iiv_ci_col]] <- NA_character_
					pop[pop_idx, boot_iiv_ci_col] <- pop[omega_idx[match(pop_root, omega_root)], boot_ci_col_name]
				}

				if ("Boot_RSE" %in% names(pop)) {
					pop[["Boot_IIV_RSE"]] <- NA_real_
					pop[pop_idx, "Boot_IIV_RSE"] <- pop[omega_idx[match(pop_root, omega_root)], "Boot_RSE"]
				}
			}
		}

		# Add IOV columns if requested (default TRUE)
		gamma_idx    <- grep("gamma_", pop$Parameter)
		gamma_cv_idx <- NULL

		if (include_IOV && length(gamma_idx) > 0) {
			gamma_root <- sub("^gamma_", "", pop$Parameter[gamma_idx])

			if (include_CV) {
				gamma_cv_idx <- gamma_idx[endsWith(pop$Parameter[gamma_idx], "_CV")]
				gamma_idx    <- setdiff(gamma_idx, gamma_cv_idx)
				gamma_root   <- sub("^gamma_", "", pop$Parameter[gamma_idx]) # to not duplicate
			}

			pop[["IOV_SD"]] <- NA_real_
			pop[pop_idx, "IOV_SD"] <- pop[gamma_idx[match(pop_root, gamma_root)], "FE_Estimate"]

			# Add IOV CV column right after SD
			if (include_CV && length(gamma_cv_idx) > 0) {
				pop[["IOV_CV"]] <- NA_real_
				pop[pop_idx, "IOV_CV"] <- pop[gamma_cv_idx[match(pop_root, gamma_root)], "FE_Estimate"]
			}

			if (include_RSE && "FE_RSE" %in% names(pop)) {
				pop[["IOV_RSE"]] <- NA_real_
				pop[pop_idx, "IOV_RSE"] <- pop[gamma_idx[match(pop_root, gamma_root)], "FE_RSE"]
			}

			if (include_CI && paste0("FE_",ci_col_name) %in% names(pop)) {
				pop[[paste0("IOV_",ci_col_name)]] <- NA_character_
				pop[pop_idx, paste0("IOV_",ci_col_name)] <- pop[gamma_idx[match(pop_root, gamma_root)], paste0("FE_",ci_col_name)]
			}
			# Bootstrap IOV columns (same as in bootstrap IIV)
			if (include_bootstrap) {
				if ("Boot_Median" %in% names(pop)) {
					pop[["Boot_IOV_Median"]] <- NA_real_
					pop[pop_idx, "Boot_IOV_Median"] <- pop[gamma_idx[match(pop_root, gamma_root)], "Boot_Median"]
				}

				# Dynamic Boot_IOV_CI column name based on boot_ci_col_name
				if (!is.null(boot_ci_col_name) && boot_ci_col_name %in% names(pop)) {
					# Replace "Boot_CI" with "Boot_IOV_CI" to preserve the confidence level
					boot_iov_ci_col <- sub("^Boot_CI", "Boot_IOV_CI", boot_ci_col_name)

					pop[[boot_iov_ci_col]] <- NA_character_
					pop[pop_idx, boot_iov_ci_col] <- pop[gamma_idx[match(pop_root, gamma_root)], boot_ci_col_name]
				}

				if ("Boot_RSE" %in% names(pop)) {
					pop[["Boot_IOV_RSE"]] <- NA_real_
					pop[pop_idx, "Boot_IOV_RSE"] <- pop[gamma_idx[match(pop_root, gamma_root)], "Boot_RSE"]
				}
			}

		}
		#---------------------------------------------------------------------------
		# Reorder columns: group Monolix estimates first, then Bootstrap estimates
		col_order <- c("Section", "Parameter", "Label")

		# SAEM FE columns: Estimate -> CI -> RSE
		col_order <- c(col_order, "FE_Estimate")
		if (include_CI && !is.null(ci_col_name)) {
			fe_ci_col <- paste0("FE_", ci_col_name)
			if (fe_ci_col %in% names(pop)) col_order <- c(col_order, fe_ci_col)
		}
		if (include_RSE && "FE_RSE" %in% names(pop)) {
			col_order <- c(col_order, "FE_RSE")
		}

		# SAEM IIV columns: SD -> CV -> CI -> RSE
		if (include_IIV && "IIV_SD" %in% names(pop)) {
			col_order <- c(col_order, "IIV_SD")
			if (include_CV && "IIV_CV" %in% names(pop)) col_order <- c(col_order, "IIV_CV")
			if (include_CI && !is.null(ci_col_name)) {
				iiv_ci_col <- paste0("IIV_", ci_col_name)
				if (iiv_ci_col %in% names(pop)) col_order <- c(col_order, iiv_ci_col)
			}
			if (include_RSE && "IIV_RSE" %in% names(pop)) col_order <- c(col_order, "IIV_RSE")
		}

		# SAEM IOV columns: SD -> CV -> CI -> RSE
		if (include_IOV && "IOV_SD" %in% names(pop)) {
			col_order <- c(col_order, "IOV_SD")
			if (include_CV && "IOV_CV" %in% names(pop)) col_order <- c(col_order, "IOV_CV")
			if (include_CI && !is.null(ci_col_name)) {
				iov_ci_col <- paste0("IOV_", ci_col_name)
				if (iov_ci_col %in% names(pop)) col_order <- c(col_order, iov_ci_col)
			}
			if (include_RSE && "IOV_RSE" %in% names(pop)) col_order <- c(col_order, "IOV_RSE")
		}

		# Bootstrap FE columns: Median -> CI -> RSE
		if (include_bootstrap && "Boot_Median" %in% names(pop)) {
			col_order <- c(col_order, "Boot_Median")
			if (!is.null(boot_ci_col_name) && boot_ci_col_name %in% names(pop)) {
				col_order <- c(col_order, boot_ci_col_name)
			}
			if ("Boot_RSE" %in% names(pop)) col_order <- c(col_order, "Boot_RSE")
		}

		# Bootstrap IIV columns: Median -> CI -> RSE
		if (include_bootstrap && include_IIV && "Boot_IIV_Median" %in% names(pop)) {
			col_order <- c(col_order, "Boot_IIV_Median")
			if (!is.null(boot_ci_col_name)) {
				boot_iiv_ci_col <- sub("^Boot_CI", "Boot_IIV_CI", boot_ci_col_name)
				if (boot_iiv_ci_col %in% names(pop)) col_order <- c(col_order, boot_iiv_ci_col)
			}
			if ("Boot_IIV_RSE" %in% names(pop)) col_order <- c(col_order, "Boot_IIV_RSE")
		}

		# Bootstrap IOV columns: Median -> CI -> RSE
		if (include_bootstrap && include_IOV && "Boot_IOV_Median" %in% names(pop)) {
			col_order <- c(col_order, "Boot_IOV_Median")
			if (!is.null(boot_ci_col_name)) {
				boot_iov_ci_col <- sub("^Boot_CI", "Boot_IOV_CI", boot_ci_col_name)
				if (boot_iov_ci_col %in% names(pop)) col_order <- c(col_order, boot_iov_ci_col)
			}
			if ("Boot_IOV_RSE" %in% names(pop)) col_order <- c(col_order, "Boot_IOV_RSE")
		}

		# Apply column reordering
		pop <- pop[, col_order]
		#---------------------------------------------------------------------------
		# Remove omega and gamma rows from long format  # <-- This is line 621
		rows_to_remove <- c(omega_idx, omega_cv_idx, gamma_idx, gamma_cv_idx)
		if (length(rows_to_remove) > 0) {
			pop <- pop[-rows_to_remove, ]
		}
	}

	pop <- dplyr::select(pop, -"Parameter") %>%
		dplyr::rename(Parameter = "Label")

	# Set Section as factor with desired order
	pop$Section <- factor(pop$Section,
												levels = c("FE","FE_CAT",
																	 "IIV", "IOV",
																	 "CORR", "CORR_IIV", "CORR_IOV",
																	 "RES_ERR",
																	 "LAT"))

	pop <- pop %>%
		dplyr::arrange(.data$Section) %>%
		dplyr::relocate("Section")
	pop <- .round_num(pop, roundDigits, exclude = c("Section", "Parameter", "Label") , rounding_method = roundingMethod)

	# Apply pretty labels if requested for nice column names display
	if (prettyLabels) {
		pop <- .prettyLabels(pop)
	}

	# for later identfication in flextable function
	attr(pop, "table_type")  <- "pop"
	attr(pop, "format")      <- if (use_wide_format) "wide" else "long"
	attr(pop, "include_IIV") <- use_wide_format && include_IIV
	attr(pop, "include_IOV") <- use_wide_format && include_IOV
	attr(pop, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	return(pop)

	# TODO move this to flextable function (and need to adapt row insertion to be based on new section column)

	# iFirstRan <- suppressWarnings(min(which(
	# 	grepl("omega_|corr_", pop$parameter) == TRUE
	# )))
	# iFirstRes <- suppressWarnings(min(which(
	# 	grepl("omega_|corr_|_pop|beta_", pop$parameter) == FALSE
	# )))
	#
	# if (iFirstRes != Inf) {
	# 	pop <- pop %>% dplyr::add_row(label = "Residual error", .before = iFirstRes)
	# }
	# if (iFirstRan != Inf) {
	# 	pop <- pop %>% dplyr::add_row(label = "Random effects", .before = iFirstRan)
	# }
	# pop <- pop %>% dplyr::add_row(label = "Fixed effects",	.before = 1)
	#
	# iFix <- which(pop$label == "Fixed effects")
	# iRan <- which(pop$label == "Random effects")
	# iRes <- which(pop$label == "Residual error")
	#
	# last_col <- if (include_CI) { "ULCI" }
	# else if (include_RSE) { "rse" }
	# else { "Estimate" }
	#
	# ft_est <- flextable(pop) %>%
	# 	merge_h_range(i = iFix , j1 = "label", j2 = last_col) %>%
	# 	hline(i = iFix, part = "body") %>%
	# 	bold(part = "header") %>%
	# 	bold(i = iFix, part = "body")
	#
	# if (length(iRes) > 0) {
	# 	ft_est <- ft_est %>%
	# 		merge_h_range(i = iRes) %>%
	# 		hline(i = iRes - 1, part = "body") %>%
	# 		hline(i = iRes, part = "body") %>%
	# 		bold(i = iRes, part = "body")
	# }
	# if (length(iRan) > 0) {
	# 	ft_est <- ft_est %>%
	# 		merge_h_range(i = iRan, j1 = "label", j2 = last_col) %>%
	# 		hline(i = iRan - 1, part = "body") %>%
	# 		hline(i = iRan, part = "body") %>%
	# 		bold(i = iRan, part = "body")
	# }
	#
	# ft_est <- ft_est %>%
	# 	set_header_labels(rse = "RSE(%)", label = "Parameter", parameter = "Monolix name", unit = "Unit") %>%
	#
	# return(ft_est)


}

#' Creates a mapping between parameter names used in the Monolix project and display names
#'
#' Creates a data.frame that can be used as the `nameMapping` argument to [tableParameterEstimates()] to specify mapping
#' between the Monolix parameter name (`Parameter`), the parameter display name (`Label`) and the parameter units (`Unit`).
#'
#' The display names or units can include special characters by using the Unicode code (e.g., `\U03C9` for omega).
#' The order of the rows also controls the order of the parameters in the table.
#'
#' @param ... a series a vectors with named arguments (`Parameter, Label, Unit`) or (`Parameter, Label`)
#'
#' @returns a data.frame that can be passed to [tableParameterEstimates()]
#' @export
#' @seealso [tableParameterEstimates()]
#'
#' @examples
#'
#' createNameMapping(
#'     c(Parameter = "V_pop", Label = "V", Unit = "L"),
#'     c(Parameter = "Cl_pop", Label = "Cl", Unit = "L/h"),
#'     c(Parameter = "ka_pop", Label = "ka")
#' )
#'
createNameMapping <- function(...) {
    items <- list(...)

    if (!all(sapply(items, is.vector)) | !all(sapply(items, is.character))) {
        stop("Arguments must be all vectors of type character specifying (Parameter, Label, Unit)", call. = FALSE)
    }
    if (!all(sapply(items, length) %in% 2:3)) {
        stop("Each vector must specify (Parameter, Label, Unit) or (Parameter, Label)", call. = FALSE)
    }
    if (any(sapply(sapply(items, names), is.null))) {
        stop("Each vector must be named, e.g. c(Parameter = 'V_pop', Label = 'V', Unit = 'L')", call. = FALSE)
    }

    parameterMapping <- dplyr::bind_rows(lapply(items, as.data.frame.list))
    parameterMapping[is.na(parameterMapping)] <- ""

    if (!all(names(parameterMapping) %in% c("Parameter", "Label", "Unit"))) {
        stop("Each vector must specify (Parameter, Label, Unit) or (Parameter, Label)", call. = FALSE)
    }

    if (!all(c("Parameter", "Label") %in% names(parameterMapping))) {
        stop("Each vector must specify at minimum (Parameter, Label)", call. = FALSE)
    }

    return(parameterMapping)
}
