#' Creates a data frame summarizing the continuous covariates in the dataset with descriptive statistics.
#'
#' @param projectFile path to project file (Monolix: .mlxtran or PKanalix: .pkx) or NULL to use the currently loaded project
#' @param covDisplayLabels (optional) data.frame with columns covariate and label for the dataset covariate name
#' @param digits number of digits to display
#' @param roundingMethod "monolix" to use MonolixSuite rounding where digits specifies the number of places after the decimal, or "signif"
#' for digits to specify the number of significant figures
#'
#' @returns a data.frame
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # TODO
tableContinuousCovariateSummary = function(projectFile,
																					 covDisplayLabels = NULL,
																					 digits = 2,
																					 roundingMethod = c("monolix", "signif", "none"))
	# allow null project file to use currently loaded project, also pass in mapping table of names
{

	.checkProjectFile(projectFile, c("monolix", "pkanalix"))
	.ensureProjectLoaded(projectFile)

	if (!is.null(covDisplayLabels) && !is.data.frame(covDisplayLabels))
		stop("The argument 'covDisplayLabels' must be a dataframe.", call. = FALSE)

	if (!is.null(covDisplayLabels) &&
			!identical(names(covDisplayLabels), c("covariate", "label")))
		stop(
			paste0(
				"The argument 'covDisplayLabels' must contain the headers 'covariate' and 'label' Found headers are: ",
				paste(names(covDisplayLabels), collapse = ", ")
			),
			call. = FALSE
		)

	cov_info = lixoftConnectors::getCovariateInformation()

	cov_summary = cov_info$covariate %>%
		dplyr::select(cov_info$name[cov_info$type == "continuous"]) %>%
		tidyr::pivot_longer(cols = dplyr::everything(),
								 names_to = "Covariate",
								 values_to = "Value") %>%
		dplyr::summarise(
			.by = "Covariate",
			N = dplyr::n(),
			Mean = mean(.data$Value),
			SD = stats::sd(.data$Value),
			Med = stats::median(.data$Value),
			Min = min(.data$Value),
			P5 = stats::quantile(.data$Value, 0.05),
			P95 = stats::quantile(.data$Value, 0.95),
			Max = max(.data$Value)
		) %>%
		dplyr::mutate_if(is.numeric, .round_num, digits, roundingMethod)

	# rename covariates
	if (!is.null(covDisplayLabels)) {
		cov_summary = cov_summary %>%
			dplyr::mutate(Covariate = covDisplayLabels$label[match(.data$Covariate, covDisplayLabels$covariate)])
	}

	return(cov_summary)
}

#' Creates a table summarizing the frequency of categorical covariates in the dataset.
#'
#' @param projectFile path to project file (Monolix: .mlxtran or PKanalix: .pkx) or NULL to use the currently loaded project
#' @param covDisplayLabels (optional) data.frame with columns covariate and label for the dataset covariate name
#' @param categoryDisplayLabels (optional) data.frame with columns covariate, category, and label for the dataset categorical covariate categories
#' @param digits number of digits to display
#'
#' @returns a data.frame
#' @export
#'
#' @examples
#' # TODO
tableCategoricalCovariateSummary <- function(projectFile,
																					covDisplayLabels = NULL,
																					categoryDisplayLabels = NULL,
																					digits = 2)
{
	.checkProjectFile(projectFile, c("monolix", "pkanalix"))
	.ensureProjectLoaded(projectFile)

	if (!is.null(covDisplayLabels) && !is.data.frame(covDisplayLabels))
		stop("The argument 'covDisplayLabels' must be a dataframe.", call. = FALSE)

	if (!is.null(covDisplayLabels) &&
			!identical(names(covDisplayLabels), c("covariate", "label")))
		stop(
			paste0(
				"The argument 'covDisplayLabels' must contain the headers 'covariate' and 'label.' Headers found are: ",
				paste(names(covDisplayLabels), collapse = ", ")
			),
			call. = FALSE
		)

	if (!is.null(categoryDisplayLabels) && !is.data.frame(categoryDisplayLabels))
		stop("The argument 'categoryDisplayLabels' must be a dataframe.", call. = FALSE)

	if (!is.null(categoryDisplayLabels) &&
			!identical(names(categoryDisplayLabels),
								 c("covariate", "category", "label")))
		stop(
			paste0(
				"The argument 'categoryDisplayLabels' must contain the headers 'covariate,' 'category,' and 'label.' Headers found are: ",
				paste(names(categoryDisplayLabels), collapse = ", ")
			),
			call. = FALSE
		)

	cov_info = lixoftConnectors::getCovariateInformation()

	cov_data = cov_info$covariate %>%
		dplyr::select(cov_info$name[cov_info$type == "categorical"]) # verify then make summary table
	cov_summary = NULL

	for (c in names(cov_data)) {
		cov = cov_data %>%
			dplyr::summarise(.by = as.name(c), Frequency = dplyr::n()) %>%
			dplyr::mutate(
				Covariate = "",
				Category = !!as.name(c),
				Percentage = .round_num(.data$Frequency / sum(.data$Frequency), digits, "monolix")
			) %>%
			dplyr::select(!as.name(c)) %>%
			dplyr::relocate("Frequency", .before = "Percentage")

		# rename categories
		if (!is.null(categoryDisplayLabels)) {
			cov = cov %>%
				dplyr::mutate(Category = dplyr::filter(categoryDisplayLabels,
																							 .data$covariate == c)$label[match(.data$Category, dplyr::filter(categoryDisplayLabels,
																							 																																.data$covariate == c)$category)])
		}

		# rename covariates
		cov$Covariate[1] = if (is.null(covDisplayLabels)) {
			c
		} else {
			covDisplayLabels$label[covDisplayLabels$covariate == c][1]
		}
		cov_summary = rbind(cov_summary, cov)

	}

	return(cov_summary)
}
