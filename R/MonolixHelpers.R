#' Created a data.frame table of the Monolix run history from a Sycomore project.
#'
#' @param sycomoreProjectFile path to Sycomore .syc project file
#' @param digits number of digits to display
#' @param roundingMethod "monolix" to use MonolixSuite rounding where digits specifies the number of places after the decimal, or "signif"
#' for digits to specify the number of significant figures
#'
#' @returns data.frame, including the run name, the parent run, the rating and comments, as well as the
#' -2 * loglikelihood (OFV) and BICc values and the difference from the parent, if available
#' @export
#'
#' @examples
#' # TODO
tableRunHistory <- function(sycomoreProjectFile,
														digits = 2,
														roundingMethod = c("monolix", "signif", "none")) {
	if (is.null(sycomoreProjectFile))
		stop("The argument 'sycomoreProjectFile' is mandatory.", call. = FALSE)

	.checkProjectFile(sycomoreProjectFile, "sycomore")

	if (!file.exists(sycomoreProjectFile))
		stop(paste0("Could not find Sycomore project ", path.expand(sycomoreProjectFile)), call. = FALSE)

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
												idvar = "X1",
												timevar = "X2",
												direction = "wide")
	#rename columns
	dfw <- dfw %>% dplyr::rename(Index = "X1",
															 Path = "X3.path",
															 Parent = "X3.parent")


	# keep section on projectList
	proj <- sycContent[2:(which(sycContent == "[relationship]") - 2)]

	# replace \\ by = and split by =
	spp <- strsplit(sub("\\", "=", proj, fixed = TRUE), "=", fixed = TRUE)

	# delete line with size= which can be anywhere in the list (not always at beginning or end)
	for (i in 1:length(spp)) {
		if (spp[[i]][1] == 'size') {
			indexToDelete <- i
		}
	}
	spp[[indexToDelete]] <- NULL # delete line

	# from list to data.frame
	dfp <- data.frame(matrix(unlist(spp), ncol = 3, byrow = T))
	# from long format to wide format
	dfpw <- stats::reshape(dfp,
												 idvar = "X1",
												 timevar = "X2",
												 direction = "wide")
	#rename columns
	dfpw <- dfpw %>% dplyr::rename(
		Index = "X1",
		Path = "X3.path",
		Used = "X3.used",
		Selected = "X3.selected",
		Rating = "X3.rating"
	)
	# filter projects not used
	dfpw <- dfpw %>% dplyr::filter(.data$Used == "true")

	# merge the two data.frames
	dfall <- merge(dfw, dfpw, by = "Path") %>% dplyr::select(c("Path", "Parent", "Rating"))

	# from filepath to file name
	dfall <- dfall %>% dplyr::mutate(
		Run = tools::file_path_sans_ext(basename(.data$Path)),
		Parent =  tools::file_path_sans_ext(basename(.data$Parent))
	)


	dfall$LLlin <- NA
	dfall$LLis  <- NA
	dfall$BICclin <- NA
	dfall$BICcis  <- NA
	dfall$Comments <- NA

	# load all projects and extract LL and BIC + extract comments from mlxtran file
	for (i in 1:nrow(dfall)) {

		projPath <- paste0(dirname(sycomoreProjectFile),
											 "/",
											 tools::file_path_sans_ext(dfall$Path[i]))


		LLfile <- paste0(projPath, "/LogLikelihood/logLikelihood.txt")
		if (file.exists(LLfile)) {
			dfLL <- utils::read.csv(LLfile)
		}
		else {
			dfLL <- NULL
		}

		# extract LL and BIC
		if (!is.null(dfLL$linearization)) {
			dfall$BICclin[i] = dfLL$linearization[dfLL$criteria == 'BICc']
			dfall$LLlin[i] = dfLL$linearization[dfLL$criteria == 'OFV']
		}
		if (!is.null(dfLL$importanceSampling)) {
			dfall$BICcis[i] = dfLL$importanceSampling[dfLL$criteria == 'BICc']
			dfall$LLis[i] = dfLL$importanceSampling[dfLL$criteria == 'OFV']
		}
		# extract comments from mlxtran
		mlxTranContent <- readLines(paste0(projPath, ".mlxtran"))
		if (length(which(mlxTranContent == "[COMMENTS]")) == 0) {
			dfall$Comments[i] <- ""
		} else{
			# get comments
			tmp <- mlxTranContent[(which(mlxTranContent == "[COMMENTS]") + 1):length(mlxTranContent)]
			# remove first character which is ;
			tmp <- sub('.', '', tmp)
			dfall$Comments[i] <- paste(tmp, collapse = '\n')
		}
	}

	# calculate deltaLL and deltaBIC
	dfall$dLLlin <- NA
	dfall$dLLis  <- NA
	dfall$dBICclin <- NA
	dfall$dBICcis  <- NA
	for (i in 1:nrow(dfall)) {
		if (dfall$Parent[i] != "") {
			# has a parent
			indexParent <- which(dfall$Run == dfall$Parent[i])
			dfall$dLLlin[i]   =  dfall$LLlin[i]   - dfall$LLlin[indexParent]
			dfall$dLLis[i]    =  dfall$LLis[i]    - dfall$LLis[indexParent]
			dfall$dBICclin[i] =  dfall$BICclin[i] - dfall$BICclin[indexParent]
			dfall$dBICcis[i]  =  dfall$BICcis[i]  - dfall$BICcis[indexParent]
		}
	}

	dfall = dfall %>%
		dplyr::select(dplyr::where(~any(!is.na(.x)))) %>%
		dplyr::relocate("Comments", "Rating", .after = dplyr::last_col()) %>%
		dplyr::relocate("Run")
	dfall <- .round_num(dfall, digits, roundingMethod)

	return(dfall)

}

# TODO: consider adding shrinkage and SE

#' Creates a table combining population parameter estimates with information on their uncertainties.
#'
#' The nameMapping data.frame provides the mapping between the Monolix parameter name (`Parameter`), the parameter
#' display name (`Label`) and the parameter units (`Unit`).
#'
#' The display names or units can include special characters
#' by using the Unicode code (e.g., `\U03C9` for omega). The order of the rows also controls the order of the parameters
#' in the table. The results will always be grouped into sections of fixed effects, random effects,
#' and error parameters, in that order, with a column Section identifying the parameter type.
#'
#' See the function [createNameMapping()] to more easily create the `nameMapping` data.frame.
#'
#' @param project_file path to Monolix .mlxtran project file, or NULL to use currently loaded project
#' @param nameMapping data.frame with columns Parameter, Label, and Unit
#' to specify the display name and units for the Monolix parameter name
#' @param include_RSE TRUE to include a column for RSE in the table, FALSE to omit
#' @param include_CI TRUE to include columns for confidence interval in the table, FALSE to omit
#' @param include_CV TRUE to include rows for the coefficient of variation in the table, FALSE to omit
#' @param include_bootstrap TRUE to include bootstrap results for uncertainties (P2.5, Median, P97.5), FALSE to omit
#' @param use_wide_format_omegas TRUE for variability (omegas) in wide format, FALSE for long format
#' @param std_err_method "" to use whichever method the project uses (linear in the case both are present), or
#' "linear" or "stochasticApproximation" to specify the method
#' @param digits number of digits to display
#' @param roundingMethod "monolix" to use Monolix rounding where digits specifies the number of places after the decimal, or "signif"
#' for digits to specify the number of significant figures
#'
#' @returns a table of parameter estimates and uncertainties
#' @export
#' @seealso [createNameMapping()]
#'
#' @examples
#' # TODO
tableParameterEstimates <- function(project_file, nameMapping = NULL, include_RSE = TRUE,
																		include_CI = FALSE, include_CV = FALSE, include_bootstrap = FALSE,
																		use_wide_format_omegas = FALSE, std_err_method = "",
																		digits = 3, roundingMethod = c("monolix", "signif")) {

	roundingMethod <- match.arg(roundingMethod)

	.ensureProjectLoaded(project_file)
	availableParam <- names(lixoftConnectors::getEstimatedPopulationParameters(coefficientsOfVariation = include_CV))

	if (is.null(nameMapping)) {
		nameMapping = data.frame(Parameter = availableParam, Label = sub("_pop$", "", availableParam))
	}
	if (!is.data.frame(nameMapping)) {
		stop("The argument 'nameMapping' must be a dataframe.", call. = FALSE) }

	if (!("Parameter" %in% names(nameMapping)) || !("Label" %in% names(nameMapping))) {
		stop(paste0("The argument 'nameMapping' must contain the headers 'Parameter', 'Label' and optionally 'Unit', in this order. Found headers are: ",
								paste(names(nameMapping), collapse = ", ")), call. = FALSE) }

	if (is.null(availableParam) | length(availableParam) < 1) {
		stop("The Monolix project does not contain valid results.", call. = FALSE) }

	param_renaming <- nameMapping$parameter
	wrong_param_renaming <- param_renaming[!(param_renaming %in% availableParam)]
	if (length(wrong_param_renaming) > 0) {
		warning(paste0("The column 'parameter' of the argument 'nameMapping' contains the following parameters which do not appear in the project: ",
									 paste(wrong_param_renaming, collapse = ", "), ". They will be ignored." ), call. = FALSE) }


	pop <- data.frame(lixoftConnectors::getEstimatedPopulationParameters(coefficientsOfVariation = include_CV))
	names(pop) <- c("Estimate")
	pop$Parameter <- rownames(pop)

	err <- NULL

	if (include_RSE) {
		err <- lixoftConnectors::getEstimatedStandardErrors(method = std_err_method)
		if (length(err) == 0) {
			.TFL_message("Standard errors not available for method: ", std_err_method)
		} else
		{
			if (length(err) > 1) {
				.TFL_message("Standard errors available for multiple methods, selecting ", names(err)[[1]])
			}
			err <- err[[1]]
		}

		if (is.null(err)) {
			err = data.frame(Parameter = pop$Parameter, RSE = NA)
		}
		else {
			err$rse <- abs(as.numeric(err$rse)) # corrects bug that RSE has negative sign
			err <- err %>%
				dplyr::select(c("parameter", "rse")) %>%
				dplyr::rename(Parameter = "parameter", RSE = "rse")
		}
	}

	CI <- NULL

	if (include_CI) {
		CI  <- lixoftConnectors::getEstimatedConfidenceIntervals(method = std_err_method)
		if (length(CI) == 0) {
			.TFL_message("Standard errors not available for method: ", std_err_method)
		} else
		{
			if (length(CI) > 1) {
				.TFL_message("Standard errors available for multiple methods, selecting ", names(CI)[[1]])
			}
			CI <- CI[[1]]
		}

		if (is.null(CI)) {
			CI = data.frame(Parameter = pop$Parameter,
											P2.5 = NA,
											P97.5 = NA)
		}
		names(CI) <- c("Parameter", "P2.5", "P97.5")
	}

	bootstrap <- NULL

	if (include_bootstrap) {
		# boot <- lixoftConnectors::getBootstrapResults()$populationSummary %>%
		# 	dplyr::select("RSE(%)") %>%
		# 	dplyr::rename("rse" = "RSE(%)")
		# boot$Parameter <- rownames(boot)
		bootstrap  <- lixoftConnectors::getBootstrapResults()$populationSummary
		if (is.null(bootstrap)) {
			bootstrap <- data.frame(Parameter = pop$Parameter,
															P2.5 = NA,
															Median = NA,
															P97.5 = NA)
		} else {
			bootstrap  <- bootstrap %>%
				dplyr::select(dplyr::starts_with("P"), "MEDIAN") %>%
				dplyr::rename(Median = "MEDIAN") %>%
				dplyr::mutate(Parameter = rownames(.)) %>%
				dplyr::relocate("Parameter")
		}
		names(bootstrap) <- c("Parameter", paste0("Bootstrap_", names(bootstrap)[2:4]))
	}

	pop$Section <-  ""
	pop$Section[grep("_pop|beta_", pop$Parameter)] <- "Fixed effects"
	pop$Section[grep("omega_|corr_", pop$Parameter)] <- "Random effects"
	pop$Section[pop$Parameter %in% c("a", "b", "c")] <- "Residual error"

	# Restrict to just the requested parameters in specified order
	pop <- pop[match(nameMapping$Parameter, pop$Parameter),]

	pop = dplyr::right_join(nameMapping, pop, by = "Parameter")

	if (include_RSE) {
		pop = dplyr::left_join(pop, err, by = "Parameter")
	}
	if (include_CI) {
		pop = dplyr::left_join(pop, CI, by = "Parameter")
	}
	if (include_bootstrap) {
		pop = dplyr::left_join(pop, bootstrap, by = "Parameter")
	}

	if (use_wide_format_omegas) {
		pop_idx <- grep("_pop", pop$Parameter)
		omega_idx <- grep("omega_", pop$Parameter)
		omega_cv_idx <- NULL

		pop_root <- sub("_pop$", "", pop$Parameter[pop_idx])
		omega_root <- sub("^omega_", "", pop$Parameter[omega_idx])

		if (include_CV) {
			omega_cv_idx <- omega_idx[endsWith(pop$Parameter[omega_idx], "_CV")]
			omega_idx <- setdiff(omega_idx, omega_cv_idx)
			omega_root <- sub("^omega_", "", pop$Parameter[omega_idx]) # to not duplicate
		}

		pop[,ifelse(include_CV, "Variability(CV)", "Variability")] <- ""

		# TODO: variable for column name, need to use get()?
		source_idx <- if (include_CV) { omega_cv_idx } else { omega_idx }
		pop[pop_idx, ifelse(include_CV, "Variability(CV)", "Variability")] <-
			pop[source_idx[match(pop_root, omega_root)], "Estimate"]

		if (include_RSE) {
			pop[,"Variability(RSE)"] <- ""
			pop[pop_idx, "Variability(RSE)"] <-
				pop[omega_idx[match(pop_root, omega_root)], "RSE"]
		}

		pop <- pop[-c(omega_idx, omega_cv_idx),]
	}

	pop <- dplyr::select(pop, -"Parameter") %>%
		dplyr::rename(Parameter = "Label")

	pop <- pop %>%
		dplyr::arrange(.data$Section) %>% # section titles are in alphabetical order or would need to use factor levels
		dplyr::relocate("Section")
	pop <- .round_num(pop, digits, roundingMethod)

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
#'   c(Parameter = "V_pop", Label = "V", Unit = "L"),
#'   c(Parameter = "Cl_pop", Label = "Cl", Unit = "L/h"),
#'   c(Parameter = "ka_pop", Label = "ka"))
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


