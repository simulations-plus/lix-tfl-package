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
#' @param roundMethod Character string specifying the rounding method:
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
#' @param mock Logical; if `TRUE`, masks the table for documentation / example
#'   outputs: the LL/BICc/delta columns become placeholders (e.g. `"xx.xx"`),
#'   `Comments` is blanked, and `Rating` is masked (numeric values become
#'   `"xx.xx"`, non-numeric values are blanked). `Run` and `Parent` hold real
#'   project/file names (which may embed study, compound or company
#'   identifiers) and are relabeled generically (`"project01"`, `"project02"`,
#'   ...) using one consistent mapping, so a `Parent` reference always matches
#'   the generic name assigned to that project's own `Run` row; `Path` is
#'   rebuilt from the relabeled name (keeping only the file extension).
#'   Default `FALSE`.
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
														roundMethod   = c("monolix", "signif", "none"),
														sortByCriteria   = c("LL", "BICc", "none"),
														filterCriteria   = c("none", "LL", "BICc"),
														prettyLabels     = TRUE,
														mock             = FALSE) {

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
												direction = "wide")
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
	dfall <- .round_num(dfall, roundDigits, rounding_method = roundMethod)

	# Apply sorting if requested
	dfall <- .sortRunHistory(dfall, sortByCriteria = sortByCriteria)

	# Filter to show only selected criteria columns
	dfall <- .filterCriteriaColumns(dfall, filterCriteria = filterCriteria)

	if (isTRUE(mock)) {
		rating_is_num <- if ("Rating" %in% names(dfall)) {
			!is.na(suppressWarnings(as.numeric(dfall$Rating)))
		} else {
			logical(0)
		}
		real_names <- unique(c(dfall$Run, dfall$Parent[nzchar(dfall$Parent)]))
		width      <- max(2L, nchar(as.character(length(real_names))))
		proj_map   <- stats::setNames(
			paste0("project", formatC(seq_along(real_names), width = width, flag = "0")),
			real_names)

		if ("Path" %in% names(dfall)) {
			ext        <- tools::file_ext(dfall$Path)
			dfall$Path <- paste0(unname(proj_map[dfall$Run]),
													 ifelse(nzchar(ext), paste0(".", ext), ""))
		}
		dfall$Run    <- unname(proj_map[dfall$Run])
		dfall$Parent <- ifelse(nzchar(dfall$Parent), unname(proj_map[dfall$Parent]), dfall$Parent)

		dfall <- .mockOutputData(dfall, digits = roundDigits,
														 excludeCols = c("Run", "Parent", "Path", "Comments"),
														 naFillAxis  = "column")

		if ("Comments" %in% names(dfall)) dfall$Comments <- ""
		if ("Rating" %in% names(dfall) && length(rating_is_num) > 0) {
			dfall$Rating[!rating_is_num] <- ""
		}
	}

	if (isTRUE(prettyLabels)) {
		dfall <- .prettyLabels(dfall)
	}
	attr(dfall, "project_source") <- sycomoreProjectFile
	attr(dfall, "r_source")       <- .getRScriptPath()
	return(dfall)
}




#' tableModelBuilding
#'
#' @description
#' Creates a summary table of an automatic covariate/statistical model building
#' run (COSSAC, SCM, SAMBA) for a Monolix project. Each row is one model run that
#' was evaluated, with its covariate structure, objective function value
#' (OFV, i.e. -2*log-likelihood), BICc, and the change relative to the nested
#' parent model. For COSSAC runs tested param.-cov. relationship,
#' step direction (forward inclusion / backward elimination) and the selected best
#' model are reported.
#'
#' @details
#' The results are read directly from the loaded Monolix project via
#' [lixoftConnectors::getModelBuildingResults()].
#' Model building must already have been run for the project.
#'
#' **Strategy (auto-detected).** The strategy is read from
#' [lixoftConnectors::getModelBuildingSettings()]. COSSAC and SCM build the model
#' one relationship at a time and the connector returns the tested relationship,
#' the step direction and the selected `bestModel`. SAMBA (`"covSamba"`) instead
#' proposes whole candidate covariate models per iteration, it has no single
#' nested parent and the connector returns neither `tested` nor `bestModel` (see
#' SAMBA note below).
#'
#' **Reference model and deltas.** For COSSAC/SCM each tested model's *nested
#' parent* is the model whose covariate structure is identical except for the
#' single tested relationship. `dOFV`/`dBICc` are computed against that parent. In
#' the first forward round the parent is the model building start model, which
#' already reflects any covariate relationships defined in the base project. For
#' SAMBA the deltas are computed against the start model (`Model 1`) instead.
#'
#' **SAMBA.** Covariate model building with SAMBA always selects by BICc. The
#' connector reports `criterion = "LRT"`, but that setting only governs
#' *statistical* model building (not covered here), so it is ignored for
#' `"covSamba"`. Because the connector does not flag a best model, the best model
#' is derived as the one with the lowest BICc (`BestModel = "Yes"`); when SAMBA
#' finds no improvement this is the start model. An `Improved` flag reports
#' whether BICc decreased vs the start model. No `Tested`, `Step`, `p.value` or
#' `Significant` columns are produced (the candidates are not nested).
#'
#' **Selection criterion (auto-detected, COSSAC/SCM).** The columns adapt to the
#' criterion used for the run, read from
#' [lixoftConnectors::getModelBuildingSettings()]:
#'   - **LRT**: a likelihood-ratio-test `p.value` and a `Significant` flag are
#'     added. The p-value is *computed by this function* (see [.lrtPValue()]) from
#'     the difference in -2*log-likelihood between the reduced and full model,
#'     where the reduced/full roles are set by the step direction (forward: the
#'     reference is the reduced model; backward: the tested model is the reduced
#'     model) rather than by the likelihood values. `df` is the number of
#'     coefficients the relationship adds (1 for a continuous covariate,
#'     categories - 1 for a categorical covariate). The `Significant` flag uses
#'     the **direction-specific** LRT threshold from the settings (forward
#'     threshold for forward steps, backward threshold for backward steps;
#'     Monolix defaults are 0.05 and 0.01).
#'   - **BICc**: no statistical test is done; an `Improved` flag reports whether
#'     BICc decreased relative to the reference model (`dBICc < 0`).
#'
#' @param projectFile Character string specifying the path to a Monolix .mlxtran
#'   project file, or `NULL` (default) to use the currently loaded project.
#' @param customDescriptions Named vector or 2-column dataframe mapping model
#'   labels to descriptive text, added as a separate `Description` column placed
#'   immediately to the right of the `Tested` relationship column (analogous to
#'   `customDescriptions` in [tableParameterEstimates()]). Keys must match the
#'   `Run` labels of the output table (e.g. `"Model 1"`, `"Model 4"`). Rows whose
#'   `Run` is not in the mapping receive `NA`; keys that match no row are ignored
#'   with a message. Example:
#'   `c("Model 1" = "Base covariate model", "Model 17" = "Final model")`.
#' @param filterCriteria Character string specifying which criteria columns to
#'   display (mirrors [tableRunHistory()]):
#'   - `"none"` (default): show both OFV/-2LL and BICc columns (and their deltas)
#'   - `"LL"`: show only OFV/-2LL columns (removes BICc and dBICc)
#'   - `"BICc"`: show only BICc columns (removes LL and dLL)
#' @param sortBy Character string specifying the row order:
#'   - `"index"` (default): keep the algorithm's model order
#'   - `"BICc"`: sort by BICc (lowest first = best model)
#'   - `"LL"`: sort by OFV/-2LL (lowest first)
#'   - `"pValue"`: sort by LRT p-value, smallest first (LRT criterion only)
#'
#'   For stepwise strategies (COSSAC/SCM) sorting stays *within* each step group
#'   (a round: candidates tested in the same direction against the same reference
#'   model), so the forward and backward phases - and repeated rounds within a
#'   phase (e.g. a 2nd forward inclusion) - are never mixed; the rounds keep their
#'   algorithm order. SAMBA has no steps and is sorted globally.
#' @param displayBestModelPerStep Logical; if `TRUE`, reduces the table to the
#'   *accepted* best model per step group (round) plus the Base (start) model,
#'   giving a compact view of the actual model-building path. Default is `FALSE`
#'   (show every evaluated model). Within each round the winner is the model the
#'   algorithm would pick, which is direction-aware under the LRT criterion:
#'   forward steps take the *most* significant candidate (smallest LRT p-value),
#'   backward steps take the *least* significant candidate (largest p-value - the
#'   covariate whose removal is best justified). Under the BICc criterion (no
#'   p-value) the winner is the lowest-BICc model in both directions. The winner
#'   is kept only if that step was actually **accepted**: forward accepts a
#'   *significant* addition (`Significant == "Yes"`); backward accepts a
#'   *non-significant* removal (`Significant == "No"`); under the BICc criterion a
#'   step is accepted when BICc improved (`Improved == "Yes"`). Rounds that ended
#'   in no change (dead-ends - e.g. a final forward round whose best candidate is
#'   not significant) are dropped, so the table reads as the selection path rather
#'   than a mix of path and rejected candidates. Applies only to stepwise
#'   strategies (COSSAC/SCM); for SAMBA and other runs without a `Step` column it
#'   is ignored (with a message) and the full table is returned. The accepted
#'   per-step winners are also recorded on the returned data.frame's
#'   `best_per_step` attribute, which [style_tfl()] uses (via `autoHighlight`) to
#'   shade those rows. See [.modelBuildingBestPerStep()].
#' @param roundDigits Integer specifying the number of digits for rounding
#'   numeric values. Default is `2`.
#' @param roundMethod Character string specifying the rounding method:
#'   `"monolix"` (default), `"signif"`, or `"none"`.
#' @param prettyLabels Logical; if `TRUE` (default), applies human-readable
#'   column labels.
#'
#' @return A data.frame with one row per evaluated model containing (columns
#'   without information for the chosen strategy/criterion are dropped):
#'   \itemize{
#'     \item \code{Run}: model label (e.g. "Model 1")
#'     \item \code{Reference}: label of the reference model that \code{dLL}/
#'       \code{dBICc}/\code{p.value} are computed against (the nested parent for
#'       COSSAC/SCM, the start model for SAMBA; empty for the start model)
#'     \item \code{Tested}: parameter-covariate relationship tested (COSSAC/SCM)
#'     \item \code{Description}: per-model description (only if \code{customDescriptions}
#'       is supplied)
#'     \item \code{Step}: "Base" (start model), "Forward" (add), or "Backward"
#'       (remove) (COSSAC/SCM)
#'     \item \code{CovariateModel}: compact covariate structure string;
#'       \code{"(no covariates)"} for a covariate-free model
#'     \item \code{LL}: OFV (-2*log-likelihood)
#'     \item \code{BICc}: modified BIC
#'     \item \code{dLL}, \code{dBICc}: change vs the reference model (\code{Reference})
#'     \item \code{p.value}, \code{Significant}: LRT p-value and direction-aware
#'       significance flag (LRT criterion, COSSAC/SCM only). The reduced/full roles
#'       are set by the step direction (see [.lrtPValue()]), so \code{p.value} is
#'       meaningful for both forward and backward steps.
#'     \item \code{Improved}: whether BICc decreased vs the reference (BICc criterion
#'       and SAMBA)
#'     \item \code{BestModel}: "Yes" for the best model. For COSSAC/SCM this is the
#'       model the connector flags; for SAMBA it is derived as the lowest-BICc model.
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use the currently loaded project (after running model building)
#' tableModelBuilding()
#'
#' # From a project file, sorted by BICc, showing only BICc columns
#' tableModelBuilding("path/to/project.mlxtran", sortBy = "BICc", filterCriteria = "BICc")
#' }
tableModelBuilding <- function(projectFile        = NULL,
															 customDescriptions = NULL,
															 filterCriteria     = c("none", "LL", "BICc"),
															 sortBy             = c("index", "BICc", "LL", "pValue"),
															 displayBestModelPerStep = FALSE,
															 roundDigits        = 2,
															 roundMethod        = c("monolix", "none", "signif"),
															 prettyLabels       = TRUE) {

	filterCriteria <- match.arg(filterCriteria)
	sortBy         <- match.arg(sortBy)
	roundMethod    <- match.arg(roundMethod)

	if (!is.null(projectFile)) {
		.checkProjectFile(projectFile, "monolix")
	}
	.ensureProjectLoaded(projectFile)

	res <- suppressMessages(lixoftConnectors::getModelBuildingResults())
	if (is.null(res) || length(res) == 0) {
		stop("No model building results found. Run model building (COSSAC/SCM/SAMBA) before calling tableModelBuilding().",
				 call. = FALSE)
	}

	# COSSAC provides 'tested' and 'bestModel'; SAMBA does not.
	hasTested <- any(vapply(res, function(m) !is.null(m$tested), logical(1)))

	# Strategy + selection criterion drive which decision columns are shown.
	settings  <- suppressMessages(lixoftConnectors::getModelBuildingSettings())
	strategy  <- if (!is.null(settings$strategy)) settings$strategy else ""
	isSamba   <- identical(strategy, "covSamba")
	# Covariate model building with SAMBA always selects by BICc. Connector 2024.R1
	# reports criterion = "LRT", but that setting only applies to *statistical*
	# model building --> ignored for covSamba.
	criterion <- if (!is.null(settings$criterion)) toupper(settings$criterion) else "LRT"
	useLRT    <- criterion == "LRT" && !isSamba
	lrtThr    <- if (!is.null(settings$threshold$lrt)) settings$threshold$lrt else c(0.05, 0.01)
	covInfo   <- if (useLRT) suppressMessages(lixoftConnectors::getCovariateInformation()) else NULL

	if (isSamba) {
		# SAMBA proposes whole candidate covariate models rather than one relationship at a time
		# --> no nested parent to test against connector returns neither `tested` nor `bestModel`.
		# Best model is derived as the one with the lowest BICc. Deltas are reported vs start model (Model 1),
		bestIdx   <- which.min(vapply(res, function(m) m$BICc, numeric(1)))
		llStart   <- res[[1]]$LL
		bICcStart <- res[[1]]$BICc

		rows <- lapply(seq_along(res), function(i) {
			m <- res[[i]]
			data.frame(
				Run            = paste("Model", i),
				Reference      = if (i == 1) "" else "Model 1",
				CovariateModel = .covStructureString(m$individualModels),
				LL             = m$LL,
				BICc           = m$BICc,
				dLL            = if (i == 1) NA_real_ else m$LL   - llStart,
				dBICc          = if (i == 1) NA_real_ else m$BICc - bICcStart,
				BestModel      = if (i == bestIdx) "Yes" else "",
				stringsAsFactors = FALSE
			)
		})
		df <- do.call(rbind, rows)

		# BICc decision flag: whether BICc improved vs the start model
		df$Improved <- ifelse(is.na(df$dBICc), "",
													ifelse(df$dBICc < 0, "Yes", "No"))

	} else {
		# Structural key per model, to locate nested parents
		keys <- vapply(res, function(m) .imKey(m$individualModels), character(1))

		rows <- lapply(seq_along(res), function(i) {
			m  <- res[[i]]
			im <- m$individualModels

			tested   <- m$tested
			hasStep  <- !is.null(tested) && length(tested) == 2 && all(nzchar(tested))
			param    <- if (hasStep) tested[1] else NA_character_
			cov      <- if (hasStep) tested[2] else NA_character_

			# nested parent = same structure with the tested cell toggled
			dLL <- NA_real_; dBICc <- NA_real_; step <- if (hasStep) NA_character_ else "Base"
			reference <- ""
			pValue <- NA_real_
			if (hasStep) {
				added <- isTRUE(as.logical(im[param, cov]))
				step  <- if (added) "Forward" else "Backward"
				imParent <- im
				imParent[param, cov] <- !added
				j <- match(.imKey(imParent), keys)
				if (!is.na(j)) {
					reference <- paste("Model", j)
					dLL   <- m$LL   - res[[j]]$LL
					dBICc <- m$BICc - res[[j]]$BICc
					if (useLRT) {
						# Step direction sets the reduced/full roles (not the LL values):
						# forward -> this model is full, reference is reduced; backward -> vice versa.
						if (added) {
							llReduced <- res[[j]]$LL; llFull <- m$LL
						} else {
							llReduced <- m$LL;        llFull <- res[[j]]$LL
						}
						pValue <- .lrtPValue(llReduced, llFull, .covDf(cov, covInfo))
					}
				}
			}

			data.frame(
				Run            = paste("Model", i),
				Reference      = reference,
				Tested         = if (hasStep) paste0(cov, " on ", param) else "",
				Step           = step,
				CovariateModel = .covStructureString(im),
				LL             = m$LL,
				BICc           = m$BICc,
				dLL            = dLL,
				dBICc          = dBICc,
				p.value        = pValue,
				BestModel      = if (isTRUE(m$bestModel)) "Yes" else "",
				stringsAsFactors = FALSE
			)
		})

		df <- do.call(rbind, rows)

		# Decision column, driven by the selection criterion
		if (useLRT) {
			# direction-aware LRT threshold: forward vs backward
			thr <- ifelse(df$Step == "Forward",  lrtThr[1],
										ifelse(df$Step == "Backward", lrtThr[2], NA_real_))
			df$Significant <- ifelse(is.na(df$p.value) | is.na(thr), "",
															 ifelse(df$p.value < thr, "Yes", "No"))
		} else {
			# BICc criterion: no statistical test, just whether BICc improved
			df$p.value  <- NULL
			df$Improved <- ifelse(is.na(df$dBICc), "",
														ifelse(df$dBICc < 0, "Yes", "No"))
		}

		# Drop columns that carry no information for this strategy
		if (!hasTested) {
			df <- df[, setdiff(names(df), c("Reference", "Tested", "Step", "BestModel"))]
		}
	}
	df <- df[, vapply(df, function(x) any(nzchar(as.character(x)) & !is.na(x)), logical(1)) |
					 	names(df) %in% c("LL", "BICc", "dLL", "dBICc"), drop = FALSE]

	# For stepwise strategies (COSSAC/SCM): sort stays *within* each step
	# Forward, backward phases, repeated rounds within a phase are never mixed
	# rounds keep the  algorithm order; SAMBA has no steps --> sorts globally.
	bestPerStep <- .modelBuildingBestPerStep(df)

	sortCol <- switch(sortBy, BICc = "BICc", LL = "LL", pValue = "p.value", NULL)
	if (!is.null(sortCol) && sortCol %in% names(df)) {
		if ("Step" %in% names(df)) {
			grp <- .modelBuildingStepGroups(df$Step, df$Reference)
			df  <- df[order(grp, df[[sortCol]]), , drop = FALSE]
		} else {
			df  <- df[order(df[[sortCol]]), , drop = FALSE]
		}
	}

	# Reduce to *accepted* best model per step (round), i.e. actual model-building path (Base + the per-step winners in `bestPerStep`)
	# Rounds that ended in no change (dead-ends, e.g. a forward round whose best
	# candidate is not significant) contribute no winner --> drop out.
	if (isTRUE(displayBestModelPerStep)) {
		if (!("Step" %in% names(df))) {
			.TFL_message(
				"displayBestModelPerStep applies only to stepwise strategies ",
				"(COSSAC/SCM); returning the full table.")
		} else {
			keepRuns <- c(df$Run[df$Step == "Base"], bestPerStep)  # Base always kept
			df <- df[df$Run %in% keepRuns, , drop = FALSE]
		}
	}

	# Filter which criteria columns to display
	if (filterCriteria == "LL") {
		df <- df[, setdiff(names(df), c("BICc", "dBICc")), drop = FALSE]
	} else if (filterCriteria == "BICc") {
		df <- df[, setdiff(names(df), c("LL", "dLL")), drop = FALSE]
	}
	rownames(df) <- NULL

	# Optional per-model descriptions, keyed on the Run label (mirrors the
	# customDescriptions feature of tableParameterEstimates()).
	descriptions_df <- .normalizeAliasMapping(customDescriptions, "description")
	if (!is.null(descriptions_df)) {
		desc_map       <- stats::setNames(descriptions_df$description, descriptions_df$parameter)
		df$Description <- unname(desc_map[df$Run])

		unmatched_keys <- setdiff(names(desc_map), df$Run)
		if (length(unmatched_keys) > 0) {
			.TFL_message(
				"customDescriptions: the following keys did not match any Run ",
				"in the output table and were ignored: ",
				paste(unmatched_keys, collapse = ", "), ".")
		}

		# Place the description next to the tested relationship (or after Run for
		# strategies without a Tested column, e.g. SAMBA).
		after_col  <- if ("Tested" %in% names(df)) "Tested" else "Run"
		df <- dplyr::relocate(df, "Description", .after = dplyr::all_of(after_col))
	}

	df <- .round_num(df, roundDigits, rounding_method = roundMethod)

	if (isTRUE(prettyLabels)) {
		df <- .prettyLabels(df)
	}

	attr(df, "table_type")     <- "modelBuilding"
	# Run labels of the accepted best model per step, for best-per-step row
	# highlighting in style_tfl (autoHighlight). Restricted to rows still present
	# (e.g. after displayBestModelPerStep reduction). Empty for SAMBA.
	attr(df, "best_per_step")  <- intersect(bestPerStep, df$Run)
	attr(df, "project_source") <- if (!is.null(projectFile)) projectFile else
		suppressMessages(lixoftConnectors::getProjectSettings()$project)
	attr(df, "r_source") <- .getRScriptPath()
	return(df)
}




#' Creates a table combining population parameter estimates with information on their uncertainties.
#'
#' @details
#' **Table Format:**
#' The function supports two output formats controlled by `useWideFormat`:
#'
#' - **Long format** (`useWideFormat = FALSE`, default): Each parameter (fixed effects, IIV, IOV,
#'   correlations, residual error) appears in its own row. All parameters have the same column structure.
#'
#' - **Wide format** (`useWideFormat = TRUE`): Random effects (IIV/IOV) appear as additional columns
#'   next to their corresponding fixed effects. For example, a table might have columns:
#'   Parameter, Fixed Effect Estimate, RSE (%), IIV SD, IIV RSE (%), IOV SD, IOV RSE (%).
#'   Correlations and residual error parameters remain in separate rows.
#'
#' **Section Classification:**
#'
#' The `Section` column classifies parameters into categories:
#' - `FE`: Fixed effects (population parameters)
#' - `FE_CAT`: Fixed effects by covariate category (when `includeFEcategory = TRUE`)
#' - `IIV`: Inter-individual variability (omega parameters)
#' - `IOV`: Inter-occasion variability (gamma parameters)
#' - `CORR_IIV`: Correlations between IIV parameters
#' - `CORR_IOV`: Correlations between IOV parameters
#' - `RES_ERR`: Residual error parameters
#' - `LAT`: Latent covariate probabilities
#'
#' When used with [style_tfl()], these sections automatically generate visual section
#' headers to improve table readability.
#'
#' @param projectFile Character string specifying the path to a Monolix .mlxtran project file, or `NULL` (default) to use the currently loaded project.
#' @param customAliases Named vector or 2-column dataframe mapping parameter names to
#'   custom display labels. Example: `c(ka_pop = "Absorption Rate", V_pop = "Volume")`.
#'   Parameters not in the mapping keep their original names (with "_pop" suffix removed).
#'   Supports special formatting syntax: `[[sub:text]]` for subscript, `[[sup:text]]` for
#'   superscript, and `~text~` for italic. Example: `c(ka_pop = "K[[sub:a]]", Cl_pop = "CL~iv~")`.
#' @param customUnits Named vector or 2-column dataframe mapping parameter names to
#'   custom unit labels. Example: `c(ka_pop = "1/h", V_pop = "L", Cl_pop = "L/h")`.
#'   Units are appended in parentheses: "Volume (L)".
#' @param customDescriptions Named vector or 2-column dataframe mapping parameter names to
#'   descriptive text. Example: `c(Cl_pop = "Apparent clearance", V_pop = "Apparent volume of distribution")`.
#'   Unlike `customAliases`, descriptions are added as a separate `Description` column placed
#'   immediately to the right of the `Parameter` column; the `Parameter` cell value is unchanged.
#'   Parameters not in the mapping receive `NA` in the `Description` column. Keys must match
#'   the raw Monolix parameter names (e.g. `Cl_pop`, `omega_Cl`, `gamma_Cl`, `a`, `b`, ...).
#'   When `useWideFormat = TRUE`, IIV/IOV rows are collapsed into columns alongside their
#'   parent fixed effect; descriptions supplied for `omega_*` / `gamma_*` keys will therefore
#'   not be displayed (a message lists the unmatched keys).
#' @param includeRSE Logical; if `TRUE` (default), includes relative standard error (RSE) columns in the table. Applies to both fixed effects and random effects in wide format.
#' @param includeIIV Logical; if `TRUE` (default), includes inter-individual variability (IIV)
#'   omega estimates in wide format tables. When `FALSE`, only fixed effect estimates are shown
#'   in wide format (CV% can still be shown if `includeCV = TRUE`).
#' @param includeIOV Logical; if `TRUE` (default), includes inter-occasion variability (IOV) gamma parameters in the table. When `FALSE`, IOV parameters are excluded from the output.
#' @param includeCI Logical; if `TRUE`, includes confidence interval columns (P2.5, P97.5) in the table. Default is `FALSE`. Applies to both fixed effects and random effects in wide format.
#' @param includeCV Logical; if `TRUE`, includes coefficient of variation (CV%) for random effects.
#'   Default is `FALSE`. In wide format, CV% is shown as an additional column alongside omega estimates
#'   when `includeIIV = TRUE`.
#' @param includeFEcategory Logical; if `TRUE`, includes separate rows for fixed-effect values by
#'   categorical covariate level (the per-category estimates Monolix exposes via the
#'   `withValuesByGroups` option of `getEstimatedPopulationParameters()`/`getEstimatedStandardErrors()`).
#'   Each per-category row appears with `Section = "FE_CAT"` and is grouped immediately after the
#'   parent fixed-effect rows. Default is `FALSE`.
#' @param includeBootstrap Logical; if `TRUE`, includes bootstrap results: median estimate,
#'   confidence interval (e.g., `Boot_CI_2.5_97.5`), and RSE (if available from bootstrap).
#'   Bootstrap confidence interval levels automatically adapt to project settings (90%, 95%, 99%).
#'   Default is `FALSE`. Requires bootstrap analysis to have been run in Monolix.
#' @param useWideFormat Logical; if `TRUE`, presents random effects (IIV/IOV) in wide format
#'   with omega estimates as columns next to their corresponding fixed effects. If `FALSE` (default),
#'   uses long format where each parameter appears in its own row. Wide format creates sections:
#'   Fixed effects | Random effects with split header, plus separate headers for Correlations and
#'   Residual error.
#' @param stdErrMethod Character string specifying the standard error method to use. Options: `""` (default, uses whichever method the project has, preferring linear if both), `"linear"`, or `"stochasticApproximation"`.
#' @param roundDigits Integer specifying the number of digits to display. Default is 3. Interpretation depends on `roundMethod`.
#' @param roundMethod Character string specifying the rounding method. Options:
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
#'     \item{Parameter}{Parameter name (optionally replaced by a label from `customAliases`, with a unit appended in parentheses when supplied via `customUnits`)}
#'     \item{Section}{Factor column identifying parameter type: `FE`, `FE_CAT`, `IIV`, `IOV`, `CORR`, `CORR_IIV`, `CORR_IOV`, `RES_ERR`, `LAT`}
#'     \item{Estimate}{Parameter estimate (fixed effects) or variability estimate (random effects)}
#'     \item{RSE}{Relative standard error (if `includeRSE = TRUE`)}
#'     \item{CI_2.5_97.5}{Single combined confidence interval column formatted as `"(lower; upper)"` (if `includeCI = TRUE`). The percentile bounds in the column name come from the project's standard-error interval-level setting; e.g. `CI_2.5_97.5` for a 95% interval, `CI_5_95` for a 90% interval.}
#'     \item{CV}{Coefficient of variation for random effects (if `includeCV = TRUE`)}
#'     \item{Boot_Median / Boot_CI_2.5_97.5 / Boot_RSE}{Bootstrap summary columns (if `includeBootstrap = TRUE`). The bootstrap CI column name adapts to the bootstrap interval-level setting (e.g. `Boot_CI_5_95` for a 90% interval).}
#'   }
#'
#'   In wide format (`useWideFormat = TRUE`), random-effect rows are pivoted into columns
#'   alongside their parent fixed effect. The `Estimate` column is renamed `FE_Estimate` and analogous
#'   `IIV_*` / `IOV_*` / `Boot_IIV_*` / `Boot_IOV_*` columns are added for the requested statistics.
#'
#'   The returned data frame includes attributes:
#'   \describe{
#'     \item{table_type}{"pop" — identifies this as a population parameter table}
#'     \item{format}{"wide" or "long" — indicates the table format}
#'     \item{includeIIV}{Logical — `TRUE` only when wide format AND `includeIIV = TRUE`; signals that IIV columns are present}
#'     \item{includeIOV}{Logical — `TRUE` only when wide format AND `includeIOV = TRUE`; signals that IOV columns are present}
#'     \item{project_source}{Directory of the Monolix project that produced the table}
#'     \item{r_source}{Path of the R/Quarto/Rmd script that generated the table (auto-detected at table-creation time, see \code{.getRScriptPath()})}
#'   }
#'
#'   These attributes are used by [style_tfl()] to automatically add
#'   section headers when rendering the table.
#'
#' @export
#' @seealso [style_tfl()]
#'
#' @examples
#' \dontrun{
#' # Basic usage with default long format
#' pop_table <- tableParameterEstimates()
#' style_tfl(pop_table, color = c("RSE"))
#'
#' # Wide format with IIV and CV%
#' pop_table_wide <- tableParameterEstimates(
#'   useWideFormat = TRUE,
#'   includeIIV = TRUE,
#'   includeCV = TRUE)
#' style_tfl(pop_table_wide, color = c("RSE"), theme_fn = theme_booktabs)
#'
#' # With custom aliases and units
#' pop_table <- tableParameterEstimates(
#'   customAliases = c(Cl_pop = "Clearance", V_pop = "Volume", ka_pop = "Absorption rate"),
#'   customUnits = c(Cl_pop = "L/h", V_pop = "L", ka_pop = "1/h"),
#'   includeCI = TRUE,
#'   includeRSE = TRUE)
#'
#' # Exclude IOV parameters
#' pop_table_no_iov <- tableParameterEstimates(includeIOV = FALSE)
#'
#' # Simple column names (no pretty formatting)
#' pop_table_simple <- tableParameterEstimates(prettyLabels = FALSE)
#' }
tableParameterEstimates <- function(projectFile         = NULL,
																		customAliases      = NULL,
																		customUnits        = NULL,
																		customDescriptions = NULL,
																		includeRSE         = TRUE,
																		includeIIV         = TRUE,
																		includeIOV         = TRUE,
																		includeCI          = FALSE,
																		includeCV          = FALSE,
																		includeFEcategory  = FALSE,
																		includeBootstrap   = FALSE,
																		useWideFormat     = FALSE,
																		stdErrMethod      = "",
																		roundDigits         = 3,
																		roundMethod      = c("monolix", "signif", "none"),
																		prettyLabels        = TRUE) {

	roundMethod <- match.arg(roundMethod)

	.checkProjectFile(projectFile, "monolix")
	.ensureProjectLoaded(projectFile)

	# Validate availability of analysis results
	.validateProjectResultsAvailability(software = "monolix", resultType = "pop")

	pop <- data.frame(lixoftConnectors::getEstimatedPopulationParameters(coefficientsOfVariation = includeCV,
																																			 withValuesByGroups      = includeFEcategory))
	names(pop) <- c("Estimate")
	pop$Parameter <- rownames(pop)

	err <- NULL

	if (includeRSE) {
		# Validate RSE availability with method
		.validateProjectResultsAvailability(software = "monolix", resultType = "rse", method = stdErrMethod)
		err <- lixoftConnectors::getEstimatedStandardErrors(method = stdErrMethod, withValuesByGroups = includeFEcategory)
		if (length(err) == 0) {
			.TFL_message("Standard errors not available for method: ", stdErrMethod)
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

	if (includeCI) {
		# Validate RSE availability with method
		.validateProjectResultsAvailability(software = "monolix", resultType = "rse", method = stdErrMethod)

		interval_level <- lixoftConnectors::getStandardErrorEstimationSettings()$intervallevel

		# Calculate percentiles based on interval level
		lower_p     <- (100 - interval_level) / 2
		upper_p     <-  100 - lower_p
		lower_col   <- paste0("P", lower_p)
		upper_col   <- paste0("P", upper_p)
		ci_col_name <- paste0("CI_", lower_p, "_", upper_p)

		CI <- lixoftConnectors::getEstimatedConfidenceIntervals(method = stdErrMethod, withValuesByGroups = includeFEcategory)
		if (length(CI) == 0) {
			.TFL_message("Standard errors not available for method: ", stdErrMethod)
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
					lower_rounded = .round_num(.data[[lower_col]], digits = roundDigits, rounding_method = roundMethod),
					upper_rounded = .round_num(.data[[upper_col]], digits = roundDigits, rounding_method = roundMethod),
					!!ci_col_name := paste0("(", lower_rounded, "; ", upper_rounded, ")")) %>%
				dplyr::select(Parameter, !!ci_col_name)
		}
	}

	bootstrap <- NULL

	if (includeBootstrap) {
		# Validate RSE availability with method
		.validateProjectResultsAvailability(software = "monolix", resultType = "bootstrap", method = stdErrMethod)

		boot_interval_level <- getBootstrapSettings()$level

		# Calculate percentiles based on interval level
		boot_lower_p     <- (100 - boot_interval_level) / 2
		boot_upper_p     <-  100 - boot_lower_p
		boot_lower_col   <- paste0("P", boot_lower_p)
		boot_upper_col   <- paste0("P", boot_upper_p)
		boot_ci_col_name <- paste0("Boot_CI_", boot_lower_p, "_", boot_upper_p)

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
					boot_lower_rounded = .round_num(.data[[boot_lower_col]], digits = roundDigits, rounding_method = roundMethod),
					boot_upper_rounded = .round_num(.data[[boot_upper_col]], digits = roundDigits, rounding_method = roundMethod),
					Boot_Median        = .round_num(MEDIAN,                  digits = roundDigits, rounding_method = roundMethod),
					!!boot_ci_col_name := paste0("(", boot_lower_rounded, "; ", boot_upper_rounded, ")"))

			if (has_rse) {
				bootstrap <- bootstrap %>% dplyr::mutate(Boot_RSE = .round_num(`RSE(%)`, digits = roundDigits, rounding_method = roundMethod))
			}
			bootstrap <- bootstrap %>% dplyr::select(dplyr::all_of(final_cols))
		}
	}

	# Get parameters WITHOUT fixed effects by category
	params_no_FEcategory <- names(lixoftConnectors::getEstimatedPopulationParameters(coefficientsOfVariation = includeCV, withValuesByGroups = FALSE))
	# Set difference gives the category-specific FE parameters
	FE_category_params <- setdiff(pop$Parameter, params_no_FEcategory)


	# IOV variability parameters are discovered dynamically from the individual
	# parameter model so that user-defined parameters whose names happen to start
	# with "gamma_" (e.g. a gamma_pop shape parameter) are not misclassified.
	iov_params <- .getIOVParameters()

	pop$Section <- ""
	pop$Section[grep("_pop|beta_", pop$Parameter)]     <- "FE"
	pop$Section[grep("omega_",     pop$Parameter)]     <- "IIV"
	pop$Section[grep("^corr_",     pop$Parameter)]     <- "CORR"
	pop$Section[grep("^corr1_",    pop$Parameter)]     <- "CORR_IIV"
	pop$Section[pop$Parameter %in% iov_params]         <- "IOV"
	pop$Section[grep("^corr2_",    pop$Parameter)]     <- "CORR_IOV"
	# Residual error parameters are discovered dynamically from the continuous
	# observation model so that multi-output projects (a1, b1, a2, b2, ...) and
	# autocorrelated error models (extra r parameter) are classified correctly.
	res_err_params <- .getResidualErrorParameters()
	pop$Section[pop$Parameter %in% res_err_params]     <- "RES_ERR"
	pop$Section[grep("plcat_",     pop$Parameter)]     <- "LAT"
	pop$Section[pop$Parameter %in% FE_category_params] <- "FE_CAT"

	# Add CV column in long format if requested (only when NOT using wide format)
	# Must be done before RSE join to get correct column order: Estimate -> CV -> RSE -> CI
	if (includeCV && !useWideFormat) {
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
	if (includeCI) {
		pop <- dplyr::left_join(pop, CI, by = "Parameter")
	}

	if (includeRSE) {
		pop <- dplyr::left_join(pop, err, by = "Parameter")
	}

	if (includeBootstrap) {
		pop <- dplyr::left_join(pop, bootstrap, by = "Parameter")
	}
	if (useWideFormat) {
		# Rename fixed effect columns using hierarchical naming with underscore separator
		names(pop)[names(pop) == "Estimate"] <- "FE_Estimate"
		if (includeRSE && "RSE" %in% names(pop)) {
			names(pop)[names(pop) == "RSE"] <- "FE_RSE"
		}
		if (includeCI && ci_col_name %in% names(pop)) {
			names(pop)[names(pop) == ci_col_name] <- paste0("FE_",ci_col_name)
		}
		# if (includeBootstrap && boot_ci_col_name %in% names(pop)) {
		#   names(pop)[names(pop) == boot_ci_col_name] <- boot_ci_col_name # nothing to do
		# }

		pop_idx      <- grep("_pop", pop$Parameter)
		omega_idx    <- grep("omega_", pop$Parameter)
		omega_cv_idx <- NULL

		pop_root   <- sub("_pop$", "", pop$Parameter[pop_idx])
		omega_root <- sub("^omega_", "", pop$Parameter[omega_idx])

		if (includeCV) {
			omega_cv_idx <- omega_idx[endsWith(pop$Parameter[omega_idx], "_CV")]
			omega_idx    <- setdiff(omega_idx, omega_cv_idx)
			omega_root   <- sub("^omega_", "", pop$Parameter[omega_idx]) # to not duplicate
		}

		# Add IIV columns if requested (default TRUE)
		if (includeIIV && length(omega_idx) > 0) {
			pop[["IIV_SD"]] <- NA_real_
			pop[pop_idx, "IIV_SD"] <- pop[omega_idx[match(pop_root, omega_root)], "FE_Estimate"]

			# Add IIV CV column right after SD
			if (includeCV && length(omega_cv_idx) > 0) {
				pop[["IIV_CV"]] <- NA_real_
				pop[pop_idx, "IIV_CV"] <- pop[omega_cv_idx[match(pop_root, omega_root)], "FE_Estimate"]
			}

			if (includeCI && paste0("FE_",ci_col_name) %in% names(pop)) {
				pop[[paste0("IIV_",ci_col_name)]] <- NA_character_
				pop[pop_idx, paste0("IIV_",ci_col_name)] <- pop[omega_idx[match(pop_root, omega_root)], paste0("FE_",ci_col_name)]
			}
			if (includeRSE && "FE_RSE" %in% names(pop)) {
				pop[["IIV_RSE"]] <- NA_real_
				pop[pop_idx, "IIV_RSE"] <- pop[omega_idx[match(pop_root, omega_root)], "FE_RSE"]
			}

			# Bootstrap IIV columns (same pattern/logic as regular IIV)
			if (includeBootstrap) {
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
		# Match against iov_params (and their _CV variants) rather than grepping
		# "gamma_", to avoid catching user-defined parameters with that prefix.
		gamma_idx    <- which(pop$Parameter %in% c(iov_params, paste0(iov_params, "_CV")))
		gamma_cv_idx <- NULL

		if (includeIOV && length(gamma_idx) > 0) {
			gamma_root <- sub("^gamma_", "", pop$Parameter[gamma_idx])

			if (includeCV) {
				gamma_cv_idx <- gamma_idx[endsWith(pop$Parameter[gamma_idx], "_CV")]
				gamma_idx    <- setdiff(gamma_idx, gamma_cv_idx)
				gamma_root   <- sub("^gamma_", "", pop$Parameter[gamma_idx]) # to not duplicate
			}

			pop[["IOV_SD"]] <- NA_real_
			pop[pop_idx, "IOV_SD"] <- pop[gamma_idx[match(pop_root, gamma_root)], "FE_Estimate"]

			# Add IOV CV column right after SD
			if (includeCV && length(gamma_cv_idx) > 0) {
				pop[["IOV_CV"]] <- NA_real_
				pop[pop_idx, "IOV_CV"] <- pop[gamma_cv_idx[match(pop_root, gamma_root)], "FE_Estimate"]
			}

			if (includeRSE && "FE_RSE" %in% names(pop)) {
				pop[["IOV_RSE"]] <- NA_real_
				pop[pop_idx, "IOV_RSE"] <- pop[gamma_idx[match(pop_root, gamma_root)], "FE_RSE"]
			}

			if (includeCI && paste0("FE_",ci_col_name) %in% names(pop)) {
				pop[[paste0("IOV_",ci_col_name)]] <- NA_character_
				pop[pop_idx, paste0("IOV_",ci_col_name)] <- pop[gamma_idx[match(pop_root, gamma_root)], paste0("FE_",ci_col_name)]
			}
			# Bootstrap IOV columns (same as in bootstrap IIV)
			if (includeBootstrap) {
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
		col_order <- c("Section", "Parameter")

		# SAEM FE columns: Estimate -> CI -> RSE
		col_order <- c(col_order, "FE_Estimate")
		if (includeCI && !is.null(ci_col_name)) {
			fe_ci_col <- paste0("FE_", ci_col_name)
			if (fe_ci_col %in% names(pop)) col_order <- c(col_order, fe_ci_col)
		}
		if (includeRSE && "FE_RSE" %in% names(pop)) {
			col_order <- c(col_order, "FE_RSE")
		}

		# SAEM IIV columns: SD -> CV -> CI -> RSE
		if (includeIIV && "IIV_SD" %in% names(pop)) {
			col_order <- c(col_order, "IIV_SD")
			if (includeCV && "IIV_CV" %in% names(pop)) col_order <- c(col_order, "IIV_CV")
			if (includeCI && !is.null(ci_col_name)) {
				iiv_ci_col <- paste0("IIV_", ci_col_name)
				if (iiv_ci_col %in% names(pop)) col_order <- c(col_order, iiv_ci_col)
			}
			if (includeRSE && "IIV_RSE" %in% names(pop)) col_order <- c(col_order, "IIV_RSE")
		}

		# SAEM IOV columns: SD -> CV -> CI -> RSE
		if (includeIOV && "IOV_SD" %in% names(pop)) {
			col_order <- c(col_order, "IOV_SD")
			if (includeCV && "IOV_CV" %in% names(pop)) col_order <- c(col_order, "IOV_CV")
			if (includeCI && !is.null(ci_col_name)) {
				iov_ci_col <- paste0("IOV_", ci_col_name)
				if (iov_ci_col %in% names(pop)) col_order <- c(col_order, iov_ci_col)
			}
			if (includeRSE && "IOV_RSE" %in% names(pop)) col_order <- c(col_order, "IOV_RSE")
		}

		# Bootstrap FE columns: Median -> CI -> RSE
		if (includeBootstrap && "Boot_Median" %in% names(pop)) {
			col_order <- c(col_order, "Boot_Median")
			if (!is.null(boot_ci_col_name) && boot_ci_col_name %in% names(pop)) {
				col_order <- c(col_order, boot_ci_col_name)
			}
			if ("Boot_RSE" %in% names(pop)) col_order <- c(col_order, "Boot_RSE")
		}

		# Bootstrap IIV columns: Median -> CI -> RSE
		if (includeBootstrap && includeIIV && "Boot_IIV_Median" %in% names(pop)) {
			col_order <- c(col_order, "Boot_IIV_Median")
			if (!is.null(boot_ci_col_name)) {
				boot_iiv_ci_col <- sub("^Boot_CI", "Boot_IIV_CI", boot_ci_col_name)
				if (boot_iiv_ci_col %in% names(pop)) col_order <- c(col_order, boot_iiv_ci_col)
			}
			if ("Boot_IIV_RSE" %in% names(pop)) col_order <- c(col_order, "Boot_IIV_RSE")
		}

		# Bootstrap IOV columns: Median -> CI -> RSE
		if (includeBootstrap && includeIOV && "Boot_IOV_Median" %in% names(pop)) {
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

	#---------------------------------------------------------------------------
	# Merge customDescriptions BEFORE aliases/units
	descriptions_df <- .normalizeAliasMapping(customDescriptions, "description")
	if (!is.null(descriptions_df)) {
		desc_map <- setNames(descriptions_df$description, descriptions_df$parameter)
		pop$Description <- unname(desc_map[pop$Parameter])

		unmatched_keys <- setdiff(names(desc_map), pop$Parameter)
		if (length(unmatched_keys) > 0) {
			.TFL_message(
				"customDescriptions: the following keys did not match any row ",
				"in the output table and were ignored: ",
				paste(unmatched_keys, collapse = ", "),
				if (useWideFormat) " (note: in wide format, IIV/IOV rows are collapsed)." else "."
			)
		}
	}

	# Apply custom aliases and units directly to Parameter column
	aliases_df <- .normalizeAliasMapping(customAliases, "aliases")
	units_df   <- .normalizeAliasMapping(customUnits, "units")
	if (!is.null(aliases_df) || !is.null(units_df)) {
		pop <- .applyCustomAliasesUnits(pop,
																		param_col  = "Parameter",
																		aliases_df = aliases_df,
																		units_df   = units_df)
	}

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
	if (!useWideFormat) {
		pop <- dplyr::relocate(pop, "Parameter", .after = "Section")
	}
	if ("Description" %in% names(pop)) {
		pop <- dplyr::relocate(pop, "Description", .after = "Parameter")
	}
	pop <- .round_num(pop, roundDigits, exclude = c("Section", "Parameter", "Description"), rounding_method = roundMethod)

	# Apply pretty labels if requested for nice column names display
	if (prettyLabels) {
		pop <- .prettyLabels(pop)
	}

	# for later identfication in flextable function
	attr(pop, "table_type")  <- "pop"
	attr(pop, "format")      <- if (useWideFormat) "wide" else "long"
	attr(pop, "includeIIV") <- useWideFormat && includeIIV
	attr(pop, "includeIOV") <- useWideFormat && includeIOV
	attr(pop, "project_source") <- lixoftConnectors::getProjectSettings()$directory
	attr(pop, "r_source")       <- .getRScriptPath()
	return(pop)

}
