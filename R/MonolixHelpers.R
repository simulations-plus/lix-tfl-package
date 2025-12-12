#' Title
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
			dfall$BICclin[i] = round(dfLL$linearization[dfLL$criteria == 'BICc'], digits =
															 	1)
			dfall$LLlin[i] = round(dfLL$linearization[dfLL$criteria == 'OFV'], digits =
														 	1)
		}
		if (!is.null(dfLL$importanceSampling)) {
			dfall$BICcis[i] = round(dfLL$importanceSampling[dfLL$criteria == 'BICc'], digits =
																1)
			dfall$LLis[i] = round(dfLL$importanceSampling[dfLL$criteria == 'OFV'], digits =
															1)
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

	return(dfall)

}
