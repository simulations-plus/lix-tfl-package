#' Internal helper: Derive omega population-parameter name from an eta column
#'
#' Strips the leading "eta_" and, for simulated estimates with a specific
#' variabilityLevel, the trailing "_<level>" suffix, then prepends "omega_".
#'
#' @param param Single eta column name (e.g. "eta_V", "eta_Cl_ID")
#' @param settings List with indivEstimate and variabilityLevel
#' @return Character scalar omega name (e.g. "omega_V")
#' @keywords internal
.omegaNameForEta <- function(param, settings) {
	param_suffix <- sub("^eta_", "", param)
	if (settings$indivEstimate == "simulated" && !is.null(settings$variabilityLevel)) {
		suffix_pattern <- paste0("_", settings$variabilityLevel, "$")
		param_suffix <- sub(suffix_pattern, "", param_suffix)
	}
	paste0("omega_", param_suffix)
}

#' Internal helper: Prepare QQ plot data for standardized random effects
#'
#' @param parameters Character vector of parameter names
#' @param settings List with indivEstimate, level, and variabilityLevel
#' @return List with plot_data (list per parameter) and metadata
#' @keywords internal
.prepareQQDataStandardizedRandomEffects <- function(parameters, settings, stratify = list(), parsed = NULL) {

	#-----------------------------------------------------------------------------
	# Get all variability levels from the project
	available_levels <- lixoftConnectors::getVariabilityLevels()
	occ_vars <- setdiff(available_levels, "ID")
	has_occasions <- length(occ_vars) > 0

	#-----------------------------------------------------------------------------
	# Retrieve random effects based on indivEstimate type
	if (settings$indivEstimate %in% c("mode", "mean")) {
		# For mode/mean: use conditional estimates (only combined ID + OCC available)
		rand_eff_all <- lixoftConnectors::getEstimatedRandomEffects()

		slot <- if (settings$indivEstimate == "mode") "conditionalMode" else "conditionalMean"

		if (is.null(rand_eff_all[[slot]])) {
			stop(sprintf(
				"Individual estimate '%s' (slot '%s') is not available in the project results.",
				settings$indivEstimate, slot))
		}
		rand_eff_df <- rand_eff_all[[slot]]

		# Warn if variabilityLevel is set (not applicable for mode/mean)
		if (!is.null(settings$variabilityLevel)) {
			warning(sprintf(
				"variabilityLevel is not applicable for indivEstimate = '%s'. Only combined ID + OCC is available.",
				settings$indivEstimate))
		}

		# Exclude id and occasion columns from parameter selection
		exclude_cols <- c("id", occ_vars)

	} else if (settings$indivEstimate == "simulated") {
		# For simulated: use simulated random effects (ID, OCC, or combined available)
		rand_eff_df <- lixoftConnectors::getSimulatedRandomEffects()

		# Validate variability level if specified
		if (!is.null(settings$variabilityLevel) &&
				!settings$variabilityLevel %in% available_levels) {
			stop(sprintf(
				"Invalid variabilityLevel: '%s'. Available levels: %s",
				settings$variabilityLevel,
				paste(available_levels, collapse = ", ")))
		}

		# Determine parameter suffix based on variability level
		if (is.null(settings$variabilityLevel)) {
			# Default: combined ID + OCC (no suffix: eta_ka, eta_V, eta_Cl)
			param_suffix <- ""
			exclude_cols <- c("rep", "id", occ_vars)
		} else if (settings$variabilityLevel == "ID") {
			# ID level only (suffix: _ID)
			param_suffix <- "_ID"
			exclude_cols <- c("rep", "id", occ_vars)
		} else {
			# OCC level only (suffix: _OCCevid or other occ name)
			param_suffix <- paste0("_", settings$variabilityLevel)
			exclude_cols <- c("rep", "id", occ_vars)
		}

	} else {
		stop(sprintf(
			"Invalid indivEstimate: '%s'. Must be 'mode', 'mean', or 'simulated'.",
			settings$indivEstimate))
	}

	#-----------------------------------------------------------------------------
	# Select parameters
	eta_cols <- setdiff(names(rand_eff_df), exclude_cols)

	# For simulated with specific variability level, filter to appropriate suffix
	if (settings$indivEstimate == "simulated") {
		if (!is.null(settings$variabilityLevel)) {
			if (settings$variabilityLevel == "ID") {
				# Select only _ID columns
				eta_cols <- grep("_ID$", eta_cols, value = TRUE)
			} else if (settings$variabilityLevel %in% occ_vars) {
				# Select only _OCCevid (or other occ name) columns
				pattern <- paste0("_", settings$variabilityLevel, "$")
				eta_cols <- grep(pattern, eta_cols, value = TRUE)
			}
		} else {
			# Default: exclude columns with _ID or _<occ_var> suffixes
			# Build pattern dynamically based on actual occasion variables
			suffix_pattern <- paste0("_(ID|", paste(occ_vars, collapse = "|"), ")$")
			eta_cols <- grep(suffix_pattern, eta_cols, value = TRUE, invert = TRUE)
		}
	}

	#-----------------------------------------------------------------------------
	# Handle parameter names
	# Users provide base names (ka, Cl) but columns are named eta_ka, eta_Cl
	# For simulated with variabilityLevel, also need suffix (eta_ka_ID, etc.)
	if (!is.null(parameters)) {
		# Add eta_ prefix if not already present
		parameters_with_eta <- ifelse(grepl("^eta_", parameters),
																	parameters,
																	paste0("eta_", parameters))

		# For simulated with specific variability level, add suffix
		if (settings$indivEstimate == "simulated" &&
				!is.null(settings$variabilityLevel)) {
			# Add variability level suffix (_ID or _OCCevid)
			suffix <- paste0("_", settings$variabilityLevel)
			# Only add suffix if not already present
			parameters_with_eta <- ifelse(grepl(paste0(suffix, "$"), parameters_with_eta),
																		parameters_with_eta,
																		paste0(parameters_with_eta, suffix))
		}

		# Validate parameters exist
		missing_params <- setdiff(parameters_with_eta, eta_cols)
		if (length(missing_params) > 0) {
			stop(sprintf(
				"The following parameters were not found in the random effects results: %s.",
				paste(missing_params, collapse = ", ")))
		}

		parameters <- parameters_with_eta
	} else {
		# Use all available parameters
		parameters <- eta_cols
	}

	#-----------------------------------------------------------------------------
	# Stratification: add split/color columns per id
	if (is.null(parsed)) parsed <- .parseStratify(stratify)
	has_stratify <- !is.null(parsed$split_specs) || !is.null(parsed$color_specs) ||
		!is.null(parsed$filter_specs) || !is.null(parsed$ids)

	if (has_stratify) {
		# Apply filter by group index/category
		if (!is.null(parsed$filter_specs)) {
			rand_eff_df <- .applyStratifyFilter(
				rand_eff_df, parsed$filter_specs, stratify$groups)
		}

		# Apply individual selection
		if (!is.null(parsed$ids)) {
			rand_eff_df <- rand_eff_df[rand_eff_df$id %in% parsed$ids, ]
		}

		# Get split per id
		covDataSplit <- .stratifyCovariates(split = parsed$split_specs, filter = NULL)

		# Get color per id
		covDataColor <- .stratifyCovariates(split = parsed$color_specs, filter = NULL, splitbyid = TRUE)
		covDataColor <- .renameColumns(covDataColor, "split", "color")

		covData <- merge(covDataSplit, covDataColor)

		# Merge split and color onto rand_eff_df
		rand_eff_df <- merge(rand_eff_df, covData[, c("id", "split", "color")], by = "id")

		# Ensure factor and drop levels with no remaining observations (e.g. filtered-out groups)
		rand_eff_df$split <- droplevels(factor(rand_eff_df$split))
		rand_eff_df$color <- droplevels(factor(rand_eff_df$color))
	} else {
		rand_eff_df$split <- factor("All")
		rand_eff_df$color <- factor("All")
	}

	#-----------------------------------------------------------------------------
	# Retrieve population parameters omegas to standardize random effects
	pop_params <- getEstimatedPopulationParameters()

	# Skip parameters whose omega is absent (no between-subject variability on
	# that parameter). Keeps the plot useful for models where only a subset of
	# parameters has random effects.
	omega_names <- vapply(parameters, .omegaNameForEta, character(1),
												settings = settings)
	has_omega <- omega_names %in% names(pop_params) &
		!is.na(pop_params[omega_names])
	if (any(!has_omega)) {
		skipped <- sub("^eta_", "", parameters[!has_omega])
		message(sprintf(
			"QQ plot unavailable for parameter(s) without random effects: %s.",
			paste(skipped, collapse = ", ")))
		parameters <- parameters[has_omega]
	}
	if (length(parameters) == 0) {
		stop("No random effects with an estimated omega were found; nothing to plot.")
	}

	alpha <- 1 - settings$level

	# Determine grouping variable for QQ computation:
	# mergedSplits -> group by color (per-group CIs on same panel)
	# otherwise    -> group by split (separate panels)
	mergedSplits <- parsed$mergedSplits
	if (mergedSplits) {
		qq_group_col <- "color"
	} else {
		qq_group_col <- "split"
	}
	qq_group_levels <- levels(rand_eff_df[[qq_group_col]])
	if (is.null(qq_group_levels)) qq_group_levels <- unique(rand_eff_df[[qq_group_col]])

	#-----------------------------------------------------------------------------
	# Build data list (one data frame per parameter x split group)
	plot_data_list <- lapply(parameters, function(param) {

		omega  <- pop_params[.omegaNameForEta(param, settings)]
		std_re <- rand_eff_df[[param]] / sqrt(omega)

		# Compute QQ statistics per group (split or color when mergedSplits)
		per_group <- lapply(qq_group_levels, function(g) {
			mask <- rand_eff_df[[qq_group_col]] == g
			std_re_group <- std_re[mask]
			split_group  <- rand_eff_df$split[mask]
			color_group  <- rand_eff_df$color[mask]

			n_group <- length(std_re_group)
			if (n_group < 2) {
				warning(sprintf(
					"Group '%s' for parameter '%s' has fewer than 2 observations. Skipping.",
					g, param), call. = FALSE)
				return(NULL)
			}

			# Sort and carry split/color through
			ord          <- order(std_re_group)
			sorted_re    <- std_re_group[ord]
			sorted_split <- split_group[ord]
			sorted_color <- color_group[ord]

			# Blom plotting positions for this group's n
			p_group       <- ((1:n_group) - (3 / 8)) / (n_group + (1 / 4))
			theoretical_q <- qnorm(p_group)

			# CI bounds for this group's n
			lower_beta <- qbeta(alpha / 2,     1:n_group, n_group:1)
			upper_beta <- qbeta(1 - alpha / 2, 1:n_group, n_group:1)

			data.frame(
				parameter     = param,
				split         = sorted_split,
				color         = sorted_color,
				theoretical_q = theoretical_q,
				observed_q    = sorted_re,
				ci_lower      = qnorm(lower_beta),
				ci_upper      = qnorm(upper_beta),
				stringsAsFactors = FALSE)
		})

		do.call(rbind, per_group)
	})

	names(plot_data_list) <- parameters

	return(list(
		plot_data_list = plot_data_list,
		parameters     = parameters))
}


#' QQ plot of standardized random effects
#'
#' @param parameters Character vector of base parameter names (e.g., "ka", "Cl").
#'   If NULL, all available parameters are used.
#' @param settings List of plot settings:
#'   - indivEstimate: "mean" (default), "mode", or "simulated"
#'   - identityLine: logical, show identity line (default TRUE)
#'   - confidenceInterval: logical, show CI band (default TRUE)
#'   - level: CI level, e.g. 0.90 (default)
#'   - variabilityLevel: In case of IOV, specifies which level to plot
#'     (default NULL = ID + Occasions)
#'   - footnote: logical, add a footnote below the plot (and legend) with the
#'     Monolix project directory and R script path, styled like the table
#'     footers (small, italic, left-aligned) (default FALSE)
#'   - ncol: number of columns in facet layout (default 2)
#' @param stratify List with the stratification arguments:
#'   \describe{
#'     \item{groups}{List of custom group definitions. Each element is a list with:
#'       \code{name} (covariate name) and \code{definition} (for continuous: vector
#'       of break values, e.g. \code{c(35, 65)}; for categorical: list of category
#'       vectors, e.g. \code{list(c("study101"), c("study201","study202"))})}
#'     \item{split}{Character vector of covariate names used to facet the plot,
#'       e.g. \code{c("FORM","AGE")}}
#'     \item{color}{Character vector of covariate names used for coloring,
#'       e.g. \code{c("SEX")}}
#'     \item{colors}{Character vector of colors to use, e.g.
#'       \code{c("#2BB9DB","#DD6BD2")}. Takes precedence over defaults.}
#'     \item{mergedSplits}{Logical. When TRUE and \code{split} is used, split
#'       groups are displayed on the same plot with different colors instead of
#'       separate subplots. The \code{color} argument is ignored. Default FALSE.}
#'     \item{filter}{List of pairs: covariate name and vector of group indices
#'       (continuous) or category names (categorical) to keep,
#'       e.g. \code{list(list("AGE", c(1,3)), list("FORM", "ref"))}}
#'     \item{individualSelection}{List with \code{ids}: character vector of
#'       individual ids to display, e.g. \code{list(ids = c("101-01","101-02"))}}
#'   }
#' @param preferences List of visual preferences:
#'   - point: list(color = "#4682B4", size = 1.5)
#'   - identityLine: list(color = "#000000", linewidth = 0.8)
#'   - confidenceInterval: list(color = "#000000", linewidth = 0.825, linetype = "dashed")
#' @return A ggplot object
plotQQStandardizedRandomEffects <- function(parameters  = NULL,
																						settings    = list(indivEstimate      = "mean",
																															 identityLine       = TRUE,
																															 confidenceInterval = TRUE,
																															 legend             = TRUE,
																															 grid               = TRUE,
																															 level              = 0.90,
																															 variabilityLevel   = NULL,
																															 footnote           = FALSE,
																															 ncol               = 2),
																						stratify    = list(),
																						preferences = list(point = list(color = "#4682B4",
																																						size  = 1.5),
																															 identityLine = list(color     = "#000000",
																															 										linewidth = 0.7,
																															 										label     = "y = x line"),
																															 confidenceInterval = list(color     = "#000000",
																															 													linewidth = 0.5,
																															 													linetype  = "dashed",
																															 													label     = "CI"))) {

	# Merge user-supplied values over defaults (protects against partial lists)
	default_settings    <- formals()$settings
	default_preferences <- formals()$preferences

	settings    <- modifyList(eval(default_settings), settings)

	preferences <- modifyList(eval(default_preferences), preferences)
	preferences$point              <- modifyList(eval(default_preferences)$point,
																							 if (is.null(preferences$point)) list() else preferences$point)
	preferences$identityLine       <- modifyList(eval(default_preferences)$identityLine,
																							 if (is.null(preferences$identityLine)) list() else preferences$identityLine)
	preferences$confidenceInterval <- modifyList(eval(default_preferences)$confidenceInterval,
																							 if (is.null(preferences$confidenceInterval)) list() else preferences$confidenceInterval)

	# Debugging: Uncomment to test with fixed values
	# parameters = c("Cl", "V")
	# settings <- list(
	#   indivEstimate      = "simulated",
	#   identityLine       = TRUE,
	#   confidenceInterval = TRUE,
	#   legend             = TRUE,
	#   grid               = TRUE,
	#   level              = 0.95,
	#   variabilityLevel   = "OCCevid",
	#   ncol               = 2)
	#
	# preferences <- list(
	#   point = list(
	#     color = "#4682B4",
	#     size  = 1.5),
	#   identityLine = list(
	#     color     = "#000000",
	#     linewidth = 0.7,
	#     label     = "Identity"),
	#   confidenceInterval = list(
	#     color     = "#000000",
	#     linewidth = 0.725,
	#     linetype  = "dashed",
	#     label     = "CI"))
	#
	# stratify <- list()

	#-----------------------------------------------------------------------------
	# Input validation
	valid_estimates <- c("mean", "mode", "simulated")
	if (!settings$indivEstimate %in% valid_estimates) {
		stop(sprintf(
			"Invalid `indivEstimate`: '%s'. Must be one of: %s.",
			settings$indivEstimate,
			paste(valid_estimates, collapse = ", ")))
	}
	if (!is.numeric(settings$level) || settings$level <= 0 || settings$level >= 1) {
		stop("`level` must be a number strictly between 0 and 1, e.g. 0.90.")
	}

	# Validate variabilityLevel compatibility with indivEstimate
	if (settings$indivEstimate %in% c("mean", "mode") &&
			!is.null(settings$variabilityLevel)) {
		warning(sprintf(
			"variabilityLevel '%s' is not applicable for indivEstimate = '%s'. Only combined ID + OCC is available. Setting variabilityLevel to NULL.",
			settings$variabilityLevel,
			settings$indivEstimate))
		settings$variabilityLevel <- NULL
	}

	#-----------------------------------------------------------------------------
	# Parse stratification arguments
	parsed <- .parseStratify(stratify)

	# Prepare data using internal helper
	qq_data <- .prepareQQDataStandardizedRandomEffects(parameters, settings, stratify, parsed)

	#-----------------------------------------------------------------------------
	# Combine data from all parameters into single long data frame for faceting
	plot_data <- do.call(rbind, qq_data$plot_data_list)

	has_split <- length(unique(plot_data$split)) > 1
	has_color_strat <- length(unique(plot_data$color)) > 1

	#-----------------------------------------------------------------------------
	# Build legend configuration (only for visible elements)
	legend_linetypes <- c()

	if (settings$identityLine) {
		legend_linetypes[preferences$identityLine$label] <- "solid"
	}
	if (settings$confidenceInterval) {
		legend_linetypes[preferences$confidenceInterval$label] <- preferences$confidenceInterval$linetype
	}

	# Build override linewidths and linetypes for legend
	override_linewidths <- c()
	override_linetypes  <- c()
	if (settings$identityLine) {
		override_linewidths <- c(override_linewidths,
														 preferences$identityLine$linewidth)
		override_linetypes  <- c(override_linetypes, "solid")
	}
	if (settings$confidenceInterval) {
		override_linewidths <- c(override_linewidths,
														 preferences$confidenceInterval$linewidth)
		override_linetypes  <- c(override_linetypes,
														 preferences$confidenceInterval$linetype)
	}

	#-----------------------------------------------------------------------------
	# Create plot
	base_theme <- if (settings$grid) theme_minimal() else theme_classic()

	p <- ggplot(plot_data, aes(x = theoretical_q, y = observed_q))

	# Points: use color aesthetic when color stratification is active
	if (has_color_strat) {
		p <- p +
			geom_point(aes(colour = color, shape = "Empirical quantiles"),
								 size = preferences$point$size)
	} else {
		p <- p +
			geom_point(aes(shape = "Empirical quantiles"),
								 color = preferences$point$color,
								 size  = preferences$point$size)
	}

	# Faceting: use facet_grid when split is active, otherwise facet_wrap by parameter
	if (has_split) {
		p <- p + facet_grid(split ~ parameter, scales = "free",
												axes = "all", axis.labels = "all_y")
	} else {
		p <- p + facet_wrap(~ parameter, ncol = settings$ncol, scales = "free",
												strip.position = "bottom",
												axes = "all", axis.labels = "all_y")
	}

	p <- p +
		labs(x        = "Theoretical quantiles",
				 y        = "Emprirical quantiles of\nstandardized random effects",
				 colour   = NULL,
				 linetype = NULL,
				 shape    = NULL) +
		base_theme +
		theme(legend.position  = if (settings$legend) "bottom" else "none",
					legend.direction = "vertical",
					axis.line   = element_line(color = "black"),
					axis.ticks  = element_line(color = "black"),
					strip.placement = "outside",
					strip.background = element_blank())

	# Add identity line
	if (settings$identityLine) {
		p <- p + geom_abline(
			aes(intercept = 0, slope = 1,
					linetype = preferences$identityLine$label),
			color     = preferences$identityLine$color,
			linewidth = preferences$identityLine$linewidth,
			key_glyph = "path")
	}

	# Add confidence interval lines
	if (settings$confidenceInterval) {
		if (parsed$mergedSplits && has_color_strat) {
			# mergedSplits: per-group CIs colored to match their group
			p <- p +
				geom_line(aes(y        = ci_lower,
											colour   = color,
											linetype = preferences$confidenceInterval$label),
									linewidth = preferences$confidenceInterval$linewidth) +
				geom_line(aes(y        = ci_upper,
											colour   = color,
											linetype = preferences$confidenceInterval$label),
									linewidth = preferences$confidenceInterval$linewidth)
		} else {
			p <- p +
				geom_line(aes(y        = ci_lower,
											linetype = preferences$confidenceInterval$label),
									color     = preferences$confidenceInterval$color,
									linewidth = preferences$confidenceInterval$linewidth) +
				geom_line(aes(y        = ci_upper,
											linetype = preferences$confidenceInterval$label),
									color     = preferences$confidenceInterval$color,
									linewidth = preferences$confidenceInterval$linewidth)
		}
	}

	# Apply scales
	p <- p +
		scale_shape_manual(
			values = c("Empirical quantiles" = 16),
			name   = NULL,
			guide  = guide_legend(order = 1))

	if (length(legend_linetypes) > 0) {
		p <- p +
			scale_linetype_manual(
				values = legend_linetypes,
				breaks = names(legend_linetypes),
				name   = NULL,
				guide  = guide_legend(
					order = 2,
					override.aes = list(
						linewidth = unname(override_linewidths),
						linetype  = unname(override_linetypes))))
	}

	if (has_color_strat) {
		strat_colors <- stratify$colors
		if (!is.null(strat_colors) && length(strat_colors) >= length(unique(plot_data$color))) {
			p <- p + scale_colour_manual(values = strat_colors, name = NULL)
		} else {
			p <- p + scale_colour_discrete(name = NULL)
		}
	}

	# optional source footnote below the plot (and below the legend), styled like
	# the flextable footers (small, italic, left-aligned). Uses plot.caption so it
	# stays at the bottom even if the legend is later moved (e.g. to the right).
	if (isTRUE(settings$footnote)) {
		proj <- tryCatch(lixoftConnectors::getProjectSettings()$directory,
										 error = function(e) NULL)
		rsrc <- tryCatch(.getRScriptPath(), error = function(e) NULL)
		lines <- c(if (!is.null(proj) && nzchar(proj)) paste0("Monolix project: ", proj),
							 if (!is.null(rsrc) && nzchar(rsrc)) paste0("R source: ", rsrc))
		if (length(lines))
			p <- p +
				labs(caption = paste(lines, collapse = "\n")) +
				theme(plot.caption = element_text(size = 7, face = "italic", hjust = 0))
	}

	return(p)
}
################################################################################
`%||%` <- function(a, b) if (is.null(a)) b else a

# Evaluate `expr` under a fixed seed without perturbing the caller's global RNG
# stream. set.seed() overwrites .Random.seed for the whole session, so we save
# and restore it (or remove it if it did not exist) on exit.
.withSeed <- function(seed, expr) {
	if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
		old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
		on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
	} else {
		on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())), add = TRUE)
	}
	set.seed(seed)
	expr
}

# Half-normal quantile: p-th quantile of |Z|, Z ~ N(0,1)
.halfNormalQuantile <- function(p) stats::qnorm((1 + p) / 2)

# Ordinal string, e.g. 10 -> "10th", 21 -> "21st", 73 -> "73rd".
.ordinal <- function(n) {
	n   <- round(n)
	suf <- if ((n %% 100) %in% 11:13) "th"
	else c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")[(n %% 10) + 1]
	paste0(n, suf)
}

# Composite legend-key glyph: a full-width outer-band swatch with a centred
# median-band swatch on top, so the two prediction-interval bands collapse into
# a single legend entry (matching the Monolix GUI).
.makePiKeyGlyph <- function(outerFill, outerAlpha, medianFill, medianAlpha) {
	force(outerFill); force(outerAlpha); force(medianFill); force(medianAlpha)
	function(data, params, size) {
		grid::grobTree(
			grid::rectGrob(gp = grid::gpar(fill = outerFill,  col = NA, alpha = outerAlpha)),
			grid::rectGrob(width = 0.5,
										 gp = grid::gpar(fill = medianFill, col = NA, alpha = medianAlpha)))
	}
}


# =============================================================================
# Modular data-access layer

# Results directory of the currently loaded project ("remember where it lives").
.projectResultsDir <- function() {
	ps  <- lixoftConnectors::getProjectSettings()
	dir <- ps$directory %||% ps$resultDirectory   # field-name guard across versions
	if (is.null(dir) || !nzchar(dir))
		stop("Could not determine the project results directory. Is a project loaded?")
	dir
}

# Observation names of the loaded project, in order.
.observationNames <- function() {
	oi <- try(lixoftConnectors::getObservationInformation(), silent = TRUE)
	if (!inherits(oi, "try-error") && !is.null(oi$name)) return(oi$name)
	lixoftConnectors::getData()$observationNames
}

# Resolve obsName (NULL -> first observation); validate against the project.
.resolveObsName <- function(obsName = NULL) {
	nms <- .observationNames()
	if (is.null(obsName)) return(nms[1])
	if (!obsName %in% nms)
		stop("Unknown obsName '", obsName, "'. Available: ", paste(nms, collapse = ", "))
	obsName
}
################################################################################
# Path to the predictions file for one observation.
# Multi-output projects write per-output files "predictions_<output>_<obsName>.txt"
# (e.g. predictions_y1_totDrug.txt); a single modelled output writes plain
# "predictions.txt". We match by the "_<obsName>.txt" suffix, else fall back.
.predictionsPath <- function(obsName = NULL, resultsDir = .projectResultsDir()) {
	if (is.null(obsName)) obsName <- .resolveObsName(NULL)
	files <- list.files(resultsDir, pattern = "^predictions_.*\\.txt$")
	if (length(files)) {
		# File token is the model output + obs name (e.g. "y1_totDrug"), while the
		# observation name may be "1_totDrug"/"totDrug" -> match on suffix, then on
		# a plain substring as a fallback.
		hit <- files[endsWith(files, paste0(obsName, ".txt"))]
		if (!length(hit)) hit <- files[grepl(obsName, files, fixed = TRUE)]
		if (length(hit)) return(file.path(resultsDir, hit[1]))
	}
	# single modelled output (or multiple obsids but one modelled) -> predictions.txt
	path <- file.path(resultsDir, "predictions.txt")
	if (file.exists(path)) return(path)
	stop("No predictions file found for observation '", obsName, "' in ", resultsDir)
}
################################################################################
# Per-observation censoring (BLQ) flags. predictions.txt carries no BLQ flag,
# so we read it from the interpreted dataset and join it back by id + time
# (+ occasion column(s), so IOV rows sharing an id/time are disambiguated).
# Column roles come from getData() (header + headerTypes); the data itself comes
# from getInterpretedData(), whose `cens` column holds decoded labels
# ("notCensored", "intervalCensored", "leftCensored", ...). Empty = dose row.
# When several observations share the dataset (obsid column), rows are filtered
# to `obsName` so the join targets the matching predictions file.
# Returns data.frame(id, time, censored, loq, limit, <occ cols>) or NULL when
# there is no cens column. The occ columns are named as in predictions.txt.
.getCensoringInfo <- function(obsName) {
	gd    <- lixoftConnectors::getData()
	types <- gd$headerTypes
	if (!"cens" %in% types) return(NULL)
	hdr       <- gd$header
	id_col    <- hdr[types == "id"][1]
	time_col  <- hdr[types == "time"][1]
	cens_col  <- hdr[types == "cens"][1]
	obs_col   <- hdr[types == "observation"][1]                       # holds LOQ for BLQ rows
	limit_col <- if ("limit"  %in% types) hdr[types == "limit"][1]  else NA_character_
	obsid_col <- if ("obsid"  %in% types) hdr[types == "obsid"][1]  else NA_character_
	occ_cols  <- hdr[types == "occ"]                                  # occasion column(s), if any

	di <- lixoftConnectors::getInterpretedData()
	if (!is.na(obsid_col))                       # keep only this observation's rows
		di <- di[as.character(di[[obsid_col]]) == obsName, , drop = FALSE]
	cens <- as.character(di[[cens_col]])
	keep <- !is.na(cens) & nzchar(cens)          # observation rows only (not doses)
	res <- data.frame(id       = as.character(di[[id_col]][keep]),
										time     = as.numeric(di[[time_col]][keep]),
										censored = !(cens[keep] %in% c("notCensored")),
										loq      = suppressWarnings(as.numeric(di[[obs_col]][keep])),
										limit    = if (!is.na(limit_col))
											suppressWarnings(as.numeric(di[[limit_col]][keep])) else NA_real_,
										stringsAsFactors = FALSE)
	# carry occasion column(s) so the join can disambiguate IOV rows
	for (oc in intersect(occ_cols, names(di)))
		res[[oc]] <- as.character(di[[oc]][keep])
	res
}

################################################################################
# Load raw |IWRES| points for one observation model + indivEstimate.
# Returns: id, time, prediction, absiwres
.getAbsIWRESData <- function(obsName = NULL, settings) {
	est <- settings$indivEstimate
	if (!est %in% c("mode", "mean"))
		stop("indivEstimate must be 'mode' or 'mean'.")

	requested <- obsName
	nms       <- .observationNames()
	obsName   <- .resolveObsName(obsName)        # NULL -> first observation
	path      <- .predictionsPath(obsName)
	# tell the user which observation/file was picked when they didn't choose
	if (is.null(requested) && length(nms) > 1)
		message("plotAbsIWRESScatterPlot: multiple observations modelled; no obsName ",
						"given -> using '", obsName, "' (", basename(path), "). ",
						"Pass obsName to pick another: ", paste(nms, collapse = ", "), ".")
	df <- utils::read.csv(path, header = TRUE,
												stringsAsFactors = FALSE, check.names = FALSE)

	iwres_col <- paste0("indWRes_",   est)
	pred_col  <- paste0("indivPred_", est)
	missing   <- setdiff(c("id", "time", iwres_col, pred_col), names(df))
	if (length(missing))
		stop(paste0("predictions.txt missing column(s): ",
								paste(missing, collapse = ", "), "\n  available: ",
								paste(names(df), collapse = ", ")))

	out <- data.frame(
		id         = df$id,
		time       = df$time,
		prediction = df[[pred_col]],
		iwres      = df[[iwres_col]],            # signed; abs() taken at the end
		censored   = FALSE,
		loq        = NA_real_,
		limit      = NA_real_,
		stringsAsFactors = FALSE)

	# join censoring (BLQ) info from the dataset, matched by id + time (+ occasion
	# column(s) when present, so IOV rows sharing an id/time are disambiguated).
	cinfo <- tryCatch(.getCensoringInfo(obsName), error = function(e) NULL)
	if (!is.null(cinfo) && nrow(cinfo)) {
		# occ columns are whatever cinfo carries beyond the fixed set; keep those
		# also present in predictions.txt (df) as extra join keys.
		occ_cols  <- setdiff(names(cinfo), c("id", "time", "censored", "loq", "limit"))
		join_cols <- c("id", "time", intersect(occ_cols, names(df)))
		mkkey <- function(d)
			do.call(paste, c(lapply(join_cols, function(cc) as.character(d[[cc]])),
											 list(sep = "\r")))
		idx <- match(mkkey(df), mkkey(cinfo))    # df holds id/time/occ for the LHS
		hit <- !is.na(idx)
		out$censored[hit] <- cinfo$censored[idx[hit]]
		out$loq[hit]      <- cinfo$loq[idx[hit]]
		out$limit[hit]    <- cinfo$limit[idx[hit]]
	}

	# For censored (BLQ) points predictions.txt stores the deterministic residual
	# r0 = (LOQ - f)/g at the limit. With censoring = "simulated" (the GUI default)
	# replace it by a draw from N(0,1) truncated to the censoring interval
	# [(LIMIT - f)/g, (LOQ - f)/g], recovering the error sd g = (LOQ - f)/r0 from
	# r0 itself. This reproduces the GUI spread. censoring = "loq" keeps r0.
	# (Interval/left censoring on the natural scale; transformed-scale error models
	#  and right censoring are not handled here.)
	if ((settings$censoring %||% "simulated") == "simulated") {
		ci <- which(out$censored)
		if (length(ci)) {
			f  <- out$prediction[ci]
			r0 <- out$iwres[ci]
			g  <- (out$loq[ci] - f) / r0
			up <- r0                               # (LOQ - f)/g
			lo <- (out$limit[ci] - f) / g          # (LIMIT - f)/g
			lo[is.na(lo)] <- -Inf                  # no lower censoring bound
			ok <- is.finite(g) & g > 0 & is.finite(up)
			# reproducible BLQ draws without perturbing the caller's RNG stream
			sim <- .withSeed(12345L,
											 stats::qnorm(stats::runif(length(ci),
																								 stats::pnorm(lo), stats::pnorm(up))))
			out$iwres[ci[ok]] <- sim[ok]
		}
	}

	out$absiwres <- abs(out$iwres)
	out[is.finite(out$absiwres) & !is.na(out$absiwres), , drop = FALSE]
}


################################################################################
# Package-side stratification (reuses R/PlotStratification.R helpers).
# Joins covariate groups onto residuals by id -> GUI-independent grouping.
.attachStratify <- function(dat, stratify, parsed = NULL) {
	if (is.null(parsed)) parsed <- .parseStratify(stratify)
	has_strat <- !is.null(parsed$split_specs) || !is.null(parsed$color_specs) ||
		!is.null(parsed$filter_specs) || !is.null(parsed$ids)
	if (!has_strat) {
		dat$split <- factor("All"); dat$color <- factor("All")
		return(dat)
	}
	if (!is.null(parsed$filter_specs))
		dat <- .applyStratifyFilter(dat, parsed$filter_specs, stratify$groups)
	if (!is.null(parsed$ids))
		dat <- dat[dat$id %in% parsed$ids, , drop = FALSE]

	covSplit <- .stratifyCovariates(split = parsed$split_specs, filter = NULL)
	covColor <- .renameColumns(
		.stratifyCovariates(split = parsed$color_specs, filter = NULL, splitbyid = TRUE),
		"split", "color")
	covData  <- merge(covSplit, covColor)

	# coerce join keys to character (predictions id is numeric, cov id is factor)
	dat$id           <- as.character(dat$id)
	covData$id       <- as.character(covData$id)
	dat <- merge(dat, covData[, c("id", "split", "color")], by = "id")
	dat$split <- droplevels(factor(dat$split))
	dat$color <- droplevels(factor(dat$color))
	dat
}


################################################################################
# Package-side binning (GUI-independent). Returns integer bin index per x.
# Supports "equalwidth", "equalsize" and "leastsquare" (Monolix default).

# Least-squares binning: optimal contiguous partition of the sorted values into
# `nbins` groups minimising the total within-bin sum of squares (the 1-D optimal
# partition / Fisher-Jenks problem), solved exactly by O(nbins * n^2) DP. This
# reproduces Monolix's "Least-squares" binning criterion.
.leastSquaresBins <- function(x, nbins) {
	ord <- order(x); xs <- x[ord]; n <- length(xs)
	nbins <- max(1L, min(as.integer(nbins), n))

	cs  <- c(0, cumsum(xs))          # prefix sums for O(1) segment SS
	css <- c(0, cumsum(xs^2))
	ss  <- function(i, j) {          # within-segment SS for sorted points i..j
		len <- j - i + 1L
		s   <- cs[j + 1L] - cs[i]
		(css[j + 1L] - css[i]) - s * s / len
	}

	D <- matrix(Inf, nbins, n)       # D[k, j] = min SS of first j pts in k bins
	B <- matrix(0L,  nbins, n)       # back-pointers
	for (j in seq_len(n)) D[1, j] <- ss(1L, j)
	if (nbins >= 2L) for (k in 2:nbins) for (j in k:n) {
		for (m in (k - 1L):(j - 1L)) {
			cst <- D[k - 1L, m] + ss(m + 1L, j)
			if (cst < D[k, j]) { D[k, j] <- cst; B[k, j] <- m }
		}
	}

	grp_sorted <- integer(n); j <- n
	for (k in nbins:1L) {
		i <- if (k == 1L) 1L else B[k, j] + 1L
		grp_sorted[i:j] <- k
		j <- i - 1L
	}
	grp <- integer(n); grp[ord] <- grp_sorted
	grp
}

.makeBins <- function(x, binsSettings) {
	crit  <- binsSettings$criteria %||% "leastsquare"
	nbins <- if (isTRUE(binsSettings$is.fixedNbBins)) (binsSettings$nbBins %||% 5L)
	else max(binsSettings$binRange[1] %||% 5L, 5L)
	x <- as.numeric(x)
	nbins <- max(1L, min(nbins, length(unique(x))))

	if (crit == "leastsquare")
		return(.leastSquaresBins(x, nbins))

	if (crit == "equalwidth") {
		br <- seq(min(x), max(x), length.out = nbins + 1L)
	} else {                          # equalsize
		br <- stats::quantile(x, probs = seq(0, 1, length.out = nbins + 1L),
													names = FALSE, type = 7)
		br <- unique(br)
	}
	br[1] <- br[1] - 1e-9; br[length(br)] <- br[length(br)] + 1e-9
	as.integer(cut(x, breaks = br, include.lowest = TRUE))
}


################################################################################
# Per-bin summary: empirical percentiles of |IWRES|, flat half-normal predicted
# percentiles, and simulation-based (VPC) prediction-interval bands.
.summariseBins <- function(sub, xvar, binsSettings, percs, level,
													 nSim = 500L, seed = 12345L) {
	x      <- as.numeric(sub[[xvar]])
	binidx <- .makeBins(x, binsSettings)
	ub     <- sort(unique(binidx[!is.na(binidx)]))

	# per-bin data extent + shared edges so bands span the full x range with no
	# gaps: first bin starts at min(x), last ends at max(x), interior edges are
	# midpoints between adjacent bins' data. (Bins from .makeBins are x-ordered.)
	xmn <- vapply(ub, function(b) min(x[binidx == b]), numeric(1))
	xmx <- vapply(ub, function(b) max(x[binidx == b]), numeric(1))
	nb  <- length(ub)
	xlo <- numeric(nb); xhi <- numeric(nb)
	xlo[1] <- xmn[1]; xhi[nb] <- xmx[nb]
	if (nb > 1L) {
		mids <- (xmx[-nb] + xmn[-1]) / 2
		xhi[-nb] <- mids; xlo[-1] <- mids
	}

	a   <- (1 - level / 100) / 2
	qlo <- a; qhi <- 1 - a

	# VPC bands are simulated; seed for reproducibility but do not perturb the
	# caller's global RNG stream (see .withSeed).
	rows <- .withSeed(seed, lapply(seq_along(ub), function(k) {
		b  <- ub[k]
		m  <- which(binidx == b)
		n  <- length(m)
		if (n < 1L) return(NULL)
		do.call(rbind, lapply(names(percs), function(pn) {
			p   <- percs[[pn]]
			emp <- stats::quantile(sub$absiwres[m], probs = p, names = FALSE, type = 7)
			sim <- replicate(nSim,
											 stats::quantile(abs(stats::rnorm(n)), probs = p,
											 								names = FALSE, type = 7))
			band <- stats::quantile(sim, probs = c(qlo, qhi), names = FALSE)
			data.frame(bin = b, x = stats::median(x[m]), xlo = xlo[k], xhi = xhi[k],
								 n = n, perc = pn, p = p,
								 empirical = emp, predicted = .halfNormalQuantile(p),
								 ci_lower = band[1], ci_upper = band[2],
								 outlier = emp < band[1] | emp > band[2],
								 stringsAsFactors = FALSE)
		}))
	}))
	do.call(rbind, rows)
}
################################################################################
# For linear-interpolation rendering: within each (xaxis, split, perc) group,
# order rows by bin centre and append the first/last bin's values at the outer
# bin edges, so an interpolated line/ribbon still spans the full x range.
.extendToEdges <- function(df) {
	key   <- interaction(df$xaxis, df$split, df$perc, drop = TRUE)
	parts <- split(df, key)
	out <- lapply(parts, function(g) {
		g <- g[order(g$x), ]; n <- nrow(g)
		ext <- g[c(1L, seq_len(n), n), , drop = FALSE]
		ext$x <- c(g$xlo[1], g$x, g$xhi[n])
		ext
	})
	do.call(rbind, out)
}
################################################################################
# Resample empirical / PI curves of an edge-extended frame onto a fine x-grid
# per (xaxis, split, perc), so that pmax()/pmin() outlier-area shading captures
# the true crossing between the observed line and the PI bound (not just the
# bin vertices). Returns x, empirical, ci_lower, ci_upper + grouping columns.
.fineGrid <- function(df, npts = 200L) {
	key   <- interaction(df$xaxis, df$split, df$perc, drop = TRUE)
	parts <- split(df, key)
	out <- lapply(parts, function(g) {
		g  <- g[order(g$x), , drop = FALSE]
		ux <- !duplicated(g$x)
		xs <- g$x[ux]
		if (length(xs) < 2L) return(NULL)
		xg <- seq(min(xs), max(xs), length.out = npts)
		data.frame(xaxis = g$xaxis[1], split = g$split[1], perc = g$perc[1], x = xg,
							 empirical = stats::approx(xs, g$empirical[ux], xg)$y,
							 ci_lower  = stats::approx(xs, g$ci_lower[ux],  xg)$y,
							 ci_upper  = stats::approx(xs, g$ci_upper[ux],  xg)$y,
							 stringsAsFactors = FALSE)
	})
	do.call(rbind, out)
}

################################################################################
#' Scatter plot of the absolute individual weighted residuals |IWRES|
#'
#' Plot the scatter plot of the absolute individual weighted residuals,
#' \code{|IWRES|}, versus time and/or individual predictions, with a
#' \strong{half-normal} reference distribution. Because \code{IWRES ~ N(0, 1)}
#' under the model, \code{|IWRES|} follows a half-normal distribution; the
#' predicted percentiles and the prediction interval are therefore computed from
#' the half-normal distribution.
#'
#' The data are read from the project's exported \code{predictions.txt}
#' (\code{indivPred_*} / \code{indWRes_*} columns).
#'
#' @param obsName (character) Name of the observation (in dataset header). By
#'   default the first observation is considered. For multiple modelled outputs,
#'   the matching \code{predictions_<output>_<obsName>.txt} file is used and a
#'   message reports which observation was selected.
#'
#' @param xaxis (character) List of x-axis to display: time (\code{"time"}),
#'   individual prediction (\code{"prediction"})
#'   (default \code{c("time", "prediction")}).
#'
#' @param settings List with the following settings
#'   \describe{
#'     \item{indivEstimate}{(character) Calculation of individual estimates:
#'       conditional mean (\code{"mean"}) or conditional mode with EBEs
#'       (\code{"mode"}) (default \code{"mode"}). The conditional distribution
#'       (\code{"simulated"}) is not available from \code{predictions.txt}.}
#'     \item{level}{(integer) Level for prediction-interval computation
#'       (default 90).}
#'     \item{higherPercentile}{(integer) Higher percentile for empirical and
#'       predicted percentiles computation (default 90). The lower percentile is
#'       \code{100 - higherPercentile} and the median (50th) is always shown.}
#'     \item{useCensored}{(logical) Use BLQ data (TRUE) or ignore it (FALSE) to
#'       compute the statistics (default TRUE).}
#'     \item{censoring}{(character) BLQ residuals can be simulated
#'       (\code{"simulated"}) from the distribution truncated to the censoring
#'       interval, or set equal to the limit of quantification (\code{"loq"})
#'       (default \code{"simulated"}).}
#'     \item{binsSettings}{a list of settings for bins:
#'       \describe{
#'         \item{criteria}{(character) Binning criteria, one of
#'           \code{"equalwidth"}, \code{"equalsize"}, or \code{"leastsquare"}
#'           (default \code{"leastsquare"}).}
#'         \item{is.fixedNbBins}{(logical) If TRUE define a fixed number of bins,
#'           else define a range for automatic selection (default TRUE).}
#'         \item{nbBins}{(integer) Fixed number of bins (default 5).}
#'         \item{binRange}{(vector(integer, integer)) Range for the number of
#'           bins (default \code{c(5, 100)}). Note: automatic bin-count selection
#'           is not fully implemented; when \code{is.fixedNbBins = FALSE} the
#'           lower bound \code{binRange[1]} is used as the number of bins.}
#'         \item{nbBinData}{(vector(integer, integer)) Range for the number of
#'           data points per bin (default \code{c(5, 200)}). Currently not used.}
#'       }}
#'     \item{residuals}{(logical) If TRUE display the |IWRES| dots
#'       (default TRUE).}
#'     \item{empPercentiles}{(logical) If TRUE display empirical percentiles
#'       (default FALSE).}
#'     \item{predPercentiles}{(logical) If TRUE display predicted (half-normal)
#'       percentiles (default FALSE).}
#'     \item{predInterval}{(logical) If TRUE the prediction interval is displayed
#'       (default FALSE).}
#'     \item{outlierDots}{(logical) If TRUE add red dots indicating empirical
#'       percentiles that are outside prediction intervals (default TRUE).}
#'     \item{outlierAreas}{(logical) If TRUE add red areas between an empirical
#'       percentile and the prediction-interval bound it violates
#'       (default TRUE).}
#'     \item{spline}{(logical) If TRUE display a smoothing spline (default
#'       FALSE).}
#'     \item{binLimits}{(logical) If TRUE add bin limits as vertical lines
#'       (default FALSE).}
#'     \item{linearInterpolation}{(logical) If TRUE the prediction-interval
#'       bands, empirical percentiles and outlier areas are linearly
#'       interpolated between bin centres; if FALSE they are drawn as steps
#'       spanning each bin (default TRUE).}
#'     \item{legend}{(logical) add (TRUE) / remove (FALSE) plot legend
#'       (default FALSE).}
#'     \item{grid}{(logical) add (TRUE) / remove (FALSE) plot grid
#'       (default TRUE).}
#'     \item{footnote}{(logical) If TRUE add a footnote below the plot (and
#'       below the legend) giving the Monolix project directory and the R script
#'       path, styled like the table footers (small, italic, left-aligned)
#'       (default FALSE).}
#'     \item{ncol}{(integer) number of columns for the x-axis facets when no
#'       split is applied (default 4).}
#'   }
#'
#' @param preferences (optional) List of visual preferences. Each element is a
#'   list of the aesthetics for one plot component:
#'   \describe{
#'     \item{point}{Observed |IWRES| dots: \code{color}, \code{size}.}
#'     \item{censored}{Censored (BLQ) dots: \code{color}, \code{size}.}
#'     \item{empPercentile}{Empirical percentile lines: \code{color},
#'       \code{linewidth}.}
#'     \item{predPercentile}{Predicted (half-normal) percentile lines:
#'       \code{color}, \code{linewidth}, \code{linetype}.}
#'     \item{predInterval}{Prediction-interval bands: \code{medianFill},
#'       \code{medianAlpha} (50th percentile band) and \code{outerFill},
#'       \code{outerAlpha} (lower/upper percentile bands).}
#'     \item{outlier}{Outlier dots: \code{color}, \code{size}.}
#'     \item{outlierArea}{Outlier areas: \code{fill}, \code{alpha}.}
#'     \item{spline}{Smoothing spline: \code{color}, \code{linewidth}.}
#'     \item{binLimit}{Bin-limit vertical lines: \code{color}, \code{linewidth},
#'       \code{linetype}.}
#'   }
#'
#' @param stratify List with the stratification arguments:
#'   \describe{
#'     \item{groups}{Definition of stratification groups. By default, groups are
#'       one per category for categorical covariates, and two groups of equal
#'       number of individuals (split at the median) for continuous covariates.
#'       To redefine groups, for each covariate specify a list with:
#'       \code{name} (covariate name, e.g. \code{"AGE"}) and \code{definition}
#'       (for continuous covariates, a vector of break values, e.g.
#'       \code{c(35, 65)}; for categorical covariates, groups of categories as a
#'       list of vectors, e.g. \code{list(c("study101"), c("study201","study202"))}).}
#'     \item{split}{(vector<character>) Covariates used to split (i.e. facet) the
#'       plot rows (by default no split is applied), e.g. \code{c("SEX")}.}
#'     \item{filter}{(list of pairs) Each pair is a covariate name and a vector
#'       of group indices (continuous) or category names (categorical) to keep,
#'       e.g. \code{list(list("AGE", c(1, 3)), list("SEX", "F"))}.}
#'     \item{color}{(vector<character>) Covariates used for point colouring
#'       (by default no colouring is applied), e.g. \code{c("SEX")}.}
#'     \item{colors}{Vector of colours to use when \code{color} is set. Takes
#'       precedence over colours defined in preferences, e.g.
#'       \code{c("#CD9B1D", "#CD6090")}.}
#'     \item{mergedSplits}{(logical) When TRUE and \code{split} is used, split
#'       groups are displayed on the same panel with different colours instead of
#'       separate rows (default FALSE).}
#'     \item{individualSelection}{List with \code{ids}: character vector of
#'       individual ids to display, e.g. \code{list(ids = c("101-01","101-02"))}.}
#'   }
#'
#' @details
#' Under the model \code{IWRES ~ N(0, 1)}, so \code{|IWRES|} is half-normal and
#' the p-th predicted percentile is \code{qnorm((1 + p) / 2)}. The prediction
#' interval is a VPC-style band obtained by simulating the empirical percentile
#' of \code{n} i.i.d. \code{|N(0, 1)|} draws per bin. Censoring is read from the
#' interpreted dataset (\code{getInterpretedData}) and joined to the residuals by
#' id and time; with \code{censoring = "simulated"} the BLQ residuals are drawn
#' from the distribution truncated to the censoring interval.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link{plotQQStandardizedRandomEffects}}
#'
#' @examples
#' \dontrun{
#'   initializeLixoftConnectors(software = "monolix")
#'   project <- file.path(getDemoPath(), "1.creating_and_using_models",
#'                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#'   loadProject(project)
#'   runPopulationParameterEstimation()
#'   runConditionalModeEstimation()
#'
#'   plotAbsIWRESScatterPlot()
#'   plotAbsIWRESScatterPlot(xaxis = "prediction",
#'                           settings = list(indivEstimate = "mean"))
#'   plotAbsIWRESScatterPlot(settings = list(spline = TRUE))
#'   plotAbsIWRESScatterPlot(settings = list(empPercentiles = TRUE,
#'                                           predPercentiles = TRUE,
#'                                           predInterval = TRUE,
#'                                           level = 90, higherPercentile = 75,
#'                                           binsSettings = list(is.fixedNbBins = TRUE,
#'                                                               nbBins = 5),
#'                                           binLimits = TRUE, legend = TRUE))
#'
#'   # Stratification
#'   plotAbsIWRESScatterPlot(stratify = list(filter = list("SEX", "F")))
#'   plotAbsIWRESScatterPlot(stratify = list(split = "SEX"))
#'   plotAbsIWRESScatterPlot(stratify = list(color = "WEIGHT",
#'                                           groups = list(name = "WEIGHT",
#'                                                         definition = c(75))))
#' }
#'
#' @export


plotAbsIWRESScatterPlot <- function(

	obsName  = NULL,
	xaxis    = c("time", "prediction"),
	settings = list(
		indivEstimate    = "mode",       # "mode" | "mean"
		level            = 90,           # prediction-interval level (percent)
		higherPercentile = 90,           # upper empirical percentile (percent)
		useCensored      = TRUE,         # include BLQ in the statistics (FALSE: exclude)
		censoring        = "simulated",  # BLQ residuals: "simulated" | "loq"
		binsSettings     = list(criteria = "leastsquare", is.fixedNbBins = TRUE,
														nbBins = 5L, binRange = c(5L, 100L),
														nbBinData = c(5L, 200L)),
		residuals        = TRUE,
		empPercentiles   = FALSE,
		predPercentiles  = FALSE,
		predInterval     = FALSE,
		outlierDots      = TRUE,
		outlierAreas     = TRUE,
		spline           = FALSE,
		binLimits        = FALSE,
		linearInterpolation = TRUE,      # TRUE: interpolate bands between bins; FALSE: steps
		legend           = FALSE,
		grid             = TRUE,
		footnote         = FALSE,        # TRUE: add project/R-script source footnote
		ncol             = 4L),
	stratify = list(),
	preferences = list(
		point          = list(color = "#4682B4", size = 1.5),
		empPercentile  = list(color = "#1F78B4", linewidth = 0.7),
		predPercentile = list(color = "#000000", linewidth = 0.7, linetype = "dashed"),
		predInterval   = list(medianFill = "#FF0000", medianAlpha = 0.2,
													outerFill  = "#0080FF", outerAlpha  = 0.3),
		outlier        = list(color = "#E31A1C", size = 2),
		outlierArea    = list(fill = "#E31A1C", alpha = 0.5),
		censored       = list(color = "#984EA3", size = 1.5),
		spline         = list(color = "#eba213", linewidth = 0.8),
		binLimit       = list(color = "#E31A1C", linewidth = 0.6, linetype = "solid"))){


	# merge user values over defaults
	d_settings    <- eval(formals()$settings)
	d_preferences <- eval(formals()$preferences)
	settings      <- modifyList(d_settings, settings)
	settings$binsSettings <- modifyList(d_settings$binsSettings,
																			settings$binsSettings %||% list())
	for (k in names(d_preferences))
		preferences[[k]] <- modifyList(d_preferences[[k]], preferences[[k]] %||% list())

	xaxis <- match.arg(xaxis, c("time", "prediction"), several.ok = TRUE)
	if (settings$level <= 0 || settings$level >= 100)
		stop("level must be a percentage strictly between 0 and 100.")

	higher <- settings$higherPercentile / 100
	percs  <- c(lower = 1 - higher, median = 0.5, upper = higher)

	# internal (non-user-facing) constants
	facet_scales <- "free"
	nSim         <- 500L      # VPC prediction-interval simulation replicates
	seed         <- 12345L    # reproducible VPC bands

	# load raw points + attach package-side stratification
	parsed <- .parseStratify(stratify)
	raw    <- .getAbsIWRESData(obsName, settings)
	raw    <- .attachStratify(raw, stratify, parsed)

	grp_col <- if (isTRUE(parsed$mergedSplits)) "color" else "split"

	# long plotting frame for the dots
	point_df <- do.call(rbind, lapply(xaxis, function(ax) {
		xv <- if (ax == "time") "time" else "prediction"
		data.frame(xaxis = ax, x = raw[[xv]], absiwres = raw$absiwres,
							 censored = raw$censored, split = raw$split, color = raw$color,
							 stringsAsFactors = FALSE)
	}))

	# per-bin summaries, per x-axis and per grouping level. As in Monolix,
	# useCensored controls only whether BLQ data enter the statistics (empirical
	# percentiles / bins / prediction interval); the censored dots are still
	# displayed via point_df above. Drop censored rows from the stats when FALSE.
	stat_raw <- if (isTRUE(settings$useCensored)) raw else raw[!raw$censored, , drop = FALSE]
	summ_df <- do.call(rbind, lapply(xaxis, function(ax) {
		xv <- if (ax == "time") "time" else "prediction"
		do.call(rbind, lapply(levels(stat_raw[[grp_col]]), function(g) {
			sub <- stat_raw[stat_raw[[grp_col]] == g, , drop = FALSE]
			if (nrow(sub) < 2L) return(NULL)
			s <- .summariseBins(sub, xv, settings$binsSettings, percs, settings$level,
													nSim = nSim, seed = seed)
			if (is.null(s)) return(NULL)
			s$xaxis <- ax
			s$split <- if (grp_col == "split") g else sub$split[1]
			s$color <- if (grp_col == "color") g else sub$color[1]
			s
		}))
	}))

	has_split  <- length(unique(point_df$split)) > 1
	has_color  <- length(unique(point_df$color)) > 1
	base_theme <- if (settings$grid) theme_minimal() else theme_classic()
	interp     <- isTRUE(settings$linearInterpolation)
	lowerPct   <- 100 - settings$higherPercentile
	upperPct   <- settings$higherPercentile
	L_OBS      <- "Observed data"
	L_CENS     <- "Censored data"
	L_OUT      <- "Outliers"
	L_OUTAREA  <- "Outlier areas"
	L_SPLINE   <- "Spline"
	L_BINLIM   <- "Bin limits"
	L_EMP      <- paste0("Observed percentiles (median, ", .ordinal(lowerPct), " and ", .ordinal(upperPct), ")")
	L_PRED     <- paste0("Half-normal median, ",           .ordinal(lowerPct), " and ", .ordinal(upperPct), " percentiles")
	L_PI       <- paste0(settings$level, "% prediction interval of predicted percentiles")
	out_df     <- if (!is.null(summ_df)) summ_df[summ_df$outlier, , drop = FALSE] else summ_df

	# collectors for the shape / linetype / fill legends (label -> value & key colour)
	shp_val <- integer(0);  shp_col <- character(0)
	lt_val  <- character(0); lt_col <- character(0)
	fill_val <- character(0)

	p <- ggplot()

	# prediction-interval bands: interpolated ribbons or stepped rectangles.
	# The outer band carries the (single) fill legend entry with a composite key;
	# the median band uses a fixed fill and is not added to the legend again.
	addBand <- function(p, df, alpha, fill_fixed = NULL, fill_label = NULL, keyglyph = "rect") {
		if (is.null(df) || !nrow(df)) return(p)
		mapped <- !is.null(fill_label)
		if (interp) {
			d <- .extendToEdges(df)
			if (mapped)
				p + geom_ribbon(data = d, aes(x = x, ymin = ci_lower, ymax = ci_upper,
																			fill = fill_label, group = interaction(perc, split, xaxis)),
												alpha = alpha, key_glyph = keyglyph)
			else
				p + geom_ribbon(data = d, aes(x = x, ymin = ci_lower, ymax = ci_upper,
																			group = interaction(perc, split, xaxis)),
												fill = fill_fixed, alpha = alpha)
		} else {
			if (mapped)
				p + geom_rect(data = df, aes(xmin = xlo, xmax = xhi, ymin = ci_lower, ymax = ci_upper,
																		 fill = fill_label, group = interaction(perc, bin)),
											alpha = alpha, key_glyph = keyglyph)
			else
				p + geom_rect(data = df, aes(xmin = xlo, xmax = xhi, ymin = ci_lower, ymax = ci_upper,
																		 group = interaction(perc, bin)),
											fill = fill_fixed, alpha = alpha)
		}
	}
	if (settings$predInterval && !is.null(summ_df)) {
		piKey <- .makePiKeyGlyph(preferences$predInterval$outerFill, preferences$predInterval$outerAlpha,
														 preferences$predInterval$medianFill, preferences$predInterval$medianAlpha)
		# outer band first (behind) -> mapped to the single legend entry
		p <- addBand(p, summ_df[summ_df$perc != "median", , drop = FALSE],
								 preferences$predInterval$outerAlpha, fill_label = L_PI, keyglyph = piKey)
		# median band on top -> fixed fill, no extra legend entry
		p <- addBand(p, summ_df[summ_df$perc == "median", , drop = FALSE],
								 preferences$predInterval$medianAlpha, fill_fixed = preferences$predInterval$medianFill)
		fill_val[L_PI] <- preferences$predInterval$outerFill
	}

	# outlier areas: shade the region between an observed percentile and the
	# prediction-interval bound it violates, in red. Rendering mirrors the bands
	# (interpolated ribbon or stepped rectangles).
	if (settings$outlierAreas && !is.null(summ_df) && any(summ_df$outlier)) {
		if (interp) {
			# Shade between the interpolated empirical line and its PI bound where the
			# line is OUTSIDE the band. Two zero-floored ribbons: upper fills
			# [ci_upper, max(emp, ci_upper)], lower fills [min(emp, ci_lower), ci_lower]
			# -> zero height inside, so the shaded region starts exactly at the
			# crossing. Resampled to a fine grid so the crossing is captured accurately.
			oa <- .fineGrid(.extendToEdges(summ_df))
			a  <- preferences$outlierArea$alpha
			p  <- p +
				geom_ribbon(data = oa,
										aes(x = x, ymin = ci_upper, ymax = pmax(empirical, ci_upper),
												fill = L_OUTAREA, group = interaction(perc, split, xaxis)),
										alpha = a) +
				geom_ribbon(data = oa,
										aes(x = x, ymin = pmin(empirical, ci_lower), ymax = ci_lower,
												fill = L_OUTAREA, group = interaction(perc, split, xaxis)),
										alpha = a)
		} else {
			above <- summ_df$empirical > summ_df$ci_upper
			below <- summ_df$empirical < summ_df$ci_lower
			oa <- summ_df
			oa$amin <- ifelse(above, oa$ci_upper, ifelse(below, oa$empirical, NA_real_))
			oa$amax <- ifelse(above, oa$empirical, ifelse(below, oa$ci_lower, NA_real_))
			oa <- oa[!is.na(oa$amin), , drop = FALSE]
			if (nrow(oa))
				p <- p + geom_rect(data = oa,
													 aes(xmin = xlo, xmax = xhi, ymin = amin, ymax = amax,
													 		fill = L_OUTAREA, group = interaction(perc, bin)),
													 alpha = preferences$outlierArea$alpha)
		}
		fill_val[L_OUTAREA] <- preferences$outlierArea$fill
	}

	# residual dots: grouped colour when colour-stratified, else shape entries.
	# Censored (BLQ) residuals are drawn in a distinct colour (own legend entry).
	if (settings$residuals) {
		if (has_color) {
			# covariate colour grouping takes the colour aesthetic; censored not split
			p <- p + geom_point(data = point_df, aes(x = x, y = absiwres, colour = color),
													size = preferences$point$size)
		} else {
			obs_pts  <- point_df[!point_df$censored, , drop = FALSE]
			cens_pts <- point_df[ point_df$censored, , drop = FALSE]
			if (nrow(obs_pts)) {
				p <- p + geom_point(data = obs_pts, aes(x = x, y = absiwres, shape = L_OBS),
														colour = preferences$point$color, size = preferences$point$size)
				shp_val[L_OBS] <- 16L; shp_col[L_OBS] <- preferences$point$color
			}
			if (nrow(cens_pts)) {
				p <- p + geom_point(data = cens_pts, aes(x = x, y = absiwres, shape = L_CENS),
														colour = preferences$censored$color, size = preferences$censored$size)
				shp_val[L_CENS] <- 16L; shp_col[L_CENS] <- preferences$censored$color
			}
		}
	}

	# empirical percentile lines (interp: line through bin centres; else steps)
	if (settings$empPercentiles && !is.null(summ_df)) {
		lw  <- preferences$empPercentile$linewidth
		if (interp)
			p <- p + geom_line(data = .extendToEdges(summ_df),
												 aes(x = x, y = empirical, linetype = L_EMP,
												 		group = interaction(perc, split, xaxis)),
												 colour = preferences$empPercentile$color, linewidth = lw)
		else
			p <- p + geom_segment(data = summ_df,
														aes(x = xlo, xend = xhi, y = empirical, yend = empirical,
																linetype = L_EMP, group = interaction(perc, bin)),
														colour = preferences$empPercentile$color, linewidth = lw)
		lt_val[L_EMP] <- "solid"; lt_col[L_EMP] <- preferences$empPercentile$color
	}

	# predicted (half-normal) percentiles: constant in x -> full-width segments
	if (settings$predPercentiles && !is.null(summ_df)) {
		p <- p + geom_segment(data = summ_df,
													aes(x = xlo, xend = xhi, y = predicted, yend = predicted,
															linetype = L_PRED, group = interaction(perc, bin)),
													colour = preferences$predPercentile$color,
													linewidth = preferences$predPercentile$linewidth)
		lt_val[L_PRED] <- preferences$predPercentile$linetype
		lt_col[L_PRED] <- preferences$predPercentile$color
	}

	# outlier dots (empirical percentile falling outside its prediction interval)
	if (settings$outlierDots && !is.null(out_df) && nrow(out_df)) {
		p <- p + geom_point(data = out_df, aes(x = x, y = empirical, shape = L_OUT),
												colour = preferences$outlier$color, size = preferences$outlier$size)
		shp_val[L_OUT] <- 16L; shp_col[L_OUT] <- preferences$outlier$color
	}

	if (settings$spline) {
		p <- p + geom_smooth(data = point_df,
												 aes(x = x, y = absiwres, linetype = L_SPLINE),
												 method = "loess", se = FALSE, key_glyph = "path",
												 color = preferences$spline$color,
												 linewidth = preferences$spline$linewidth)
		lt_val[L_SPLINE] <- "solid"; lt_col[L_SPLINE] <- preferences$spline$color
	}

	# bin-limit vertical lines (per xaxis x split panel). Drawn LAST so they stay
	# visible on top of the bands and the (dense) point cloud.
	if (settings$binLimits && !is.null(summ_df)) {
		key <- interaction(summ_df$xaxis, summ_df$split, drop = TRUE)
		bl  <- do.call(rbind, lapply(split(summ_df, key), function(g)
			data.frame(xaxis = g$xaxis[1], split = g$split[1],
								 edge = sort(unique(c(g$xlo, g$xhi))), stringsAsFactors = FALSE)))
		p <- p + geom_vline(data = bl, aes(xintercept = edge, linetype = L_BINLIM),
												color = preferences$binLimit$color,
												linewidth = preferences$binLimit$linewidth,
												key_glyph = "path")
		lt_val[L_BINLIM] <- preferences$binLimit$linetype
		lt_col[L_BINLIM] <- preferences$binLimit$color
	}

	# faceting: xaxis labels placed BELOW the panels, at the axis, with nice titles
	xaxis_lab <- labeller(xaxis = c(time = "Time", prediction = "Individual predictions"))
	if (has_split)
		p <- p + facet_grid(split ~ xaxis, scales = facet_scales, switch = "x",
												labeller = xaxis_lab)
	else
		p <- p + facet_wrap(~ xaxis, ncol = settings$ncol, scales = facet_scales,
												strip.position = "bottom", labeller = xaxis_lab)

	# scales: stratification colour (if any) + shape + linetype legends
	if (has_color) {
		cols <- stratify$colors
		if (!is.null(cols) && length(cols) >= length(unique(point_df$color)))
			p <- p + scale_colour_manual(values = cols, name = NULL)
		else
			p <- p + scale_colour_discrete(name = NULL)
	}
	# NB: pin `breaks` to the insertion order so each guide's override.aes colours
	# line up with the keys (ggplot otherwise sorts discrete breaks alphabetically).
	if (length(shp_val))
		p <- p + scale_shape_manual(
			values = shp_val, name = NULL, breaks = names(shp_val),
			guide  = guide_legend(order = 1,
														override.aes = list(colour = unname(shp_col[names(shp_val)]),
																								size   = preferences$point$size + 1)))
	if (length(lt_val))
		p <- p + scale_linetype_manual(
			values = lt_val, name = NULL, breaks = names(lt_val),
			guide  = guide_legend(order = 2,
														override.aes = list(colour = unname(lt_col[names(lt_val)]))))
	# fill legend: prediction-interval band (composite key from its layer) and the
	# outlier areas. Each label's key is drawn by its own layer, so no override.
	if (length(fill_val))
		p <- p + scale_fill_manual(values = fill_val, name = NULL, breaks = names(fill_val),
															 guide = guide_legend(order = 3))

	p <- p +
		labs(x = NULL, y = "|IWRES|") +
		base_theme +
		theme(legend.position  = if (settings$legend) "bottom" else "none",
					legend.box       = "vertical",     # stack the guides vertically
					legend.direction = "vertical",     # stack keys within each guide
					strip.placement  = "outside",      # xaxis strip sits below the axis
					strip.background  = element_blank())

	# optional source footnote below the plot (and below the legend), styled like
	# the flextable footers (small, italic, left-aligned). Uses plot.caption so it
	# stays at the bottom even if the legend is later moved (e.g. to the right).
	if (isTRUE(settings$footnote)) {
		proj <- tryCatch(lixoftConnectors::getProjectSettings()$directory,
										 error = function(e) NULL)
		rsrc <- tryCatch(.getRScriptPath(), error = function(e) NULL)
		lines <- c(if (!is.null(proj) && nzchar(proj)) paste0("Monolix project: ", proj),
							 if (!is.null(rsrc) && nzchar(rsrc)) paste0("R source: ", rsrc))
		if (length(lines))
			p <- p +
				labs(caption = paste(lines, collapse = "\n")) +
				theme(plot.caption = element_text(size = 7, face = "italic", hjust = 0))
	}

	p
}
