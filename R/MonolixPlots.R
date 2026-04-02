#' Internal helper: Prepare QQ plot data for standardized random effects
#'
#' @param parameters Character vector of parameter names
#' @param settings List with indivEstimate, level, and variabilityLevel
#' @return List with plot_data (list per parameter) and metadata
#' @keywords internal
.prepareQQDataStandardizedRandomEffects <- function(parameters, settings) {

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
	# Retrieve population parameters omegas to standardize random effects
	pop_params <- getEstimatedPopulationParameters()

	#-----------------------------------------------------------------------------
	# Compute shared CI (same n for all parameters)
	n     <- nrow(rand_eff_df)
	alpha <- 1 - settings$level

	lower_beta <- qbeta(alpha / 2,     1:n, n:1)
	upper_beta <- qbeta(1 - alpha / 2, 1:n, n:1)
	ci_lower   <- qnorm(lower_beta)
	ci_upper   <- qnorm(upper_beta)

	# Blom plotting positions and theoretical quantiles (same for all parameters)
	p             <- ((1:n) - (3 / 8)) / (n + (1 / 4))
	theoretical_q <- qnorm(p)

	#-----------------------------------------------------------------------------
	# Build data list (one data frame per parameter)
	plot_data_list <- lapply(parameters, function(param) {

		# omega parameter name convention: omega_<param_suffix>
		# e.g. eta_V -> omega_V, eta_V_ID -> omega_V
		param_suffix <- sub("^eta_", "", param)

		# For simulated with variability level, also remove the suffix
		# e.g. Cl_ID -> Cl, V_OCCevid -> V
		if (settings$indivEstimate == "simulated" && !is.null(settings$variabilityLevel)) {
			suffix_pattern <- paste0("_", settings$variabilityLevel, "$")
			param_suffix <- sub(suffix_pattern, "", param_suffix)
		}

		omega_name <- paste0("omega_", param_suffix)

		if (is.null(pop_params[omega_name]) || is.na(pop_params[omega_name])) {
			stop(sprintf(
				"Could not find population variance parameter '%s' for random effect '%s'.",
				omega_name, param))
		}
		omega      <- pop_params[omega_name]
		std_re     <- rand_eff_df[[param]] / sqrt(omega)
		sorted_res <- sort(std_re)

		data.frame(
			parameter     = param,
			theoretical_q = theoretical_q,
			observed_q    = sorted_res,
			ci_lower      = ci_lower,
			ci_upper      = ci_upper)
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
#'   - ncol: number of columns in facet layout (default 2)
#' @param stratify List, placeholder for future stratification (not yet implemented)
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
																							 preferences$point              %||% list())
	preferences$identityLine       <- modifyList(eval(default_preferences)$identityLine,
																							 preferences$identityLine       %||% list())
	preferences$confidenceInterval <- modifyList(eval(default_preferences)$confidenceInterval,
																							 preferences$confidenceInterval %||% list())

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
	# Prepare data using internal helper
	qq_data <- .prepareQQDataStandardizedRandomEffects(parameters, settings)

	#-----------------------------------------------------------------------------
	# Combine data from all parameters into single long data frame for faceting
	plot_data <- do.call(rbind, qq_data$plot_data_list)

	#-----------------------------------------------------------------------------
	# Build legend configuration (only for visible elements)
	legend_colors    <- c()
	legend_linetypes <- c()

	if (settings$identityLine) {
		legend_colors[preferences$identityLine$label]    <- preferences$identityLine$color
		legend_linetypes[preferences$identityLine$label] <- "solid"
	}
	if (settings$confidenceInterval) {
		legend_colors[preferences$confidenceInterval$label]    <- preferences$confidenceInterval$color
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
	# Create plot with facet_wrap
	base_theme <- if (settings$grid) theme_minimal() else theme_classic()

	p <- ggplot(plot_data, aes(x = theoretical_q, y = observed_q)) +
		geom_point(aes(shape = "Empirical quantiles"),
							 color = preferences$point$color,
							 size  = preferences$point$size) +
		facet_wrap(~ parameter, ncol = settings$ncol, scales = "free",
							 strip.position = "bottom") +
		labs(x        = "Theoretical quantiles",
				 y        = "Emprirical quantiles of standardized random effects",
				 color    = NULL,
				 linetype = NULL,
				 shape    = NULL) +
		base_theme +
		theme(legend.position  = if (settings$legend) "bottom" else "none",
					legend.direction = "vertical",
					axis.line   = element_line(color = "black"),
					axis.ticks  = element_line(color = "black"),
					strip.placement = "outside")

	# Add identity line
	if (settings$identityLine) {
		p <- p + geom_abline(
			aes(intercept = 0, slope = 1,
					color     = preferences$identityLine$label,
					linetype  = preferences$identityLine$label),
			linewidth = preferences$identityLine$linewidth,
			key_glyph = "path")
	}

	# Add confidence interval lines
	if (settings$confidenceInterval) {
		p <- p +
			geom_line(aes(y        = ci_lower,
										color    = preferences$confidenceInterval$label,
										linetype = preferences$confidenceInterval$label),
								linewidth = preferences$confidenceInterval$linewidth) +
			geom_line(aes(y        = ci_upper,
										color    = preferences$confidenceInterval$label,
										linetype = preferences$confidenceInterval$label),
								linewidth = preferences$confidenceInterval$linewidth)
	}

	# Apply scales
	p <- p +
		scale_shape_manual(
			values = c("Empirical quantiles" = 16),
			name   = NULL,
			guide  = guide_legend(order = 1))

	if (length(legend_colors) > 0) {
		p <- p +
			scale_color_manual(
				values = legend_colors,
				breaks = names(legend_colors),
				name   = NULL) +
			scale_linetype_manual(
				values = legend_linetypes,
				breaks = names(legend_linetypes),
				name   = NULL,
				guide  = "none") +
			guides(
				color = guide_legend(
					order = 2,
					override.aes = list(
						linewidth = unname(override_linewidths),
						linetype  = unname(override_linetypes))))
	}

	return(p)
}
