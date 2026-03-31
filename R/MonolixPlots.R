#' QQ plot of standardized random effects
#'
#' @param parameters Character vector of parameter names with random effects.
#'   If NULL, all available parameters are used.
#' @param settings List of plot settings:
#'   - indivEstimate: "mean" (default), "mode", or "saem"
#'   - identityLine: logical, show identity line (default TRUE)
#'   - confidenceInterval: logical, show CI band (default TRUE)
#'   - level: CI level, e.g. 0.90 (default)
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
                                                               ncol               = 2),
                                            stratify    = list(),
                                            preferences = list(point = list(color = "#4682B4",
                                                                            size  = 1.5),
                                                               identityLine = list(color     = "#000000",
                                                                                   linewidth = 0.7,
                                                                                   label     = "Idntity line"),
                                                               confidenceInterval = list(color     = "#000000",
                                                                                         linewidth = 0.725,
                                                                                         linetype  = "dashed",
                                                                                         label     = "Confidence interval"))) {
  
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
  
  # settings <- list(
  #   indivEstimate      = "mode",
  #   identityLine       = TRUE,
  #   confidenceInterval = TRUE,
  #   legend             = TRUE,
  #   grid               = TRUE,
  #   level              = 0.90,
  #   ncol               = 2)
  # 
  # preferences <- list(
  #   point = list(
  #     color = "#4682B4",
  #     size  = 1.5),
  #   identityLine = list(
  #     color     = "#000000",
  #     linewidth = 0.7,
  #     label     = "Identität"),
  #   confidenceInterval = list(
  #     color     = "#000000",
  #     linewidth = 0.725,
  #     linetype  = "dashed",
  #     label     = "CI"))
  # 
  # stratify <- list()
  # 
  
  #-----------------------------------------------------------------------------
  # Input validation
  valid_estimates <- c("mean", "mode", "saem")
  if (!settings$indivEstimate %in% valid_estimates) {
    stop(sprintf(
      "Invalid `indivEstimate`: '%s'. Must be one of: %s.",
      settings$indivEstimate,
      paste(valid_estimates, collapse = ", ")))
  }
  if (!is.numeric(settings$level) || settings$level <= 0 || settings$level >= 1) {
    stop("`level` must be a number strictly between 0 and 1, e.g. 0.90.")
  }
  
  #-----------------------------------------------------------------------------
  # Retrieve random effects 
  rand_eff_all <- getEstimatedRandomEffects()
  
  estimate_map <- list(
    mean = "conditionalMean",
    mode = "conditionalMode",
    saem = "saem")
  
  slot <- estimate_map[[settings$indivEstimate]]
  
  if (is.null(rand_eff_all[[slot]])) {
    stop(sprintf(
      "Individual estimate '%s' (slot '%s') is not available in the project results.",
      settings$indivEstimate, slot))
  }
  rand_eff_df <- rand_eff_all[[slot]]
  
  #-----------------------------------------------------------------------------
  # Select parameters
  eta_cols <- setdiff(names(rand_eff_df), "id")
  
  if (is.null(parameters)) {
    parameters <- eta_cols
  } else {
    missing_params <- setdiff(parameters, eta_cols)
    if (length(missing_params) > 0) {
      stop(sprintf(
        "The following parameters were not found in the random effects results: %s.",
        paste(missing_params, collapse = ", ")))
    }
  }
  
  #-----------------------------------------------------------------------------
  # Retrieve population parameters omegas to standardize random effects
  pop_params <- getEstimatedPopulationParameters()
  
  #-----------------------------------------------------------------------------
  # Compute shared CI (same n for all parameters)
  n     <- nrow(rand_eff_df)
  alpha <- 1 - settings$level
  
  lower_beta <- qbeta(alpha/2,     1:n, n:1)
  upper_beta <- qbeta(1 - alpha/2, 1:n, n:1)
  ci_lower   <- qnorm(lower_beta)
  ci_upper   <- qnorm(upper_beta)
  
  # Blom plotting positions and theoretical quantiles (same for all parameters)
  p             <- ((1:n) - (3/8)) / (n + (1/4))
  theoretical_q <- qnorm(p)
  
  #-----------------------------------------------------------------------------
  # Build long data frame for faceting
  plot_data <- do.call(rbind, lapply(parameters, function(param) {
    
    # omega parameter name convention: omega_<param_suffix>
    # e.g. eta_V -> omega_V
    param_suffix <- sub("^eta_", "", param)
    omega_name   <- paste0("omega_", param_suffix)
    
    if (is.null(pop_params[omega_name]) || is.na(pop_params[omega_name])) {
      stop(sprintf(
        "Could not find population variance parameter '%s' for random effect '%s'.",
        omega_name, param))
    }
    omega      <- pop_params[omega_name]
    std_re     <- rand_eff_df[[param]] / sqrt(omega)
    sorted_res <- sort(std_re)
    
    data.frame(parameter     = param,
               theoretical_q = theoretical_q,
               observed_q    = sorted_res,
               ci_lower      = ci_lower,
               ci_upper      = ci_upper)
  }))
  
  #-----------------------------------------------------------------------------
  # Build plot 
  # Legend labels (only for visible elements)
  # Use dummy color scale to drive the legend, mapping line names to colors
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
  
  # ---- base plot -------------------------------------------------------------
  base_theme <- if (settings$grid) theme_minimal() else theme_classic()
  
  p <- ggplot(plot_data, aes(x = theoretical_q, y = observed_q)) +
    geom_point(color = preferences$point$color,
               size  = preferences$point$size) +
    facet_wrap(~ parameter, ncol = settings$ncol) +
    labs(x     = "Theoretical Quantiles",
         y     = "Standardized Random Effects",
         color = NULL) +
    base_theme +
    theme(legend.position = if (settings$legend && length(legend_colors) > 0) "top" else "none")
  
  # ---- identity line ---------------------------------------------------------
  if (settings$identityLine) {
    p <- p + geom_abline(
      aes(intercept = 0, slope = 1,
          color    = preferences$identityLine$label,
          linetype = preferences$identityLine$label))
  }
  
  # ---- confidence interval lines ---------------------------------------------
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
  
  # ---- apply colour + linetype scales (drives legend) -----------------------
  if (length(legend_colors) > 0) {
    
    # Build override linewidths only for active elements
    override_linewidths <- c()
    if (settings$identityLine)       override_linewidths <- c(override_linewidths, preferences$identityLine$linewidth)
    if (settings$confidenceInterval) override_linewidths <- c(override_linewidths, preferences$confidenceInterval$linewidth)
    
    p <- p +
      scale_color_manual(
        values = legend_colors,
        breaks = names(legend_colors)) +
      scale_linetype_manual(values = legend_linetypes,
                            breaks = names(legend_linetypes),
                            guide  = "none") +
      guides(color = guide_legend(
        override.aes = list(linewidth = override_linewidths)))
  }
  
  return(p)
}