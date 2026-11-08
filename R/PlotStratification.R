# Translate public stratify API to internal format for .stratifyCovariates
#
# @param stratify List with: groups, split, color, colors, filter,
#   mergedSplits, individualSelection
# @return List with: split_specs, color_specs, filter_specs, colors,
#   mergedSplits, ids
.parseStratify <- function(stratify) {
  if (length(stratify) == 0) {
    return(list(split_specs = NULL, color_specs = NULL, filter_specs = NULL,
                colors = NULL, mergedSplits = FALSE, ids = NULL))
  }

  groups_defs <- stratify$groups

  # Helper: build old-format spec from covariate name + groups definitions
  .buildSpec <- function(cov_name) {
    spec <- list(name = cov_name)
    for (g in groups_defs) {
      if (g$name == cov_name) {
        defn <- g$definition
        if (is.numeric(defn)) {
          # Continuous: definition is breaks
          spec$breaks <- defn
        } else if (is.list(defn)) {
          # Categorical: definition is list of category vectors
          group_names <- sapply(defn, function(cats) paste(cats, collapse = " - "))
          spec$groups <- stats::setNames(defn, group_names)
        }
        break
      }
    }
    return(spec)
  }

  # Build split specs from vector of covariate names
  split_specs <- NULL
  if (!is.null(stratify$split)) {
    split_specs <- lapply(stratify$split, .buildSpec)
  }

  # Build color specs from vector of covariate names
  color_specs <- NULL
  mergedSplits <- if (is.null(stratify$mergedSplits)) FALSE else stratify$mergedSplits

  if (mergedSplits && !is.null(split_specs)) {
    # mergedSplits: split groups become color groups on the same panel
    color_specs <- split_specs
    split_specs <- NULL
  } else if (!is.null(stratify$color)) {
    color_specs <- lapply(stratify$color, .buildSpec)
  }

  # Filter specs (list of pairs: list(cov_name, indices_or_categories))
  # These are applied post-hoc after group assignment, not via .checkFilter
  filter_specs <- stratify$filter

  # Colors palette
  colors <- stratify$colors

  # Individual selection
  ids <- NULL
  if (!is.null(stratify$individualSelection)) {
    ids <- stratify$individualSelection$ids
  }

  return(list(split_specs = split_specs, color_specs = color_specs,
              filter_specs = filter_specs, colors = colors,
              mergedSplits = mergedSplits, ids = ids))
}

# Apply filter by group index/category after stratification
#
# @param rand_eff_df Data frame with id column
# @param filter_specs List of filter pairs, e.g. list(list("AGE", c(1,3)))
# @param groups_defs Groups definitions from stratify$groups
# @return Filtered rand_eff_df
.applyStratifyFilter <- function(rand_eff_df, filter_specs, groups_defs) {
  if (is.null(filter_specs) || length(filter_specs) == 0) return(rand_eff_df)

  for (f in filter_specs) {
    cov_name <- f[[1]]
    keep     <- f[[2]]

    # Build spec for this covariate and get per-id group assignment
    spec <- list(name = cov_name)
    for (g in groups_defs) {
      if (g$name == cov_name) {
        defn <- g$definition
        if (is.numeric(defn)) {
          spec$breaks <- defn
        } else if (is.list(defn)) {
          group_names <- sapply(defn, function(cats) paste(cats, collapse = " - "))
          spec$groups <- stats::setNames(defn, group_names)
        }
        break
      }
    }

    cov_strat <- .stratifyCovariates(split = list(spec), filter = NULL)
    group_levels <- levels(as.factor(cov_strat$split))

    if (is.numeric(keep)) {
      # Numeric indices: keep groups at these positions
      keep_groups <- group_levels[keep]
    } else {
      # Character categories: match against group labels
      keep_groups <- group_levels[grepl(paste(keep, collapse = "|"), group_levels)]
    }

    keep_ids <- cov_strat$id[cov_strat$split %in% keep_groups]
    rand_eff_df <- rand_eff_df[rand_eff_df$id %in% keep_ids, ]
  }

  return(rand_eff_df)
}

.stratifyCovariates <- function(split = NULL, filter = NULL, splitbyid = FALSE) {
  ## Get covariates information ------------------------------------------------
  covData <- lixoftConnectors::getCovariateInformation()$covariate
  allcovariates <- .getCovariates(continuous = TRUE, categorical = TRUE)
  covtypes = sapply(allcovariates, .getCovariateType)
  covNames <- allcovariates

  data <- lixoftConnectors::getData()

  if (length(.getOccasions())) {
    occHeader <- .getOccasions()
    covtypes[occHeader] <- "categorical"
    covNames <- c(covNames, occHeader)
  }

  if (is.null(covData)) {
    observations <- lixoftConnectors::getObservationInformation()
    obsNames <- observations$name
    covData <- unique(do.call(
      rbind,
      lapply(obsNames, function(obs) unique(observations[[obs]][intersect(c("id", covNames), names(observations[[obs]]))]))))
  }
  covData$id <- factor(covData$id, levels = unique(covData$id))

  if (splitbyid) {
    idHeader <- data$header[data$headerTypes == "id"]
    allcovariates <- c(allcovariates, idHeader)
    covData[idHeader] <- covData$id
    covtypes[idHeader] <- "categorical"
  }

  ## Check Arguments -----------------------------------------------------------
  split <- .checkSplit(split, "split", splitbyid)
  filter <- .checkFilter(filter)
  # concatenate split and filter infos
  stratifiers <- c(list(), split)
  stratNames <- sapply(stratifiers, function(s) s$name)
  # for (f in filter) {
  #   idxCov <- NULL
  #   if (length(stratifiers)) {
  #     idxCov <- which(sapply(stratifiers, function(s) s$name == f$name))
  #   }
  #   if (length(idxCov) & covtypes[[f$name]] == "categorical") {
  #     stratifiers[[idxCov]] <- f
  #   } else if (length(idxCov) & covtypes[[f$name]] == "continuous") {
  #     stratifiers[[idxCov]]$breaks <- unique(sort(c(stratifiers[[idxCov]]$breaks, f$breaks)))
  #     stratifiers[[idxCov]]$min <- f$min
  #     stratifiers[[idxCov]]$max <- f$max
  #   } else {
  #     stratifiers[[length(stratifiers) + 1]] <- f
  #   }
  # }
  
  if (!is.null(covData)) {
    # split column
    stratDf <- .splitCovariates(covData, covtypes, stratifiers = stratifiers)
    # filter column
    filtDf <- .filter(covData, covtypes, filters = filter)
    
    # add covariates
    covData$split <- stratDf$split
    covData$filter <- filtDf$filter
  } else {
    covData$split <- "All"
    covData$filter <- FALSE
  }
  # remove covariates columns
  if (length(allcovariates)) {
    covData <- covData[! names(covData) %in% setdiff(allcovariates, "id")]
  }
  return(covData)
}

.splitCovariates <- function(covData, covtypes, stratifiers) {
  stratNames <- sapply(stratifiers, function(s) s$name)
  startifyDf <- data.frame(list(split = rep("All", nrow(covData))))
  for (strat in stratifiers) {
    cov <- strat$name
    if (covtypes[[cov]] == "continuous") {
      factor <- cut(covData[[cov]], breaks = strat$breaks, include.lowest=TRUE, right = FALSE)
      levels(factor) <- paste0(cov, ": ", levels(factor), " ")
      
    } else {
      factor <- factor(covData[[cov]])
      if (! is.null(strat$groups)) {
        groups <- strat$groups
        # invert group list
        groups_ <- stats::setNames(names(unlist(groups)), unlist(groups))
        # sort list by names
        groups_ <- stats::setNames(lapply(sort(names(groups_)), FUN = function(n) groups_[[n]]), sort(names(groups_)))
        # rearrange group names
        groups_ <- sapply(
          groups_,
          function(g) ifelse(grepl(paste0("(", paste(names(groups), collapse = "|"), ")[0-9]"), g),
                             gsub("[0-9]$", "", g),
                             g)
        )
        levels(factor) <- paste0(cov, ": ", unlist(groups_[levels(factor)]))
      } else {
        if (cov != .matchToData("id")) {
          levels(factor) <- paste0(cov, ": ", levels(factor))
        }
      }
    }
    startifyDf[[cov]] <- factor
  }
  if (length(stratNames)) {
    startifyDf$split <- as.factor(Reduce(interaction, startifyDf[stratNames]))
  }
  return(startifyDf)
}

.filter <- function(covData, covtypes, filters) {
  startifyDf <- .splitCovariates(covData, covtypes, filters)
  startifyDf$filter <- rep(FALSE, nrow(startifyDf))
  for (filt in filters) {
    cov <- filt$name
    if (covtypes[[cov]] == "continuous") {
      mins <- tapply(covData[[cov]], startifyDf[[cov]], min)
      maxs <- tapply(covData[[cov]], startifyDf[[cov]], max)
      group <- names(mins)[sapply(seq_along(mins), function(i) any(mins[i] >= filt$min & maxs[i] <= filt$max))]
    } else {
      group <- paste0(cov, ": ", filt$cat)
    }
    startifyDf$filter <- startifyDf$filter | (! startifyDf[[cov]] %in% group)
  }
  return(startifyDf)
}

.getIndividualParametersWithVariability <- function(variabilityLevel = NULL) {
  parameterVariability <- lixoftConnectors::getIndividualParameterModel()$variability

  if (! is.null(variabilityLevel)) {
    variabilityLevel[variabilityLevel == "ID"] <- "id"
    parameterVariability <- parameterVariability[variabilityLevel]
  }
  parameters <- unique(do.call(c, lapply(parameterVariability, function(l) names(l[l]))))

  return(parameters)
}

.getCovariates <- function(categorical = FALSE, continuous = FALSE, latent = FALSE) {
  covariates <- NULL
  covTypes <- lixoftConnectors::getCovariateInformation()$type
  
  if (length(covTypes) == 0) return(covariates)

  if (continuous) {
    contcov <- names(which(sapply(covTypes, function(cov) grepl("continuous", cov))))
    covariates <- c(covariates, contcov)
  }

  if (categorical) {
    catcov <- names(which(sapply(covTypes, function(cov) grepl("categorical", cov))))
    covariates <- c(covariates, catcov)
  }

  if (latent) {
    latcov <- names(which(sapply(covTypes, function(cov) grepl("latent", cov))))
    covariates <- c(covariates, latcov)
  }
  
  return(covariates)
}

.getCovariateType <- function(covariate) {
  contCovariates <- .getCovariates(continuous = TRUE)
  catCovariates <- .getCovariates(categorical = TRUE)
  
  if (covariate == .matchToData("id")) {
    type <- "categorical"
  } else if (covariate %in% .getOccasions()) {
    type <- "categorical"
  } else if (covariate %in% catCovariates) {
    type <- "categorical"
  } else if (covariate %in% contCovariates) {
    type <- "continuous"
  } else {
    type <- "latent"
  }
  return(type)
} 

.matchToData <- function(headerMlx, na.omit = FALSE){
  if (na.omit) {
    headerMlx <- .filterHeaders(headerMlx)
  }
  
  data <- lixoftConnectors::getData()
  headersMlx <- data$headerTypes
  headersDataset <- data$header
  
  headerDataset <- c()
  for (h in headerMlx) {
    if (is.element(h, headersMlx)) {
      idxHeader <- which(headersMlx %in% h)
      headerDataset <- c(headerDataset, headersDataset[idxHeader])
    } else {
      headerDataset <- c(headerDataset, NaN)
    }
  }
  
  return(headerDataset)
}

.getOccasions <- function() {
  occasions <- c()
  
  software <- .software()
  
  if (software %in% c("pkanalix", "monolix")) {
    if (.isHeader("occ")) {
      occasions <- .matchToData("occ")
    }
    if (.isHeader("evid")) {
      if ("OCCevid" %in% names(lixoftConnectors::getObservationInformation()[[lixoftConnectors::getObservationInformation()$name[1]]])) {
        occasions <- c(occasions, "OCCevid")
      }
    }
  } else {
    occElements <- lixoftConnectors::getOccasionElements()$names
    if (length(occElements) > 0) {
      occasions <-  occElements
    }
  }
  
  return(occasions)
}

.software <- function (){
  
  softname <- lixoftConnectors::getLixoftConnectorsState()$software
  return(softname)
  
}

.isHeader <- function(headers) {
  return(headers %in% lixoftConnectors::getData()$headerTypes)
}

.renameColumns <- function(df, oldName, newName){
  if (length(oldName) != length(newName)) {
    message("[ERROR] vector of old names and new names must match in size")
    return(df)
  }
  for (i in seq_along(oldName)) {
    old <- oldName[i]
    new <- newName[i]
    if (old %in% names(df)) {
      names(df)[names(df) == old] <- new
    }
  }
  return(df)
}

.filterHeaders <- function(headersList) {
  outputHeadersList <- c()
  for (h in headersList) {
    if (.isHeader(h)) {
      outputHeadersList <- c(outputHeadersList, h)
    } else if (is.element(h, lixoftConnectors::getData()$header)) {
      outputHeadersList <- c(outputHeadersList, h)
    }
  }
  return(outputHeadersList)
}