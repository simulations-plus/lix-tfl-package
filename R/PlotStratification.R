# Split & stratify data
.transformChartData <- function(data, ids = NULL, split = NULL, color = NULL,
                                filter = NULL, removeFiltered = TRUE) {

  data <- data[! names(data) %in% c("split", "color", "filter")]
  data <- unique(data)

  # rename id
  data <- .renameColumns(data, "ID", "id")

  # split and filter
  covDataSplit <- .stratifyCovariates(split = split, filter = filter)
  
  # color
  covDataColor <- .stratifyCovariates(split = color, filter = filter, splitbyid = TRUE)
  covDataColor <- .renameColumns(covDataColor, "split", "color")
  
  covData <- merge(covDataSplit, covDataColor)
  
  # Ids filtering
  if (!is.null(ids)) {
    .checkIds(ids)
    data <- data[data$id %in% ids,]
    covData <- covData[covData$id %in% ids,]
  }
  
  data$id = factor(data$id, levels = unique(data$id))

  # add filter and split columns to data
  data <- merge(data, covData)

  # sort dataframe
  for (h in intersect(names(data), c("rep", .getOccasions(), "censored"))) data[[h]] <- as.integer(data[[h]])
  for (h in intersect(names(data), c("time"))) data[[h]] <- as.double(data[[h]])

  tosort <- intersect(c("rep", .getSubjocc(), "time", "split"), names(data))
  if (length(tosort)) {
    if (length(tosort) == 1) {
      data <- data[order(data[[tosort]]),]
    } else {
      data <- data[do.call(order, data[,tosort]),] 
    }
  }
  data$split <- as.factor(data$split)
  data$color <- as.factor(data$color)
  data$filter <- as.integer(data$filter)
  data$filter <- as.factor(data$filter)
  
  # reorder columns
  data <- data[, c(setdiff(names(data), c("split", "color", "filter")), "split", "color", "filter")]

  # filter dataframe on filter column
  if (removeFiltered) {
    data <- data[data$filter == 0,]
  }
  
  if (nrow(data[data$filter == 0,]) == 0) {
    warning("After applying filtering, there is no more data to display.", call. = FALSE)
  }

  row.names(data) <- NULL

  return(data)
}

.stratifyCovariates <- function(split = NULL, filter = NULL, splitbyid = FALSE) {
  ## Get covariates information ------------------------------------------------
  covData <- getCovariateInformation()$covariate
  allcovariates <- .getCovariates(continuous = TRUE, categorical = TRUE)
  covtypes = sapply(allcovariates, .getCovariateType)
  covNames <- allcovariates

  data <- getData()

  if (length(.getOccasions())) {
    occHeader <- .getOccasions()
    covtypes[occHeader] <- "categorical"
    covNames <- c(covNames, occHeader)
  }

  if (is.null(covData)) {
    observations <- getObservationInformation()
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

# read dataset -----------------------------------------------------------------
.readDataset <- function(filename, sheet = NULL) {
  # check that file exists
  filename <- .check_file(filename)

  # check file extension
  fileext <- utils::tail(strsplit(filename, "\\.")[[1]], n=1)
  .check_in_vector(fileext, paste(filename, "extension"), c("txt", "csv"))

  # get separator
  sep <- .getDelimiter(filename)
  if (is.null(sep)) {
    stop(paste0("Cannot read file ", filename, "."))
  }
  
  # read file
  df <- utils::read.csv(filename, sep = sep, check.names=FALSE)

  return(df)
}

.getIndividualParametersWithVariability <- function(variabilityLevel = NULL) {
  parameterVariability <- getIndividualParameterModel()$variability

  if (! is.null(variabilityLevel)) {
    variabilityLevel[variabilityLevel == "ID"] <- "id"
    parameterVariability <- parameterVariability[variabilityLevel]
  }
  parameters <- unique(do.call(c, lapply(parameterVariability, function(l) names(l[l]))))

  return(parameters)
}

.getCensoredInterval <- function(obsName = NULL, subjoccs = NULL) {
  cens <- .transformToNumeric(
    getObservationInformation()[[obsName]],
    headers = c("time", obsName, "censoredInterval_min", "censoredInterval_max")
  )
  cens <- cens[cens$censored == TRUE,]

  # filter dataframe
  if (! is.null(subjoccs)) {
    cens <- merge(cens, subjoccs)
  }
  
  intervalCensored <- NULL
  rightCensored <- NULL
  leftCensored <- NULL

  if (! "censored" %in% names(cens)) {
    return(leftCensored = leftCensored,
           rightCensored = rightCensored,
           intervalCensored = intervalCensored
    )
  }

  # left censored interval
  leftCensoredData <- cens[cens$censoredInterval_min != "-Infinity" & cens$censoredInterval_max == "Infinity",]
  if (nrow(leftCensoredData)) {
    leftCensored <- c(- Inf, max(leftCensoredData$censoredInterval_max))
    leftCensored <- as.numeric(leftCensored)
  }
  
  # right censored interval
  rightCensoredData <- cens[cens$censoredInterval_min == "-Infinity" & cens$censoredInterval_max != "Infinity",]
  if (nrow(rightCensoredData)) {
    rightCensored <- c(min(rightCensoredData$censoredInterval_min), Inf)
    rightCensored <- as.numeric(rightCensored)
  }
  
  # interval censored interval
  intervalCensoredData <- cens[cens$censoredInterval_min != "-Infinity" & cens$censoredInterval_max != "Infinity",]
  if (nrow(intervalCensoredData)) {
    intervalCensored <- c(min(intervalCensoredData$censoredInterval_min),
                          max(intervalCensoredData$censoredInterval_max))
    intervalCensored <- as.numeric(intervalCensored)
  }

  return(list(
    leftCensored = leftCensored,
    rightCensored = rightCensored,
    intervalCensored = intervalCensored
  ))
}

.getCovariates <- function(categorical = FALSE, continuous = FALSE, latent = FALSE) {
  covariates <- NULL
  covTypes <- getCovariateInformation()$type
  
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
  
  data <- getData()
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
      if ("OCCevid" %in% names(getObservationInformation()[[getObservationInformation()$name[1]]])) {
        occasions <- c(occasions, "OCCevid")
      }
    }
  } else {
    occElements <- getOccasionElements()$names
    if (length(occElements) > 0) {
      occasions <-  occElements
    }
  }
  
  return(occasions)
}

.software <- function (){
  
  softname <- getLixoftConnectorsState()$software
  return(softname)
  
}

.isHeader <- function(headers) {
  return(headers %in% getData()$headerTypes)
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
    } else if (is.element(h, getData()$header)) {
      outputHeadersList <- c(outputHeadersList, h)
    }
  }
  return(outputHeadersList)
}