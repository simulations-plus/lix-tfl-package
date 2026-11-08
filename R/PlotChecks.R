# Check Split list has correct elements and correct fields
#' @importFrom stats na.omit
.checkSplit <- function(split, argname = "group", splitbyid = FALSE) {
  if (is.null(split)) {
    return(split)
  }
  
  covariates <- lixoftConnectors::getCovariateInformation()$covariate
  covNames <- .getCovariates(continuous = TRUE, categorical = TRUE)

  # When only one output --> create a list of output
  if (is.element("name", names(split))) {
    split <- list(split)
  }
  
  # Split must be a list of lists
  if (!is.list(split)) {
    stop("Invalid '", argname, "' argument. It must be a list object.", call. = FALSE)
  }
  
  if (!all(sapply(split, function(s) is.list(s)))) {
    stop("Invalid '", argname, "' list. All elements must be lists with at least a field 'name'.", call. = FALSE)
  }
  
  allowedSplit <- c(covNames, .getOccasions())
  if (splitbyid) {
    allowedSplit <- c(allowedSplit, .matchToData("id", na.omit = TRUE))
  }
  covtypes = sapply(allowedSplit, .getCovariateType)

  splitNames <- sapply(split, function(s) s$name)
  
  if (is.null(allowedSplit)) {
    stop("Invalid '", argname, "' list. There is no stratifier (covariates or occasions) in the dataset.", call. = FALSE)
  }
  
  if (any(is.null(splitNames))) {
    stop("Invalid '", argname, "' list. All elements must be lists with at least a field 'name'.", call. = FALSE)
  }
  
  if (any(!is.element(splitNames, allowedSplit))) {
    stop("Invalid '", argname, "' list. Unkonwn names: ", paste(setdiff(splitNames, allowedSplit), collapse = ", "),
         ". You must select splits in {'", paste(allowedSplit, collapse = "', '"), "'}", call. = FALSE)
  }
  
  for (i in seq_along(split)) {
    s <- split[[i]]
    sname <- s$name
    # categorical splits
    if (covtypes[[sname]] == "categorical") {
      .check_list(s, argname = paste("Categorical ", argname, " element", sname),
                  listNames = "name",
                  optionalNames = "groups")

      if (is.null(covariates)) {
        covariates <- unique(do.call(
          rbind,
          lapply(lixoftConnectors::getObservationInformation()$name,
                 function(obs) unique(lixoftConnectors::getObservationInformation()[[obs]][intersect(.getSubjocc(), names(lixoftConnectors::getObservationInformation()[[obs]]))]))
        ))
      }

      groups <- s$groups
      covValues <- levels(as.factor(cbind(stats::setNames(list(covariates$id), .matchToData("id")), covariates)[[sname]]))
      if (! is.null(groups)) {
        if (! is.list(groups)) {
          groups <- list(groups)
        }
        .check_in_vector(unlist(groups),
                         paste("Categorical ", argname, " element", sname, "groups"),
                         covValues)
        # add missing covariates
        for (cov in setdiff(covValues, unlist(groups))) {
          groups[cov] <- cov
        }
        group_names = sapply(groups, function(g) return(paste(g, collapse = " - ")))
        if (is.null(names(groups))) {
          names(groups) = group_names
        } else {
          names(groups)[names(groups) == ""] <- group_names[names(groups) == ""]
        }
        s$groups <- groups
      }
      split[[i]] <- s[names(s) %in% c("name", "groups")]
    }
    
    # continuous splits
    if (covtypes[[sname]] == "continuous") {
      .check_list(s, argname = paste("Continuous ", argname, " element", sname),
                  listNames = c("name", "breaks"))
      .check_vector_of_double(s$breaks, paste("breaks field in ", argname, " element", sname))
      s$breaks <- unique(sort(c(min(covariates[[sname]]), s$breaks, max(covariates[[sname]]))))
      split[[i]] <- s[names(s) %in% c("name", "breaks")]
    }
  }
  return(split)
}

# check if an argument is a list with the required fields
.check_list <- function(l, argname, listNames = NULL, optionalNames = NULL) {
  if (is.null(l)) {
    return(l)
  }

  if (!is.list(l)) {
    stop("Invalid ", argname, ". It must be a list.", call. = FALSE)
  }

  if (!is.null(listNames)) {

    if (!all(is.element(listNames, names(l)))) {
      stop("Invalid '", argname, "'. Missing mandatory fields : ",
           paste(setdiff(listNames, names(l)), collapse = ", "), ".", call. = FALSE)
    }

    if (!all(is.element(names(l), c(listNames, optionalNames)))) {
      warning("Invalid fields in '", argname, "' will be ignored. ",
              paste(setdiff(names(l), listNames), collapse = ", "), " will be ignored.",
              call. = F)
    }
  }
  return(invisible(TRUE))
}

# Check Filter list has correct elements and correct fields
.checkFilter <- function(filter, argname = "filter") {
  if (is.null(filter)) {
    return(filter)
  }
  
  cov <- lixoftConnectors::getCovariateInformation()$covariate
  covNames <- .getCovariates(continuous = TRUE, categorical = TRUE)

  # When only one output --> create a list of output
  if (is.element("name", names(filter))) {
    filter <- list(filter)
  }
  # Filter must be a list of lists
  if (!is.list(filter)) {
    stop("Invalid '", argname, "' argument. It must be a list object.", call. = FALSE)
  }
  
  if (!all(sapply(filter, function(f) is.list(f)))) {
    stop("Invalid '", argname, "' list. All filter elements must be lists with at least a field 'name'.", call. = FALSE)
  }
  
  allowedFilt <- c(covNames, .matchToData("occ", na.omit = TRUE))
  covtypes <- sapply(allowedFilt, .getCovariateType)

  filtNames <- sapply(filter, function(f) f$name)
  
  if (is.null(allowedFilt)) {
    stop("Invalid '", argname, "' list. There is no stratifier (covariates or occasions) in the dataset.", call. = FALSE)
  }
  
  if (any(is.null(filtNames))) {
    stop("Invalid '", argname, "' list. All filter elements must be lists with at least a field 'name'.", call. = FALSE)
  }
  
  if (any(!is.element(filtNames, allowedFilt))) {
    stop("Invalid '", argname, "' list. Unkonwn filter names: ", paste(setdiff(filtNames, allowedFilt), collapse = ", "),
         ". You must select filters in {'", paste(allowedFilt, collapse = "', '"), "'}", call. = FALSE)
  }
  
  for (i in seq_along(filter)) {
    f <- filter[[i]]
    fname <- f$name
    
    # categorical splits
    if (covtypes[[fname]] == "categorical") {
      .check_list(f, argname = paste("Categorical filter element", fname), listNames = c("name", "cat"))
      .check_in_vector(f$cat, argname = paste("Categories in filter element", fname), allowed_args = unique(cov[[fname]]))
      filter[[i]] <- f[names(f) %in% c("name", "cat")]
    }
    
    # continuous splits
    if (covtypes[[fname]] == "continuous") {
      .check_list(f, argname = paste("Continuous filter element", fname), listNames = c("name", "interval"))
      if (is.vector(f$interval) & length(f$interval) == 2) f$interval <- list(f$interval)
      .check_list(f$interval, argname = paste("interval of continuous filter element", fname))
      for (interval in f$interval) {
        if (!all(sapply(f$interval, function(i) length(i) == 2))) {
          stop("Invalid filter element", fname, ". Interval elements must be of length 2: c(min, max) with min < max", call. = FALSE)
        }
        if (!all(sapply(f$interval, function(i) i[1] < i[2]))) {
          stop("Invalid filter element", fname, ". Interval elements must be of length 2: c(min, max) with min < max", call. = FALSE)
        }
      }
      f$min <- sapply(f$interval, function(i) i[1])
      f$max <- sapply(f$interval, function(i) i[2])
      f$breaks <- unique(sort(c(min(cov[[fname]]), do.call(c, f$interval), max(cov[[fname]]))))
      f$breaks <- f$breaks[(f$breaks >= min(cov[[fname]])) & (f$breaks <= max(cov[[fname]]))]
      filter[[i]] <- f[names(f) %in% c("name", "interval", "breaks", "min", "max")]
    }
  }
  return(filter)
}

# check if an argument contains allowed elements -------------------------------
.check_in_vector <- function(arg, argname, allowed_args) {
  if (is.null(arg)) {
    return(arg)
  }

  if (is.null(arg)) {
    return(invisible(TRUE))
  }

  if (! all(is.element(arg, allowed_args))) {
    stop("Invalid ", argname, ": ", paste(setdiff(arg, allowed_args), collapse = ", "),
         ". Available elements: '", 
         paste(allowed_args, collapse="', '"), "'", call. = FALSE)
  }
  return(invisible(TRUE))
}

# check if a vector contains only doubles --------------------------------------
.check_vector_of_double <- function(v, argname) {
  if (is.null(v)) {
    return(v)
  }

  if(!(is.double(v)||is.integer(v))) {
    stop("Invalid ", argname, ". It must be a vector of doubles.", call. = F)
  }
  return(v)
}