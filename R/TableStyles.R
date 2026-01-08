################################################################################
#-------------------------------Table Styles-----------------------------------#
################################################################################


print_table_flex <- function(data.flex,
                             grouping.columns = NULL,
                             header_level1    = NULL,
                             header_level2    = NULL,
                             header_level3    = NULL,
                             color            = NULL, # e.g. c("BE","DP","PVAL","RSE","IIVRSE")
                             theme_fn         = theme_box,
                             soften_amount    = 0.50, # 0 = original, 1 = white
                             html_alpha       = 0.18) # only used for html rgba)
{

  color <- toupper(color)  # optional but helpful

  # Check if columns exist
  pval.flag   <- ("PVAL"   %in% color) && "p.value"    %in% colnames(data.flex)
  BE.flag     <- ("BE"     %in% color) && "Conclusion" %in% colnames(data.flex)
  RSE.flag    <- ("RSE"    %in% color) && "%RSE"       %in% colnames(data.flex)
  IIVRSE.flag <- ("IIVRSE" %in% color) && "IIV %RSE"   %in% colnames(data.flex)
  DP.flag     <- ("DP"     %in% color) && "Conclusion" %in% colnames(data.flex)

  flextable::set_flextable_defaults(na_str = "NA")

  # Reorder columns so grouping columns come first
  if (!is.null(grouping.columns)) {
    other.cols <- setdiff(names(data.flex), grouping.columns)
    data.flex  <- data.flex[, c(grouping.columns, other.cols), drop = FALSE]
  }


  ft <- flextable(data.flex)
  #-----------------------------------------------------------------------------
  # [Code Chunk of Alias Conversion]
  # Detect columns containing [[sub:...]] markers
  # Only these columns need processing
  alias_cols <- names(data.flex)[sapply(data.flex, function(col) {
    any(grepl("\\[\\[sub:", col, fixed = FALSE))
  })]

  # Apply decoder to alias columns
  # Identified columns
  if (length(alias_cols) > 0) {
    for (col in alias_cols) {

      # Process each row
      for (i in seq_len(nrow(data.flex))) {
        # CRITICAL: Must process cell-by-cell because flextable::compose() requires
        # specific row (i) and column (j) indices to target individual cells.
        # Cannot apply vectorized transformation to entire column at once.
        cell_value <- data.flex[[col]][i]

        # Handle NA or empty strings
        if (is.na(cell_value) || !nzchar(as.character(cell_value))) {
          next  # Skip, leave as is
        }

        cell_value <- as.character(cell_value)

        # Only process if it contains [[sub:...]]
        if (!grepl("\\[\\[sub:", cell_value)) {
          next
        }

        # --- BEGIN: Inline parsing logic (duplicate of .decode_flex_alias) ---
        # WHY INLINE? Because flextable::compose() needs to be called with specific
        # i/j coordinates for each cell. We can't pre-generate paragraphs and apply
        # them later - compose() must be called in the loop with the parsed chunks.
        pattern <- "\\[\\[sub:([^]]+)\\]\\]"
        chunks <- list()
        current <- cell_value

        repeat {
          m <- regexpr(pattern, current, perl = TRUE)
          if (m[1] == -1) {
            # No more subscript markers - add remaining text
            if (nzchar(current)) {
              chunks <- append(chunks, list(flextable::as_chunk(current)))
            }
            break
          }
          start <- m[1]
          end   <- start + attr(m, "match.length") - 1

          # Text before the [[sub:...]] marker
          before <- substr(current, 1, start - 1)
          if (nzchar(before)) {
            chunks <- append(chunks, list(flextable::as_chunk(before)))
          }
          # Extract subscript text
          match_str <- substr(current, start, end)
          subtxt    <- sub(pattern, "\\1", match_str, perl = TRUE)
          chunks    <- append(chunks, list(flextable::as_sub(subtxt)))

          # Continue with rest of string
          current <- substr(current, end + 1, nchar(current))
        }

        # Apply to this specific cell
        ft <- flextable::compose(ft, i = i, j = col, value = do.call(flextable::as_paragraph, chunks)) # i... row index, j... column name,
      }
    }
  }

  #-----------------------------------------------------------------------------
  # [End of Alias conversion]
  #-----------------------------------------------------------------------------


  #-----------------------------------------------------------------------------
  # Initialize flextable with theme
  ft <- theme_fn(ft)
  ft <- fontsize(ft, size = 8, part = "all")

  # Merge vertical grouping columns
  if (!is.null(grouping.columns)) {
    ft <- merge_v(ft, j = grouping.columns)
  }

  # Center all table content
  ft <- align(ft, align = "center", part = "all")

  # left-align each grouping column by name
  if (!is.null(grouping.columns)) {
    for (col in grouping.columns) {
      ft <- align(ft, j = col, align = "left", part = "all")
    }
  }

  # ---------------------------------------------------------------------------
  # Header levels (1 = topmost)
  # ---------------------------------------------------------------------------
  # Allow passing either a string or a 1-column data.frame
  .as_header_string <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) x <- x[[1]]
    x <- as.character(x)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NULL)
    x[1]  # take first element if someone passes a vector
  }

  h1 <- .as_header_string(header_level1)
  h2 <- .as_header_string(header_level2)
  h3 <- .as_header_string(header_level3)

  # Add from lowest to highest so level 1 ends up on top
  # add_header_lines() inserts ABOVE the existing header each time
  for (line in na.omit(c(h3, h2, h1))) {
    ft <- flextable::add_header_lines(ft, values = line)
  }

  ft <- flextable::align(ft, align = "center", part = "header")


  # ---------------------------------------------------------------------------
  # SOFT COLOR PALETTE (embedded)
  # ---------------------------------------------------------------------------
  soften_hex <- function(hex, amount = 0.8) {
    rgb <- grDevices::col2rgb(hex) / 255
    new <- rgb + (1 - rgb) * amount
    grDevices::rgb(new[1], new[2], new[3])
  }

  rgba <- function(hex, alpha = 0.2) {
    rgb <- grDevices::col2rgb(hex)
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
  }

  is_html <- function() {
    tryCatch(knitr::is_html_output(), error = function(e) FALSE)
  }

  base_yellow <- "#FFD700"
  base_orange <- "#EE7600"
  base_red    <- "#8B1A1A"
  base_green  <- "#458B00"

  # Default: pastel hex (good for docx/pptx and generally safe)
  col_yellow <- soften_hex(base_yellow, soften_amount)
  col_orange <- soften_hex(base_orange, soften_amount)
  col_red    <- soften_hex(base_red,    soften_amount)
  col_green  <- soften_hex(base_green,  soften_amount)

  # If HTML output, optionally use rgba for transparency
  if (is_html()) {
    col_yellow <- rgba(base_yellow, html_alpha)
    col_orange <- rgba(base_orange, html_alpha)
    col_red    <- rgba(base_red,    html_alpha)
    col_green  <- rgba(base_green,  html_alpha)
  }

  # Apply conditional background formatting
  if (RSE.flag) {
    ft <- ft %>%
      flextable::bg(i = ~ `%RSE` >= 50  & `%RSE` < 100, j = ~ `%RSE`, bg = col_yellow, part = "body") %>%
      flextable::bg(i = ~ `%RSE` >= 100 & `%RSE` < 200, j = ~ `%RSE`, bg = col_orange, part = "body") %>%
      flextable::bg(i = ~ `%RSE` > 200,                 j = ~ `%RSE`, bg = col_red,    part = "body")
  }

  if (IIVRSE.flag) {
    ft <- ft %>%
      flextable::bg(i = ~ `IIV %RSE` >= 50  & `IIV %RSE` < 100, j = ~ `IIV %RSE`, bg = col_yellow, part = "body") %>%
      flextable::bg(i = ~ `IIV %RSE` >= 100 & `IIV %RSE` < 200, j = ~ `IIV %RSE`, bg = col_orange, part = "body") %>%
      flextable::bg(i = ~ `IIV %RSE` > 200,                     j = ~ `IIV %RSE`, bg = col_red,    part = "body")
  }

  if (pval.flag) {
    ft <- ft %>%
      flextable::bg(i = ~ p.value <= 0.01,                  j = ~ p.value, bg = col_red,    part = "body") %>%
      flextable::bg(i = ~ p.value > 0.01 & p.value <= 0.05, j = ~ p.value, bg = col_orange, part = "body") %>%
      flextable::bg(i = ~ p.value > 0.05 & p.value <= 0.09, j = ~ p.value, bg = col_yellow, part = "body")
  }
  # Apply conditional background formatting
  if (DP.flag) {
    ft <- ft %>%
      flextable::bg(i = ~ Conclusion == "Proportional",     j = ~ Conclusion, bg = col_green,  part = "body") %>%
      flextable::bg(i = ~ Conclusion == "Inconclusive",     j = ~ Conclusion, bg = col_yellow, part = "body") %>%
      flextable::bg(i = ~ Conclusion == "Not Proportional", j = ~ Conclusion, bg = col_red,    part = "body")
  }
  if (BE.flag) {
    ft <- ft %>%
      flextable::bg(i = ~ Conclusion == "Bioequivalent",     j = ~ Conclusion, bg = col_green, part = "body") %>%
      flextable::bg(i = ~ Conclusion == "Not bioequivalent", j = ~ Conclusion, bg = col_red,   part = "body")
  }
  ft <- set_table_properties(ft, width = 1, layout = "autofit")
  return(ft)
}

################################################################################
# Vanilla table style  = theme_vanilla()
# Zebra table style    = theme_zebra()
# Booktabs table style = theme_booktabs()
# Box table style      = theme_box(ft)
# Thron table style    = theme_tron()
################################################################################
