################################################################################
#-------------------------------Table Styles-----------------------------------#
################################################################################

# ---------------------------------------------------------------------------
# Internal helper functions
# ---------------------------------------------------------------------------

#' Convert header input to string
#' Helper function that converts header input to a single string value.
#' Handles data.frame or character input and extracts the first non-NA element.
#' @param x Input value. Can be NULL, data.frame, or character vector.
#' @return A single character string, or NULL if input is NULL or empty.
#' @keywords internal
.as_header_string <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.data.frame(x)) x <- x[[1]]
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NULL)
  }
  x[1] # take first element if someone passes a vector
}

################################################################################
#' Soften hex color by blending with white
#' Blends a hex color with white to create a softer, pastel version.
#' Primarily used for docx/pptx output where softer colors are preferred.
#' @param hex Character string specifying a hex color code (e.g., "#FFD700").
#' @param amount Numeric value between 0 and 1 specifying the blend amount. 0 = original color, 1 = white. Default is 0.8.
#' @return A character string with the softened hex color code.
#' @keywords internal
.soften_hex <- function(hex, amount = 0.8) {
  rgb <- grDevices::col2rgb(hex) / 255
  new <- rgb + (1 - rgb) * amount
  grDevices::rgb(new[1], new[2], new[3])
}

################################################################################
#' Convert hex color to rgba format
#' Converts a hex color code to rgba format with transparency.
#' Primarily used for HTML output where rgba colors with alpha transparency
#' provide better visual results.
#' @param hex Character string specifying a hex color code (e.g., "#FFD700").
#' @param alpha Numeric value between 0 and 1 specifying the alpha transparency. 0 = fully transparent, 1 = fully opaque. Default is 0.2.
#' @return A character string with the rgba color specification (e.g., "rgba(255,215,0,0.20)").
#' @keywords internal
.hex_to_rgba <- function(hex, alpha = 0.2) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

################################################################################
#' Check if output format is HTML
#' Detects whether the current knitr rendering context is HTML output.
#' Used to determine whether to apply HTML-specific formatting (like rgba colors).
#' @return Logical; `TRUE` if HTML output, `FALSE` otherwise.
#' @keywords internal
.is_html_output <- function() {
  tryCatch(knitr::is_html_output(), error = function(e) FALSE)
}

################################################################################
#' .apply_alias_subscripts: Apply subscript formatting to flextables
#'
#' Processes [[sub:...]] markers in both body cells and column headers, converting
#' them to properly formatted subscripts using flextable::compose().
#'
#' @param ft Flextable object to apply subscript formatting to
#' @param data.flex Data frame containing the table data (used to detect markers)
#'
#' @return Modified flextable object with subscripts applied
#'
#' @details
#' This function handles two cases:
#' - **Body cells**: Processes [[sub:...]] markers in data values (horizontal = TRUE format)
#' - **Column headers**: Processes [[sub:...]] markers in column names (horizontal = FALSE format)
#'
#' The parsing logic extracts subscript text from markers and uses flextable::as_sub()
#' to render them correctly in the final output.
#'
#' @keywords internal
.apply_alias_subscripts <- function(ft, data.flex) {
	#-----------------------------------------------------------------------------
	# [Process Body Cells]
	# Detect columns containing [[sub:...]] markers
	#-----------------------------------------------------------------------------
	alias_cols <- names(data.flex)[sapply(data.flex, function(col) {
		any(grepl("\\[\\[sub:", col, fixed = FALSE))
	})]

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
					next # Skip, leave as is
				}

				cell_value <- as.character(cell_value)

				# Only process if it contains [[sub:...]]
				if (!grepl("\\[\\[sub:", cell_value)) {
					next
				}

				# --- BEGIN: Inline parsing logic ---
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
					end <- start + attr(m, "match.length") - 1

					# Text before the [[sub:...]] marker
					before <- substr(current, 1, start - 1)
					if (nzchar(before)) {
						chunks <- append(chunks, list(flextable::as_chunk(before)))
					}
					# Extract subscript text
					match_str <- substr(current, start, end)
					subtxt <- sub(pattern, "\\1", match_str, perl = TRUE)
					chunks <- append(chunks, list(flextable::as_sub(subtxt)))

					# Continue with rest of string
					current <- substr(current, end + 1, nchar(current))
				}

				# Apply to this specific cell
				ft <- flextable::compose(ft, i = i, j = col, value = do.call(flextable::as_paragraph, chunks))
			}
		}
	}

	#-----------------------------------------------------------------------------
	# [Process Column Headers]
	# Process column names that contain [[sub:...]] markers
	# This handles the case when horizontal = FALSE (parameters in headers)
	#-----------------------------------------------------------------------------
	header_names <- colnames(data.flex)
	header_alias_cols <- which(grepl("\\[\\[sub:", header_names))

	if (length(header_alias_cols) > 0) {
		for (col_idx in header_alias_cols) {
			header_value <- header_names[col_idx]

			# Only process if it contains [[sub:...]]
			if (!grepl("\\[\\[sub:", header_value)) {
				next
			}

			# --- BEGIN: Inline parsing logic (same as body processing) ---
			pattern <- "\\[\\[sub:([^]]+)\\]\\]"
			chunks <- list()
			current <- header_value

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
				end <- start + attr(m, "match.length") - 1

				# Text before the [[sub:...]] marker
				before <- substr(current, 1, start - 1)
				if (nzchar(before)) {
					chunks <- append(chunks, list(flextable::as_chunk(before)))
				}
				# Extract subscript text
				match_str <- substr(current, start, end)
				subtxt <- sub(pattern, "\\1", match_str, perl = TRUE)
				chunks <- append(chunks, list(flextable::as_sub(subtxt)))

				# Continue with rest of string
				current <- substr(current, end + 1, nchar(current))
			}

			# Apply to this header cell (i = 1 for first header row, j = col_idx)
			ft <- flextable::compose(ft, i = 1, j = col_idx, value = do.call(flextable::as_paragraph, chunks), part = "header")
		}
	}

	return(ft)
}

################################################################################
#' .add_section_headers: Add section headers to parameter tables (population or NCA)
#' For parameter tables (identified by `table_type` attribute), inserts section
#' header rows to visually separate different sections.
#'
#' **Population tables:** Fixed effects, Random effects (IIV/IOV), Correlations, Residual error
#' **NCA tables:** Summary statistics, Individual data (Parameters/Concentrations/Ratios)
#'
#' @param data.flex Data frame with parameter estimates. Must have:
#'   - Attribute `table_type` set to "pop" or "nca_*" (nca_parameters,
#'     nca_concentration, nca_ratio)
#'   - Attribute `format` indicating "wide" or "long" format
#'   - Column `Section` with section codes:
#'     - Pop: FE, IIV, IOV, CORR, CORR_IIV, CORR_IOV, RES_ERR
#'     - NCA: SUMMARY, INDIVIDUAL
#' @param grouping.columns Character vector of column names that are used for vertical
#'   merging. When provided, section header rows will preserve values in these columns
#'   (for vertical merging) and only span across non-grouping columns. Only applies to NCA tables.
#'
#' @return A list with four elements:
#'   \describe{
#'     \item{data.flex}{Modified data frame with section header rows inserted}
#'     \item{section_header_rows}{Integer vector of row indices where section headers were inserted}
#'     \item{wide_split_col}{Integer; column index where "Random effects" section starts in wide format (NULL for long format)}
#'     \item{is_wide}{Logical; `TRUE` if table is in wide format, `FALSE` otherwise}
#'   }
#'
#' @details
#' **Population - Wide format:** Creates a split header at the top ("Fixed effects | Random effects")
#' and adds separate section headers only for Correlations and Residual error sections.
#' The split column is determined by finding the first column containing "IIV" or "IOV".
#'
#' **Population - Long format:** Adds section headers for all sections (Fixed effects, Random effects,
#' Correlations, Residual error). If IOV parameters are present, IIV and IOV get separate
#' headers ("Random effects IIV" and "Random effects IOV").
#'
#' **NCA tables:** Adds section headers for "Summary Statistics" and individual data
#' ("Individual Parameters", "Individual Concentrations", or "Individual Ratios"
#' depending on table type) in long format only.
#'
#' **Grouping columns (NCA only):** When grouping.columns is provided for NCA tables, section header
#' rows preserve values in grouping columns (for vertical merging) and only span non-grouping columns.
#' @keywords internal
.add_section_headers <- function(data.flex, grouping.columns = NULL) {
	# Check table type

	table_type <- attr(data.flex, "table_type")
	is_pop_table <- !is.null(table_type) && table_type == "pop"
	is_nca_table <- !is.null(table_type) && grepl("^nca_", table_type)
	table_format <- attr(data.flex, "format")
	is_wide <- !is.null(table_format) && table_format == "wide"

	# Initialize return values
	section_header_rows <- integer(0)
	wide_split_col <- NULL

	# If not a pop or nca table, or no Section column, return unchanged
	if ((!is_pop_table && !is_nca_table) || !"Section" %in% colnames(data.flex)) {
		return(list(
			data.flex           = data.flex,
			section_header_rows = section_header_rows,
			wide_split_col      = wide_split_col,
			is_wide             = is_wide
		))
	}

	# Save Section column before processing
	section_col <- as.character(data.flex$Section)

	# ============================================================================
	# NCA TABLE HANDLING (simpler than population tables)
	# ============================================================================
	if (is_nca_table) {
		# NCA tables only have long format with SUMMARY and INDIVIDUAL sections
		# Determine individual section label based on specific table type
		individual_label <- switch(table_type,
			"nca_parameters"    = "Individual Parameters",
			"nca_concentration" = "Individual Concentrations",
			"nca_ratio"         = "Individual Ratios",
			"Individual Data"   # fallback
		)
		section_labels <- c(
			SUMMARY    = "Summary Statistics",
			INDIVIDUAL = individual_label
		)

		# Convert factor columns to character to avoid issues when inserting header rows
		for (col in names(data.flex)) {
			if (is.factor(data.flex[[col]])) {
				data.flex[[col]] <- as.character(data.flex[[col]])
			}
		}

		# Move Section column to the end
		other_cols <- setdiff(names(data.flex), "Section")
		data.flex <- data.flex[, c(other_cols, "Section"), drop = FALSE]

		# Map section codes to full labels in the data
		data.flex$Section <- section_labels[section_col]

		# Identify where to insert section headers
		insert_positions <- integer(0)
		section_names <- character(0)

		for (i in seq_len(nrow(data.flex))) {
			if (i == 1 || data.flex$Section[i] != data.flex$Section[i - 1]) {
				insert_positions <- c(insert_positions, i)
				section_names <- c(section_names, data.flex$Section[i])
			}
		}

		# Insert header rows from bottom to top
		for (idx in length(insert_positions):1) {
			pos <- insert_positions[idx]
			section_name <- section_names[idx]

			# Create header row
			header_row <- data.flex[pos, , drop = FALSE]

			# Handle grouping columns if provided
			if (!is.null(grouping.columns) && length(grouping.columns) > 0) {
				# Keep grouping column values from data row for vertical merging
				for (grp_col in grouping.columns) {
					if (grp_col %in% names(header_row)) {
						header_row[1, grp_col] <- data.flex[pos, grp_col]
					}
				}

				# Empty all non-grouping columns
				non_grouping_cols <- setdiff(names(header_row), grouping.columns)
				for (col in non_grouping_cols) {
					header_row[1, col] <- ""
				}

				# Put section header text in first non-grouping column
				if (length(non_grouping_cols) > 0) {
					header_row[1, non_grouping_cols[1]] <- section_name
				}
			} else {
				# No grouping columns: use existing logic (clear all, put text in first column)
				header_row[1, ] <- ""
				header_row[1, 1] <- section_name
			}

			# Insert before the position
			if (pos == 1) {
				data.flex <- rbind(header_row, data.flex)
			} else {
				data.flex <- rbind(
					data.flex[1:(pos - 1), , drop = FALSE],
					header_row,
					data.flex[pos:nrow(data.flex), , drop = FALSE]
				)
			}

			# Track final position for styling
			section_header_rows <- c(pos + (idx - 1), section_header_rows)
		}
		rownames(data.flex) <- NULL

		# Remove the Section column
		data.flex$Section <- NULL

		return(list(
			data.flex           = data.flex,
			section_header_rows = section_header_rows,
			wide_split_col      = wide_split_col,
			is_wide             = FALSE
		))
	}

	# ============================================================================
	# POPULATION TABLE HANDLING
	# ============================================================================
	if (is_wide) {
		# Wide format: split header at top + section headers for CORR/RES_ERR
		# Determine labels for non-FE/IIV/IOV sections
		section_labels <- c(
			FE_CAT   = "Fixed effects by category",
			CORR     = "Correlations",
			CORR_IIV = "Correlations IIV",
			CORR_IOV = "Correlations IOV",
			RES_ERR  = "Residual error",
			LAT      = "Latent probabilities")

		# Move Section column to the end
		other_cols <- setdiff(names(data.flex), "Section")
		data.flex <- data.flex[, c(other_cols, "Section"), drop = FALSE]

		# Find first column containing "IIV" or "IOV" (AFTER reordering)
		iiv_iov_cols <- grep("IIV|IOV", colnames(data.flex), ignore.case = TRUE)
		if (length(iiv_iov_cols) > 0) {
			wide_split_col <- min(iiv_iov_cols)
		}

		# Map section codes to labels (use section_col which is already character)
		data.flex$Section <- ifelse(section_col %in% names(section_labels),
																section_labels[section_col],
																section_col
		)

		# Find where to insert section headers (for CORR and RES_ERR only)
		insert_positions <- integer(0)
		section_names <- character(0)

		for (i in seq_len(nrow(data.flex))) {
			if (i == 1 || isTRUE(data.flex$Section[i] != data.flex$Section[i - 1])) {
				# Only add headers for CORR and RES_ERR sections
				if (!is.na(data.flex$Section[i]) && data.flex$Section[i] %in% section_labels) {
					insert_positions <- c(insert_positions, i)
					section_names <- c(section_names, data.flex$Section[i])
				}
			}
		}

		# Insert header rows from bottom to top
		if (length(insert_positions) > 0) {
			for (idx in length(insert_positions):1) {
				pos <- insert_positions[idx]
				section_name <- section_names[idx]

				# Create header row
				header_row <- data.flex[pos, , drop = FALSE]
				header_row[1, ] <- ""
				header_row[1, 1] <- section_name

				# Insert before the position
				if (pos == 1) {
					data.flex <- rbind(header_row, data.flex)
				} else {
					data.flex <- rbind(
						data.flex[1:(pos - 1), , drop = FALSE],
						header_row,
						data.flex[pos:nrow(data.flex), , drop = FALSE]
					)
				}

				# Track position after all insertions
				section_header_rows <- c(pos + (idx - 1), section_header_rows)
			}
			rownames(data.flex) <- NULL
		}

		# Remove the Section column
		data.flex$Section <- NULL

		# Add split header at the very top
		if (!is.null(wide_split_col)) {
			header_row <- data.flex[1, , drop = FALSE]
			header_row[1, ] <- ""

			data.flex <- rbind(header_row, data.flex)
			section_header_rows <- c(1, section_header_rows + 1)
			rownames(data.flex) <- NULL
		}
	} else {
		# Long format: multiple section headers
		# Determine labels based on actual data
		has_IOV <- any(section_col == "IOV")
		section_labels <- c(
			FE       = "Fixed effects",
			FE_CAT   = "Fixed effects by category",
			IIV      = if (has_IOV) "Random effects IIV" else "Random effects",
			IOV      = "Random effects IOV",
			CORR     = "Correlations",
			CORR_IIV = "Correlations IIV",
			CORR_IOV = "Correlations IOV",
			RES_ERR  = "Residual error",
			LAT      = "Latent probabilities")

		# Move Section column to the end
		other_cols <- setdiff(names(data.flex), "Section")
		data.flex <- data.flex[, c(other_cols, "Section"), drop = FALSE]

		# Map section codes to full labels in the data
		data.flex$Section <- section_labels[section_col]

		# Identify where to insert section headers
		insert_positions <- integer(0)
		section_names <- character(0)

		for (i in seq_len(nrow(data.flex))) {
			if (i == 1 || data.flex$Section[i] != data.flex$Section[i - 1]) {
				insert_positions <- c(insert_positions, i)
				section_names <- c(section_names, data.flex$Section[i])
			}
		}

		# Insert header rows from bottom to top
		for (idx in length(insert_positions):1) {
			pos <- insert_positions[idx]
			section_name <- section_names[idx]

			# Create header row
			header_row <- data.flex[pos, , drop = FALSE]
			header_row[1, ] <- ""
			header_row[1, 1] <- section_name

			# Insert before the position
			if (pos == 1) {
				data.flex <- rbind(header_row, data.flex)
			} else {
				data.flex <- rbind(
					data.flex[1:(pos - 1), , drop = FALSE],
					header_row,
					data.flex[pos:nrow(data.flex), , drop = FALSE]
				)
			}

			# Track final position for styling
			section_header_rows <- c(pos + (idx - 1), section_header_rows)
		}
		rownames(data.flex) <- NULL

		# Remove the Section column
		data.flex$Section <- NULL
	}

	return(list(
		data.flex           = data.flex,
		section_header_rows = section_header_rows,
		wide_split_col      = wide_split_col,
		is_wide             = is_wide
	))
}

################################################################################
#' Generate automatic footnotes from data frame attributes
#'
#' Scans data frame attributes for known footnote triggers and generates
#' appropriate footnote messages. Currently supports time deviation filtering
#' attributes from `tableNCAConcentration`.
#'
#' @param attrs Named list of attributes (from `attributes(data.flex)`)
#'
#' @return Character vector of footnote messages (empty vector if none)
#'
#' @details
#' Currently recognized attributes:
#' \itemize{
#'   \item `timeDeviationFiltered`: If `TRUE`, generates footnote about excluded
#'     observations based on `timeDeviationThreshold` and `timeDeviationExcluded`
#' }
#'
#' @keywords internal
.generateAutoFootnotes <- function(attrs) {
	footnotes <- character(0)

	# Return early if attrs is NULL or empty

	if (is.null(attrs) || length(attrs) == 0) {
		return(footnotes)
	}

	# -------------------------------------------------------------------------
	# Time deviation filtering footnote
	if (isTRUE(attrs$timeDeviationFiltered)) {
		excluded  <- attrs$timeDeviationExcluded
		threshold <- attrs$timeDeviationThreshold

		if (!is.null(excluded) && nrow(excluded) > 0) {
			# Dynamically detect occasion columns (everything except fixed columns)
			fixed_cols <- c("id", "ActualTime", "NominalTime", ".pct_deviation")
			occ_cols <- setdiff(names(excluded), fixed_cols)

			# Build modular message from excluded observations
			obs_parts <- apply(excluded, 1, function(row) {
				parts <- paste0("Subject ", row["id"])

				# Add occasion columns if present (dynamic detection)
				if (length(occ_cols) > 0) {
					occ_parts <- sapply(occ_cols, function(occ) {
						if (!is.na(row[occ]) && row[occ] != "") {
							paste0(occ, ": ", row[occ])
						} else {
							NULL
						}
					})
					occ_parts <- occ_parts[!sapply(occ_parts, is.null)]
					if (length(occ_parts) > 0) {
						parts <- paste0(parts, " - ", paste(occ_parts, collapse = " - "))
					}
				}

				# Add actual time
				parts <- paste0(parts, " at t=", row["ActualTime"])
				return(parts)
			})

			footnote_msg <- sprintf(
				"Observations excluded from summary (>%g%% deviation from nominal time):\n%s",
				threshold,
				paste(obs_parts, collapse = "; ")
			)
			footnotes <- c(footnotes, footnote_msg)
		}
	}
	# -------------------------------------------------------------------------
	# Source footnotes (project + R script).
	# Both lines are emitted as a single footer entry joined by "\n" so that
	# they render as adjacent lines inside one paragraph - no blank/extra
	# spacing between them, even when flextable adds inter-paragraph margins.
	# - project_source: the Monolix/PKanalix project the table is based on.
	# - r_source: the .R/.Rmd/.qmd script that generated the data frame
	#   (captured by helpers via .getRScriptPath() at table-creation time).
	source_lines <- character(0)
	if (!is.null(attrs$project_source)) {
		source_lines <- c(source_lines, paste0("Project source: ", attrs$project_source))
	}
	if (!is.null(attrs$r_source)) {
		source_lines <- c(source_lines, paste0("Script source: ", attrs$r_source))
	}
	if (length(source_lines) > 0) {
		footnotes <- c(footnotes, paste(source_lines, collapse = "\n"))
	}
	# -------------------------------------------------------------------------
	# Future: Add more auto-footnote detection here
	# e.g., BLQ handling, missing data, etc.
	# -------------------------------------------------------------------------

	return(footnotes)
}
################################################################################

#' Format and style data frames as flextables
#'
#' Converts a data frame into a styled flextable with support for multiple features:
#' conditional coloring, section headers for population tables, subscript formatting,
#' custom headers, and various pre-defined themes.
#'
#' @param data.flex Data frame to be formatted as a flextable. Can include:
#'   - Subscript markers: Use `[[sub:text]]` in cell values for subscripts
#'   - Attributes for special handling:
#'     - `table_type = "pop"`: Enables population table section headers
#'     - `format = "wide"` or `"long"`: Controls section header layout for pop tables
#'   - Column `Section` (for pop tables): Section codes (FE, IIV, IOV, CORR, RES_ERR)
#'
#' @param grouping.columns Character vector of column names to left-align. These columns are reordered to appear first in the table. If `NULL` (default), no special column ordering is applied.
#' @param header_level1 Character string, data frame, or vector for the topmost header line. Added using `flextable::add_header_lines()`. If `NULL` (default), no level 1 header.
#' @param header_level2 Character string, data frame, or vector for the middle header line. If `NULL` (default), no level 2 header.
#' @param header_level3 Character string, data frame, or vector for the bottom header line. If `NULL` (default), no level 3 header.
#' @param color Character vector specifying which columns to conditionally color.
#'   Options include:
#'   \describe{
#'     \item{"RSE"}{Colors any column with "RSE" in its name based on thresholds:
#'       50-100 (yellow), 100-200 (orange), >200 (red)}
#'     \item{"PVAL"}{Colors `p.value` column: <=0.01 (red), 0.01-0.05 (orange), 0.05-0.09 (yellow)}
#'     \item{"BE"}{Colors `Conclusion` column for bioequivalence: "Bioequivalent" (green), "Not bioequivalent" (red)}
#'     \item{"DP"}{Colors `Conclusion` column for dose proportionality: "Proportional" (green),
#'       "Inconclusive" (yellow), "Not Proportional" (red)}
#'   }
#'   If `NULL` (default), no conditional coloring is applied.
#' @param theme_fn Function to apply a flextable theme. Default is `theme_box`. Other options include `theme_vanilla`, `theme_zebra`, `theme_booktabs`, `theme_tron`.
#' @param soften_amount Numeric value between 0 and 1 controlling color softening for docx/pptx output. 0 = original colors, 1 = white. Default is 0.50.
#' @param html_alpha Numeric value between 0 and 1 controlling transparency for HTML output (rgba colors). Default is 0.18.
#' @param align_default Character string for global alignment: "left", "center" (default), "right", or "justify".
#' @param align_grouping Character string for grouping column alignment. If `NULL` (default), uses `align_default`.
#' @param align_cols Named character vector for column-specific alignment (e.g., `c("Parameter" = "left", "Estimate" = "right")`). Overrides other alignment settings. If `NULL` (default), no column-specific alignment.
#'
#' @return A flextable object with applied styling and formatting.
#'
#' @details
#' **Population table section headers:**
#' If `data.flex` has attribute `table_type = "pop"` and a `Section` column, section headers are automatically added:
#' - **Wide format**: Split header "Fixed effects | Random effects" at top,
#'   plus section headers for Correlations and Residual error
#' - **Long format**: Section headers for all sections (Fixed effects,
#'   Random effects IIV/IOV, Correlations, Residual error)
#'
#' **Subscript formatting:**
#' Use `[[sub:text]]` markers in cell values to render subscripts. Example: `"C[[sub:max]]"` renders as C with subscript "max".
#' **Color palettes:**
#' Colors are automatically adapted based on output format:
#' - HTML: rgba colors with transparency
#' - docx/pptx: Softened hex colors blended with white
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' df <- data.frame(Parameter = c("Cl", "V"), Estimate = c(1.5, 20.3), RSE = c(45, 120))
#' print_table_flex(df, color = c("RSE"))
#'
#' # Population table with section headers
#' pop_table <- tableParameterEstimates(use_wide_format = TRUE)
#' print_table_flex(pop_table, color = c("RSE"), theme_fn = theme_booktabs)
#'
#' # With grouping columns and custom headers
#' print_table_flex(df,
#'                  grouping.columns = c("Parameter"),
#'                  header_level1 = "Population PK Parameters",
#'                  color = c("RSE"))
#' }
style_tfl <- function(data.flex,
											grouping.columns = NULL,
											header_level1    = NULL,
											header_level2    = NULL,
											header_level3    = NULL,
											color            = NULL,     # e.g. c("BE","DP","PVAL","RSE")
											theme_fn         = theme_booktabs,
											soften_amount    = 0.50,     # 0 = original, 1 = white
											html_alpha       = 0.18,     # only used for html rgba
											align_default    = "center", # global default alignment
											align_grouping   = NULL,     # alignment for grouping columns (NULL = use default)
											align_cols       = NULL,     # named vector for column-specific alignments
											footnote         = NULL,     # manual footnote text
											autoFootnote     = TRUE)     # auto-detect footnotes from attributes
{
	##############################################################################
	# 0. Check required packages
	if (!requireNamespace("flextable", quietly = TRUE)) {
		stop("Package 'flextable' is required for print_table_flex(). ",
				 "Install it with: install.packages('flextable')", call. = FALSE)
	}
	#-----------------------------------------------------------------------------
	# ARGUMENT VALIDATION
	# 1. data.flex
	if (!is.data.frame(data.flex)) {
		stop("'data.flex' must be a data.frame.", call. = FALSE)
	}
	if (nrow(data.flex) == 0) {
		stop("'data.flex' must have at least one row.", call. = FALSE)
	}
	#-----------------------------------------------------------------------------
	# 2. grouping.columns
	if (!is.null(grouping.columns)) {
		if (!is.character(grouping.columns)) {
			stop("'grouping.columns' must be a character vector or NULL.", call. = FALSE)
		}
		# Column existence check below
	}
	#-----------------------------------------------------------------------------
	# 3. header_level1, header_level2, header_level3
	for (hdr_name in c("header_level1", "header_level2", "header_level3")) {
		hdr_val <- get(hdr_name)
		if (!is.null(hdr_val) && (!is.character(hdr_val) || length(hdr_val) != 1)) {
			stop(sprintf("'%s' must be a single character string or NULL.", hdr_name), call. = FALSE)
		}
	}
	#-----------------------------------------------------------------------------
	# 4. color
	if (!is.null(color)) {
		color <- match.arg(toupper(color), c("BE", "DP", "PVAL", "RSE"), several.ok = TRUE)
	}
	#-----------------------------------------------------------------------------
	# 5. theme_fn
	if (!is.function(theme_fn)) {
		stop("'theme_fn' must be a function.", call. = FALSE)
	}
	#-----------------------------------------------------------------------------
	# 6. soften_amount
	if (!is.numeric(soften_amount) || length(soften_amount) != 1) {
		stop("'soften_amount' must be a single numeric value.", call. = FALSE)
	}
	if (soften_amount < 0 || soften_amount > 1) {
		stop("'soften_amount' must be between 0 and 1.", call. = FALSE)
	}
	#-----------------------------------------------------------------------------
	# 7. html_alpha
	if (!is.numeric(html_alpha) || length(html_alpha) != 1) {
		stop("'html_alpha' must be a single numeric value.", call. = FALSE)
	}
	if (html_alpha < 0 || html_alpha > 1) {
		stop("'html_alpha' must be between 0 and 1.", call. = FALSE)
	}
	#-----------------------------------------------------------------------------
	# 8. align_default
	valid_alignments <- c("left", "center", "right", "justify")
	if (!is.character(align_default) || length(align_default) != 1 ||
			!align_default %in% valid_alignments) {
		stop(sprintf(
			"'align_default' must be one of: %s",
			paste(valid_alignments, collapse = ", ")
		), call. = FALSE)
	}
	#-----------------------------------------------------------------------------
	# 9. align_grouping
	if (!is.null(align_grouping)) {
		if (!is.character(align_grouping) || length(align_grouping) != 1 ||
				!align_grouping %in% valid_alignments) {
			stop(sprintf(
				"'align_grouping' must be one of: %s (or NULL)",
				paste(valid_alignments, collapse = ", ")
			), call. = FALSE)
		}
	}
	#-----------------------------------------------------------------------------
	# 10. align_cols
	if (!is.null(align_cols)) {
		if (!is.character(align_cols) || is.null(names(align_cols))) {
			stop("'align_cols' must be a named character vector or NULL.", call. = FALSE)
		}
		invalid_aligns <- setdiff(align_cols, valid_alignments)
		if (length(invalid_aligns) > 0) {
			stop(sprintf(
				"'align_cols' values must be one of: %s\nInvalid values: %s",
				paste(valid_alignments, collapse = ", "),
				paste(invalid_aligns, collapse = ", ")
			), call. = FALSE)
		}
		missing_cols <- setdiff(names(align_cols), names(data.flex))
		if (length(missing_cols) > 0) {
			warning(sprintf(
				"Some columns in 'align_cols' not found in data: %s",
				paste(missing_cols, collapse = ", ")
			), call. = FALSE)
		}
	}
	#-----------------------------------------------------------------------------
	# 11. footnote
	if (!is.null(footnote) && !is.character(footnote)) {
		stop("'footnote' must be a character vector or NULL.", call. = FALSE)
	}
	#-----------------------------------------------------------------------------
	# 12. autoFootnote
	if (!is.logical(autoFootnote) || length(autoFootnote) != 1 || is.na(autoFootnote)) {
		stop("'autoFootnote' must be TRUE or FALSE.", call. = FALSE)
	}
	##############################################################################
	# Check if columns exist
	pval.flag <- ("PVAL" %in% color) && "p.value" %in% colnames(data.flex)
	BE.flag   <- ("BE" %in% color) && "Conclusion" %in% colnames(data.flex)
	DP.flag   <- ("DP" %in% color) && "Conclusion" %in% colnames(data.flex)

	# RSE flag: detect any columns containing "RSE" in their name
	# This covers: "RSE", "%RSE", "RSE (%)", "FE_RSE", "IIV_RSE", "IOV_RSE", "IIV\nRSE (%)", etc.
	RSE.flag <- "RSE" %in% color
	RSE.cols <- character(0)
	if (RSE.flag) {
		RSE.cols <- grep("RSE", colnames(data.flex), value = TRUE, ignore.case = TRUE)
		RSE.flag <- length(RSE.cols) > 0
	}

	flextable::set_flextable_defaults(na_str = "NA")

	# Save attributes before reordering (bc subsetting can lose attributes)
	saved_attrs <- attributes(data.flex)

	# Reorder columns so grouping columns come first
	if (!is.null(grouping.columns)) {
		# Validate that all grouping columns exist in the data
		missing_cols <- setdiff(grouping.columns, names(data.flex))
		if (length(missing_cols) > 0) {
			stop(sprintf(
				"Grouping column%s not found in data: %s\nAvailable columns: %s",
				if (length(missing_cols) > 1) "s" else "",
				paste(missing_cols, collapse = ", "),
				paste(names(data.flex), collapse = ", ")
			))
		}

		other.cols <- setdiff(names(data.flex), grouping.columns)
		data.flex <- data.flex[, c(grouping.columns, other.cols), drop = FALSE]

		# Restore custom attributes (table_type, format, etc.)
		# Keep only non-standard attributes (not names, row.names, class)
		custom_attrs <- saved_attrs[!names(saved_attrs) %in% c("names", "row.names", "class")]
		for (attr_name in names(custom_attrs)) {
			attr(data.flex, attr_name) <- custom_attrs[[attr_name]]
		}
	}

	#-----------------------------------------------------------------------------
	# [Code Chunk of Section Headers]
	# For parameter tables (population or NCA), add section header rows to
	# visually separate different sections:
	# - Population: Fixed effects, Random effects, Correlations, Residual error
	# - NCA: Summary Statistics, Individual data (Parameters/Concentrations/Ratios)
	# Wide format (pop only) uses a split header at top for FE/RE.
	#-----------------------------------------------------------------------------
	# Save table_type before calling .add_section_headers (it may be lost during processing)
	table_type_for_styling <- attr(data.flex, "table_type")

	section_result <- .add_section_headers(data.flex, grouping.columns)
	data.flex <- section_result$data.flex
	section_header_rows <- section_result$section_header_rows
	wide_split_col <- section_result$wide_split_col
	is_wide <- section_result$is_wide
	has_section_headers <- length(section_header_rows) > 0 || !is.null(wide_split_col)
	# --- END: Section Headers ---

	ft <- flextable(data.flex)

	#-----------------------------------------------------------------------------
	# [Alias Conversion]
	# Apply subscript formatting to both body cells and column headers
	#-----------------------------------------------------------------------------
	ft <- .apply_alias_subscripts(ft, data.flex)


	#-----------------------------------------------------------------------------
	# Initialize flextable with theme
	ft <- theme_fn(ft)
	ft <- fontsize(ft, size = 8, part = "all")

	# Merge vertical grouping columns
	if (!is.null(grouping.columns)) {
		ft <- merge_v(ft, j = grouping.columns)
	}

	# Apply default alignment to all table content
	ft <- flextable::align(ft, align = align_default, part = "all")

	# Apply grouping column alignment if specified
	if (!is.null(grouping.columns) && !is.null(align_grouping)) {
		for (col in grouping.columns) {
			ft <- flextable::align(ft, j = col, align = align_grouping, part = "all")
		}
	}

	# Apply column-specific alignments (highest priority)
	if (!is.null(align_cols)) {
		for (col_name in names(align_cols)) {
			if (col_name %in% colnames(data.flex)) {
				ft <- flextable::align(ft, j = col_name, align = align_cols[[col_name]], part = "all")
			}
		}
	}

	# Style section header rows for tables with sections (pop or NCA)
	if (has_section_headers && length(section_header_rows) > 0) {
		border_style <- officer::fp_border(color = "black", width = 1)

		if (is_wide && !is.null(wide_split_col)) {
			# Wide format: split header row at top + section headers below
			split_row_idx <- section_header_rows[1]

			# Merge and label "Fixed effects" (columns 1 to split_col - 1)
			ft <- ft %>%
				flextable::merge_at(i = split_row_idx, j = 1:(wide_split_col - 1), part = "body") %>%
				flextable::compose(i = split_row_idx, j = 1, value = flextable::as_paragraph("Fixed effects"), part = "body") %>%
				flextable::align(i = split_row_idx, j = 1, align = "left", part = "body")

			# Merge and label "Random effects" (columns split_col to end)
			ft <- ft %>%
				flextable::merge_at(i = split_row_idx, j = wide_split_col:ncol(data.flex), part = "body") %>%
				flextable::compose(i = split_row_idx, j = wide_split_col, value = flextable::as_paragraph("Random effects"), part = "body") %>%
				flextable::align(i = split_row_idx, j = wide_split_col:ncol(data.flex), align = "left", part = "body")

			# Bold the split header row
			ft <- ft %>%
				flextable::bold(i = split_row_idx, part = "body")

			# Add borders around split header
			ft <- ft %>%
				flextable::hline_top(border = border_style, part = "body") %>%
				flextable::hline(i = split_row_idx, border = border_style, part = "body")

			# Style remaining section headers (CORR, RES_ERR) if any
			if (length(section_header_rows) > 1) {
				for (row_idx in section_header_rows[-1]) {
					ft <- ft %>%
						flextable::bold(i = row_idx, part = "body") %>%
						flextable::align(i = row_idx, align = "left", part = "body") %>%
						flextable::merge_at(i = row_idx, j = seq_len(ncol(data.flex)), part = "body")

					# Add line above
					ft <- ft %>%
						flextable::hline(i = row_idx - 1, border = border_style, part = "body")

					# Add line below
					ft <- ft %>%
						flextable::hline(i = row_idx, border = border_style, part = "body")
				}
			}
		} else {
			# Long format: multiple section headers
			# Check if this is an NCA table with grouping columns
			is_nca_with_grouping <- !is.null(table_type_for_styling) &&
				grepl("^nca_", table_type_for_styling) &&
				!is.null(grouping.columns) && length(grouping.columns) > 0

			for (row_idx in section_header_rows) {
				if (is_nca_with_grouping) {
					# For NCA with grouping: only merge non-grouping columns
					non_grouping_indices <- which(!(names(data.flex) %in% grouping.columns))

					ft <- ft %>%
						flextable::bold(i = row_idx, part = "body") %>%
						flextable::align(i = row_idx, j = non_grouping_indices, align = "left", part = "body") %>%
						flextable::merge_at(i = row_idx, j = non_grouping_indices, part = "body")
				} else {
					# For all other cases: merge all columns (existing logic)
					ft <- ft %>%
						flextable::bold(i = row_idx, part = "body") %>%
						flextable::align(i = row_idx, align = "left", part = "body") %>%
						flextable::merge_at(i = row_idx, j = seq_len(ncol(data.flex)), part = "body")
				}

				# Add line above
				if (row_idx > 1) {
					ft <- ft %>%
						flextable::hline(i = row_idx - 1, border = border_style, part = "body")
				} else {
					ft <- ft %>%
						flextable::hline_top(border = border_style, part = "body")
				}

				# Add line below
				ft <- ft %>%
					flextable::hline(i = row_idx, border = border_style, part = "body")
			}
		}
	}

	# ---------------------------------------------------------------------------
	# Header levels (1 = topmost)
	# ---------------------------------------------------------------------------
	h1 <- .as_header_string(header_level1)
	h2 <- .as_header_string(header_level2)
	h3 <- .as_header_string(header_level3)

	# Add from lowest to highest so level 1 ends up on top
	# add_header_lines() inserts ABOVE the existing header each time
	for (line in na.omit(c(h3, h2, h1))) {
		ft <- flextable::add_header_lines(ft, values = line)
	}

	ft <- flextable::align(ft, align = "center", part = "header")

	# Re-apply column-specific header alignment so it isn't overridden by the
	# header re-centering above (which is needed for any header lines added via
	# add_header_lines()).
	if (!is.null(align_cols)) {
		for (col_name in names(align_cols)) {
			if (col_name %in% colnames(data.flex)) {
				ft <- flextable::align(ft, j = col_name, align = align_cols[[col_name]], part = "header")
			}
		}
	}


	# ---------------------------------------------------------------------------
	# Color palette setup
	# ---------------------------------------------------------------------------
	base_yellow <- "#FFD700"
	base_orange <- "#EE7600"
	base_red    <- "#8B1A1A"
	base_green  <- "#458B00"

	# Default: pastel hex (good for docx/pptx and generally safe)
	col_yellow <- .soften_hex(base_yellow, soften_amount)
	col_orange <- .soften_hex(base_orange, soften_amount)
	col_red    <- .soften_hex(base_red, soften_amount)
	col_green  <- .soften_hex(base_green, soften_amount)

	# If HTML output, optionally use rgba for transparency
	if (.is_html_output()) {
		col_yellow <- .hex_to_rgba(base_yellow, html_alpha)
		col_orange <- .hex_to_rgba(base_orange, html_alpha)
		col_red    <- .hex_to_rgba(base_red, html_alpha)
		col_green  <- .hex_to_rgba(base_green, html_alpha)
	}

	# Apply conditional background formatting for RSE columns
	if (RSE.flag && length(RSE.cols) > 0) {
		for (rse_col in RSE.cols) {
			all_rows <- seq_len(nrow(data.flex))
			data_row_indices <- if (length(section_header_rows) > 0) {
				setdiff(all_rows, section_header_rows)
			} else {
				all_rows
			}

			for (row_idx in data_row_indices) {
				rse_value <- as.numeric(data.flex[row_idx, rse_col])

				if (is.na(rse_value) || is.nan(rse_value)) {
					next
				}

				if (rse_value > 200) {
					ft <- flextable::bg(ft, i = row_idx, j = rse_col, bg = col_red, part = "body")
				} else if (rse_value >= 100 && rse_value < 200) {
					ft <- flextable::bg(ft, i = row_idx, j = rse_col, bg = col_orange, part = "body")
				} else if (rse_value >= 50 && rse_value < 100) {
					ft <- flextable::bg(ft, i = row_idx, j = rse_col, bg = col_yellow, part = "body")
				} else {
					# VALUES < 50: explicitly clear any existing background color
					ft <- flextable::bg(ft, i = row_idx, j = rse_col, bg = "transparent", part = "body")
				}
			}
		}
	}

	if (pval.flag) {
		ft <- ft %>%
			flextable::bg(i = ~ p.value <= 0.01, j = ~p.value, bg = col_red, part = "body") %>%
			flextable::bg(i = ~ p.value > 0.01 & p.value <= 0.05, j = ~p.value, bg = col_orange, part = "body") %>%
			flextable::bg(i = ~ p.value > 0.05 & p.value <= 0.09, j = ~p.value, bg = col_yellow, part = "body")
	}
	# Apply conditional background formatting
	if (DP.flag) {
		ft <- ft %>%
			flextable::bg(i = ~ Conclusion == "Proportional", j = ~Conclusion, bg = col_green, part = "body") %>%
			flextable::bg(i = ~ Conclusion == "Inconclusive", j = ~Conclusion, bg = col_yellow, part = "body") %>%
			flextable::bg(i = ~ Conclusion == "Not Proportional", j = ~Conclusion, bg = col_red, part = "body")
	}
	if (BE.flag) {
		ft <- ft %>%
			flextable::bg(i = ~ Conclusion == "Bioequivalent", j = ~Conclusion, bg = col_green, part = "body") %>%
			flextable::bg(i = ~ Conclusion == "Not bioequivalent", j = ~Conclusion, bg = col_red, part = "body")
	}

	# ---------------------------------------------------------------------------
	# Footnotes
	# ---------------------------------------------------------------------------
	all_footnotes <- character(0)

	# Auto-detect footnotes from attributes
	if (isTRUE(autoFootnote)) {
		auto_notes <- .generateAutoFootnotes(saved_attrs)
		all_footnotes <- c(all_footnotes, auto_notes)
	}

	# Add manual footnotes
	if (!is.null(footnote)) {
		all_footnotes <- c(all_footnotes, as.character(footnote))
	}

	# Apply footnotes to flextable
	if (length(all_footnotes) > 0) {
		for (fn in all_footnotes) {
			ft <- flextable::add_footer_lines(ft, values = fn)
		}
		ft <- flextable::fontsize(ft, size = 7, part = "footer")
		ft <- flextable::italic(ft, part = "footer")
		ft <- flextable::align(ft, align = "left", part = "footer")
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
# Custom theme example
my_custom_theme <- function(ft) {
	ft <- flextable::border_remove(ft)
	ft <- flextable::bg(ft, bg = "#AABFE4", part = "header")
	ft <- flextable::bold(ft, part = "header")
	ft <- flextable::fontsize(ft, size = 12, part = "all")
	ft <- flextable::hline_bottom(ft,
																border = officer::fp_border(color = "black", width = 2.5),
																part = "header")
	ft <- flextable::hline_bottom(ft,
																border = officer::fp_border(color = "black", width = 1),
																part = "body")
	ft
}
################################################################################
