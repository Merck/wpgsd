#' Core Event Data Validation Function
#'
#' @description
#' Shared validation logic for event data across different validation contexts.
#' Supports three validation levels with increasing strictness.
#'
#' @param data A data.frame or tibble to validate
#' @param validation_level Character string specifying validation level:
#'   - "basic": Basic structure and type validation
#'   - "strict": Strict validation for correlation computation
#'   - "s7": Full validation for S7 EventTable objects
#' @param return_errors Logical; if TRUE, returns character vector of errors
#'   instead of stopping on first error
#'
#' @return If return_errors=FALSE: invisible(TRUE) on success, stops on error.
#'   If return_errors=TRUE: NULL on success, character vector of errors on failure.
#'
#' @details
#' **Basic level:**
#' - Required columns (H1, H2, Analysis, Event) present
#' - All columns are numeric
#' - Hypothesis indices (H1, H2) are positive
#' - Analysis numbers are positive
#' - Event counts are non-negative
#'
#' **Strict level (includes basic plus):**
#' - H1 <= H2 for all rows (correlation computation requirement)
#' - Unique combinations of H1, H2, Analysis
#' - Sequential hypothesis and analysis indices starting from 1
#' - Multiple analyses required
#' - Diagonal entries exist for all off-diagonal entries
#'
#' **S7 level (includes strict plus):**
#' - Event counts non-decreasing across analyses for fixed H1, H2
#' - Diagonal entries have Event >= corresponding off-diagonal entries
#' - Allows H1 > H2 (more flexible than strict)
#'
#' @keywords internal
validate_event_data_core <- function(data, validation_level = c("basic", "strict", "s7"), return_errors = FALSE) {
  validation_level <- match.arg(validation_level)

  # Store errors when return_errors = TRUE
  errors <- character(0)

  # Helper function to handle errors
  handle_error <- function(msg) {
    if (return_errors) {
      errors <<- c(errors, msg)
    } else {
      stop(msg, call. = FALSE)
    }
  }

  # Basic validation - required for all functions
  required_cols <- c("H1", "H2", "Analysis", "Event")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    handle_error(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  if (!is.numeric(data$H1) || !is.numeric(data$H2)) {
    handle_error("H1 and H2 must be numeric")
  }

  if (!is.numeric(data$Analysis)) {
    handle_error("Analysis must be numeric")
  }

  if (!is.numeric(data$Event)) {
    handle_error("Event must be numeric")
  }

  if (is.numeric(data$H1) && is.numeric(data$H2)) {
    if (any(data$H1 <= 0, na.rm = TRUE) || any(data$H2 <= 0, na.rm = TRUE)) {
      handle_error("Hypothesis indices (H1, H2) must be positive integers")
    }

    # Check that H1 and H2 are integers
    if (any(data$H1 != floor(data$H1), na.rm = TRUE) || any(data$H2 != floor(data$H2), na.rm = TRUE)) {
      handle_error("Hypothesis indices (H1, H2) must be integers")
    }
  }

  if (is.numeric(data$Analysis)) {
    if (any(data$Analysis <= 0, na.rm = TRUE)) {
      handle_error("Analysis numbers must be positive integers")
    }

    # Check that Analysis values are integers
    if (any(data$Analysis != floor(data$Analysis), na.rm = TRUE)) {
      handle_error("Analysis numbers must be integers")
    }
  }

  # Basic event value check - only verify it's numeric for basic validation
  # (Non-negative and integer checks happen in strict validation)

  # Return early if we have errors and return_errors = TRUE
  if (return_errors && length(errors) > 0) {
    return(errors)
  }

  # Stop here for basic validation
  if (validation_level == "basic") {
    return(if (return_errors) NULL else invisible(TRUE))
  }

  # Strict validation - additional requirements for correlation computation
  # Note: S7 validation skips some strict requirements
  if (validation_level == "strict" && any(data$H1 > data$H2, na.rm = TRUE)) {
    handle_error("H1 must be <= H2 for all rows")
  }

  # Check uniqueness of H1, H2, Analysis combinations
  combo_check <- paste(data$H1, data$H2, data$Analysis, sep = "_")
  if (any(duplicated(combo_check))) {
    handle_error("Combinations of H1, H2, Analysis must be unique")
  }

  # Check Event is non-negative (no integer requirement)
  if (any(data$Event < 0, na.rm = TRUE)) {
    handle_error("Event must be non-negative")
  }

  # Check Analysis values are sequential positive integers starting from 1
  # Only required for strict and s7 validation
  if (validation_level %in% c("strict", "s7")) {
    # Convert to integers for sequential check
    unique_analyses <- sort(unique(as.integer(round(data$Analysis))))
    expected_analyses <- seq_len(max(unique_analyses))
    if (!identical(unique_analyses, expected_analyses)) {
      handle_error("Analysis values must be sequential positive integers starting from 1")
    }

    # Check H1 values are sequential integers starting from 1
    unique_h1 <- sort(unique(as.integer(data$H1)))
    expected_h1 <- seq_len(max(unique_h1))
    if (!identical(unique_h1, expected_h1)) {
      handle_error("H1 values must be sequential positive integers starting from 1")
    }

    # Check H2 values are sequential integers starting from 1
    unique_h2 <- sort(unique(as.integer(data$H2)))
    expected_h2 <- seq_len(max(unique_h2))
    if (!identical(unique_h2, expected_h2)) {
      handle_error("H2 values must be sequential positive integers starting from 1")
    }
  }

  # Check that for each analysis, if off-diagonal entries exist, diagonal entries exist
  # Only required for strict and s7 validation
  if (validation_level %in% c("strict", "s7")) {
    unique_analyses <- sort(unique(as.integer(data$Analysis)))
    K <- max(unique_analyses)
    for (k in seq_len(K)) {
      analysis_data <- data[data$Analysis == k, ]
      off_diagonal <- analysis_data[analysis_data$H1 < analysis_data$H2, ]

      if (nrow(off_diagonal) > 0) {
        diagonal_data <- analysis_data[analysis_data$H1 == analysis_data$H2, ]
        if (nrow(diagonal_data) == 0) {
          handle_error(paste("For Analysis", k, ", off-diagonal entries exist but no diagonal entries found"))
        }
      }
    }

    # Check that for any off-diagonal entry, both corresponding diagonal entries exist
    for (i in seq_len(nrow(data))) {
      h1 <- data$H1[i]
      h2 <- data$H2[i]
      analysis <- data$Analysis[i]

      if (h1 < h2) { # Off-diagonal entry
        # Check H1=h1, H2=h1 exists for this analysis
        diag_h1 <- data[data$H1 == h1 & data$H2 == h1 & data$Analysis == analysis, ]
        if (nrow(diag_h1) == 0) {
          handle_error(paste0("Missing diagonal entry: H1=", h1, ", H2=", h1, ", Analysis=", analysis))
        }

        # Check H1=h2, H2=h2 exists for this analysis
        diag_h2 <- data[data$H1 == h2 & data$H2 == h2 & data$Analysis == analysis, ]
        if (nrow(diag_h2) == 0) {
          handle_error(paste0("Missing diagonal entry: H1=", h2, ", H2=", h2, ", Analysis=", analysis))
        }
      }
    }
  }

  # Return early if we have errors and return_errors = TRUE
  if (return_errors && length(errors) > 0) {
    return(errors)
  }

  # Stop here for strict validation
  if (validation_level == "strict") {
    return(if (return_errors) NULL else invisible(TRUE))
  }

  # S7 validation - additional requirements for EventTable objects
  # Note: S7 validation is more flexible - allows H1 > H2 and non-integer Event values

  # S7 specific validation: Event counts non-decreasing across analyses for fixed H1, H2
  for (h1 in unique(data$H1)) {
    for (h2 in unique(data$H2)) {
      subset_data <- data[data$H1 == h1 & data$H2 == h2, ]
      if (nrow(subset_data) > 1) {
        subset_data <- subset_data[order(subset_data$Analysis), ]
        if (any(diff(subset_data$Event) < 0)) {
          handle_error(paste0("For H1=", h1, ", H2=", h2, ", Event counts must be non-decreasing across analyses"))
        }
      }
    }
  }

  # S7 specific validation: Diagonal entries must have Event >= corresponding off-diagonal entries
  for (i in seq_len(nrow(data))) {
    h1 <- data$H1[i]
    h2 <- data$H2[i]
    analysis <- data$Analysis[i]
    event_val <- data$Event[i]

    # Skip if this is already a diagonal entry
    if (h1 == h2) next

    # Check that diagonal H1=H1 entry exists with Event >= current Event
    h1_diagonal <- data[data$H1 == h1 & data$H2 == h1 & data$Analysis == analysis, ]
    if (nrow(h1_diagonal) == 0) {
      handle_error(paste0("Missing diagonal entry: H1=", h1, ", H2=", h1, ", Analysis=", analysis))
    } else if (h1_diagonal$Event[1] < event_val) {
      handle_error(paste0(
        "Diagonal entry H1=", h1, ", H2=", h1, ", Analysis=", analysis,
        " has Event (", h1_diagonal$Event[1], ") < off-diagonal Event (", event_val, ")"
      ))
    }

    # Check that diagonal H2=H2 entry exists with Event >= current Event
    h2_diagonal <- data[data$H1 == h2 & data$H2 == h2 & data$Analysis == analysis, ]
    if (nrow(h2_diagonal) == 0) {
      handle_error(paste0("Missing diagonal entry: H1=", h2, ", H2=", h2, ", Analysis=", analysis))
    } else if (h2_diagonal$Event[1] < event_val) {
      handle_error(paste0(
        "Diagonal entry H1=", h2, ", H2=", h2, ", Analysis=", analysis,
        " has Event (", h2_diagonal$Event[1], ") < off-diagonal Event (", event_val, ")"
      ))
    }
  }

  # Return final result
  if (return_errors) {
    return(if (length(errors) > 0) errors else NULL)
  } else {
    return(invisible(TRUE))
  }
}
