# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the wpgsd program.
#
# wpgsd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# S7 Classes for wpgsd Package
#
# This file contains S7 class definitions for the wpgsd package.
# These classes provide type safety and method dispatch for the main
# data structures used in weighted parametric group sequential designs.

#' @import S7

# Define the EventTable S7 class
EventTable <- S7::new_class(
  "EventTable",
  properties = list(
    data = S7::class_data.frame,
    n_hypotheses = S7::class_integer,
    n_analyses = S7::class_integer
  ),
  constructor = function(data = tibble::tibble()) {
    # Validate required columns
    required_cols <- c("H1", "H2", "Analysis", "Event")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("EventTable requires columns: ", paste(missing_cols, collapse = ", "))
    }

    # Calculate derived properties
    n_hypotheses <- max(c(data$H1, data$H2), na.rm = TRUE)
    n_analyses <- max(data$Analysis, na.rm = TRUE)

    S7::new_object(
      EventTable,
      data = data,
      n_hypotheses = as.integer(n_hypotheses),
      n_analyses = as.integer(n_analyses)
    )
  },
  validator = function(self) {
    # Validate data types
    if (!is.numeric(self@data$H1) || !is.numeric(self@data$H2)) {
      "@data$H1 and @data$H2 must be numeric"
    } else if (!is.numeric(self@data$Analysis)) {
      "@data$Analysis must be numeric"
    } else if (!is.numeric(self@data$Event)) {
      "@data$Event must be numeric"
    } else if (any(self@data$H1 <= 0, na.rm = TRUE) || any(self@data$H2 <= 0, na.rm = TRUE)) {
      "Hypothesis indices (H1, H2) must be positive integers"
    } else if (any(self@data$Analysis <= 0, na.rm = TRUE)) {
      "Analysis numbers must be positive integers"
    } else if (any(self@data$Event < 0, na.rm = TRUE)) {
      "Event counts must be non-negative"
    } else {
      # Additional validation requirements
      data <- self@data

      # Requirement 1: For fixed H1, H2, Event must be non-decreasing as Analysis increases
      for (h1 in unique(data$H1)) {
        for (h2 in unique(data$H2)) {
          subset_data <- data[data$H1 == h1 & data$H2 == h2, ]
          if (nrow(subset_data) > 1) {
            subset_data <- subset_data[order(subset_data$Analysis), ]
            if (any(diff(subset_data$Event) < 0)) {
              return(paste0("For H1=", h1, ", H2=", h2, ", Event counts must be non-decreasing across analyses"))
            }
          }
        }
      }

      # Requirement 2: For off-diagonal entries, diagonal entries must exist with >= Event counts
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
          return(paste0("Missing diagonal entry: H1=", h1, ", H2=", h1, ", Analysis=", analysis))
        } else if (h1_diagonal$Event[1] < event_val) {
          return(paste0(
            "Diagonal entry H1=", h1, ", H2=", h1, ", Analysis=", analysis,
            " has Event (", h1_diagonal$Event[1], ") < off-diagonal Event (", event_val, ")"
          ))
        }

        # Check that diagonal H2=H2 entry exists with Event >= current Event
        h2_diagonal <- data[data$H1 == h2 & data$H2 == h2 & data$Analysis == analysis, ]
        if (nrow(h2_diagonal) == 0) {
          return(paste0("Missing diagonal entry: H1=", h2, ", H2=", h2, ", Analysis=", analysis))
        } else if (h2_diagonal$Event[1] < event_val) {
          return(paste0(
            "Diagonal entry H1=", h2, ", H2=", h2, ", Analysis=", analysis,
            " has Event (", h2_diagonal$Event[1], ") < off-diagonal Event (", event_val, ")"
          ))
        }
      }
    }
  }
)

# TODO: Add S7 method definitions later
# #' Print method for EventTable
# S7::method(show, EventTable) <- function(object) { ... }
#
# #' Summary method for EventTable
# S7::method(summary, EventTable) <- function(object, ...) { ... }

#' Subset EventTable by Analysis or Hypotheses
#'
#' @description
#' Extract a subset of an EventTable object based on analysis numbers
#' or hypothesis indices.
#'
#' @param x An EventTable S7 object
#' @param analysis Optional vector of analysis numbers to include
#' @param hypotheses Optional vector of hypothesis indices to include
#'
#' @return A new EventTable object containing only the specified subset
#'
#' @examples
#' library(tibble)
#'
#' # Create sample data
#' event_data <- tibble(
#'   H1 = c(1, 2, 3, 1, 2, 3),
#'   H2 = c(1, 2, 3, 1, 2, 3),
#'   Analysis = c(1, 1, 1, 2, 2, 2),
#'   Event = c(155, 160, 165, 305, 320, 335)
#' )
#' event_table <- EventTable(data = event_data)
#'
#' # Subset by analysis
#' analysis_1 <- subset_event_table(event_table, analysis = 1)
#'
#' # Subset by hypotheses
#' h1_h2_only <- subset_event_table(event_table, hypotheses = c(1, 2))
#'
#' @export
subset_event_table <- function(x, analysis = NULL, hypotheses = NULL) {
  if (!S7::S7_inherits(x, EventTable)) {
    stop("x must be an EventTable object")
  }

  data_subset <- x@data

  if (!is.null(analysis)) {
    data_subset <- data_subset[data_subset$Analysis %in% analysis, ]
  }

  if (!is.null(hypotheses)) {
    data_subset <- data_subset[
      (data_subset$H1 %in% hypotheses) & (data_subset$H2 %in% hypotheses),
    ]
  }

  EventTable(data = data_subset)
}

#' Convert Data to EventTable
#'
#' @description
#' Convert a tibble or data.frame to an EventTable S7 object, or return
#' the object unchanged if it's already an EventTable.
#'
#' @param data A tibble, data.frame, or EventTable object containing the
#'   required columns (H1, H2, Analysis, Event)
#'
#' @return An EventTable S7 object
#'
#' @examples
#' library(tibble)
#'
#' # Convert tibble to EventTable
#' event_data <- tibble(
#'   H1 = c(1, 2),
#'   H2 = c(1, 2),
#'   Analysis = c(1, 1),
#'   Event = c(100, 200)
#' )
#'
#' event_table <- as_event_table(event_data)
#'
#' # If already EventTable, returns unchanged
#' same_table <- as_event_table(event_table)
#' identical(event_table, same_table) # TRUE
#'
#' @export
as_event_table <- function(data) {
  if (S7::S7_inherits(data, EventTable)) {
    return(data)
  }
  EventTable(data = data)
}

#' Validate EventTable Data Format
#'
#' @description
#' Validate that a data.frame or tibble has the correct structure and
#' data types required for creating an EventTable object.
#'
#' @param data A data.frame or tibble to validate
#'
#' @return `TRUE` if validation passes (invisible), otherwise stops with
#'   descriptive error message
#'
#' @details
#' This function checks that:
#' - Required columns (H1, H2, Analysis, Event) are present
#' - All columns are numeric
#' - Hypothesis indices (H1, H2) are positive
#' - Analysis numbers are positive
#' - Event counts are non-negative
#'
#' @examples
#' library(tibble)
#'
#' # Valid data passes silently
#' valid_data <- tibble(
#'   H1 = c(1, 2),
#'   H2 = c(1, 2),
#'   Analysis = c(1, 1),
#'   Event = c(100, 200)
#' )
#' validate_event_table_data(valid_data) # Returns TRUE
#'
#' # Invalid data throws error
#' \dontrun{
#' invalid_data <- tibble(H1 = c(1, 2)) # Missing columns
#' validate_event_table_data(invalid_data) # Error
#' }
#'
#' @export
validate_event_table_data <- function(data) {
  required_cols <- c("H1", "H2", "Analysis", "Event")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.numeric(data$H1) || !is.numeric(data$H2)) {
    stop("H1 and H2 must be numeric")
  }

  if (!is.numeric(data$Analysis)) {
    stop("Analysis must be numeric")
  }

  if (!is.numeric(data$Event)) {
    stop("Event must be numeric")
  }

  if (any(data$H1 <= 0, na.rm = TRUE) || any(data$H2 <= 0, na.rm = TRUE)) {
    stop("Hypothesis indices (H1, H2) must be positive integers")
  }

  if (any(data$Analysis <= 0, na.rm = TRUE)) {
    stop("Analysis numbers must be positive integers")
  }

  if (any(data$Event < 0, na.rm = TRUE)) {
    stop("Event counts must be non-negative")
  }

  TRUE
}

#' Create EventTable S7 Object
#'
#' @description
#' Create a type-safe S7 EventTable object that represents event count data
#' structure used in `generate_corr()` and `generate_event_table()`. This class
#' provides validation and computed properties for hypothesis and analysis counts.
#'
#' @param data A tibble or data.frame containing the required columns:
#'   - `H1`: First hypothesis index (numeric, positive integers)
#'   - `H2`: Second hypothesis index (numeric, positive integers)
#'   - `Analysis`: Analysis number (numeric, positive integers)
#'   - `Event`: Event count (numeric, non-negative)
#'
#' @details
#' The EventTable class automatically validates the input data and computes:
#' - `n_hypotheses`: Maximum hypothesis index across H1 and H2 columns
#' - `n_analyses`: Maximum analysis number
#'
#' The class ensures data integrity by validating that:
#' - All required columns are present
#' - H1, H2, Analysis, and Event are numeric
#' - Hypothesis indices are positive integers
#' - Analysis numbers are positive integers
#' - Event counts are non-negative
#'
#' @return An EventTable S7 object with validated data and computed properties
#'
#' @examples
#' library(tibble)
#'
#' # Create sample event data
#' event_data <- tibble(
#'   H1 = c(1, 2, 1, 1, 2, 1),
#'   H2 = c(1, 2, 2, 1, 2, 2),
#'   Analysis = c(1, 1, 1, 2, 2, 2),
#'   Event = c(155, 160, 85, 305, 320, 170)
#' )
#'
#' # Create EventTable object
#' event_table <- new_event_table(data = event_data)
#'
#' # Access properties
#' print(event_table@n_hypotheses) # Number of hypotheses
#' print(event_table@n_analyses) # Number of analyses
#'
#' # Use with existing wpgsd functions
#' correlation_matrix <- generate_corr(event_table@data)
#'
#' @export
new_event_table <- function(data = tibble::tibble()) {
  EventTable(data = data)
}

# CorrelationMatrix S7 Class ====

#' CorrelationMatrix S7 Class
#' 
#' @description
#' An S7 class for representing correlation matrices used in group sequential 
#' and graph-based multiple testing procedures. Provides type-safe storage and
#' validation for correlation matrices with proper mathematical constraints.
#' 
#' @field matrix A numeric matrix representing the correlation matrix
#' @field n_hypotheses Integer number of hypotheses 
#' @field n_analyses Integer number of analyses
#' @field column_names Character vector of column names (e.g., "H1_A1", "H2_A1")
#' 
#' @examples
#' library(tibble)
#' 
#' # Create sample event data
#' event_data <- tibble(
#'   H1 = c(1, 2, 1, 1, 2, 1),
#'   H2 = c(1, 2, 2, 1, 2, 2),
#'   Analysis = c(1, 1, 1, 2, 2, 2),
#'   Event = c(155, 160, 85, 305, 320, 170)
#' )
#' 
#' # Generate correlation matrix using traditional function
#' corr_matrix <- generate_corr(event_data)
#' 
#' # Create CorrelationMatrix object
#' corr_obj <- new_correlation_matrix(
#'   matrix = corr_matrix,
#'   n_hypotheses = 2,
#'   n_analyses = 2,
#'   column_names = colnames(corr_matrix)
#' )
#' 
#' print(corr_obj)
#' 
#' @export
CorrelationMatrix <- S7::new_class("CorrelationMatrix",
  properties = list(
    matrix = S7::new_S3_class("matrix"),
    n_hypotheses = S7::class_integer,
    n_analyses = S7::class_integer,
    column_names = S7::class_character
  ),
  
  constructor = function(matrix = matrix(numeric(), nrow = 0, ncol = 0),
                        n_hypotheses = 0L,
                        n_analyses = 0L,
                        column_names = character()) {
    
    # Auto-calculate dimensions if not provided
    if (length(matrix) > 0) {
      if (n_hypotheses == 0L) {
        # Try to infer from column names if available
        if (length(column_names) > 0) {
          n_hypotheses <- length(unique(sub("_.*", "", sub("^H", "", column_names))))
          n_analyses <- length(column_names) / n_hypotheses
        } else {
          # Try to infer from matrix dimensions (assuming square matrix)
          total_dim <- nrow(matrix)
          # For now, assume equal number of hypotheses and analyses
          n_hypotheses <- as.integer(sqrt(total_dim))
          n_analyses <- as.integer(total_dim / n_hypotheses)
        }
      }
      
      # Generate column names if not provided
      if (length(column_names) == 0) {
        column_names <- character()
        for (k in seq_len(n_analyses)) {
          for (i in seq_len(n_hypotheses)) {
            name_tmp <- paste("H", i, "_A", k, sep = "")
            column_names <- c(column_names, name_tmp)
          }
        }
      }
    }
    
    S7::new_object(S7::S7_object(),
      matrix = matrix,
      n_hypotheses = as.integer(n_hypotheses),
      n_analyses = as.integer(n_analyses), 
      column_names = column_names
    )
  },
  
  validator = function(self) {
    matrix <- self@matrix
    n_hypotheses <- self@n_hypotheses
    n_analyses <- self@n_analyses
    column_names <- self@column_names
    
    # Check basic properties
    if (!is.numeric(matrix)) {
      return("Matrix must be numeric")
    }
    
    if (length(matrix) > 0) {
      # Check matrix is square
      if (nrow(matrix) != ncol(matrix)) {
        return("Matrix must be square")
      }
      
      # Check dimensions consistency
      expected_dim <- n_hypotheses * n_analyses
      if (nrow(matrix) != expected_dim) {
        return(paste("Matrix dimensions (", nrow(matrix), "x", ncol(matrix), 
                    ") don't match n_hypotheses (", n_hypotheses, 
                    ") * n_analyses (", n_analyses, ") = ", expected_dim))
      }
      
      # Check column names length
      if (length(column_names) != expected_dim) {
        return(paste("Length of column_names (", length(column_names), 
                    ") must equal matrix dimensions (", expected_dim, ")"))
      }
      
      # Check matrix is symmetric (with tolerance for numerical precision)
      if (!isSymmetric(matrix, tol = 1e-10)) {
        return("Correlation matrix must be symmetric")
      }
      
      # Check diagonal elements are 1
      diag_elements <- diag(matrix)
      if (any(abs(diag_elements - 1) > 1e-10)) {
        return("Diagonal elements of correlation matrix must be 1")
      }
      
      # Check off-diagonal elements are between -1 and 1
      off_diag <- matrix[upper.tri(matrix) | lower.tri(matrix)]
      if (any(off_diag < -1 - 1e-10) || any(off_diag > 1 + 1e-10)) {
        return("Off-diagonal elements must be between -1 and 1")
      }
      
      # Check matrix is positive semi-definite (eigenvalues >= 0)
      eigenvals <- eigen(matrix, only.values = TRUE)$values
      if (any(eigenvals < -1e-10)) {
        return("Correlation matrix must be positive semi-definite")
      }
    }
    
    # Check n_hypotheses and n_analyses are positive
    if (n_hypotheses < 0) {
      return("n_hypotheses must be non-negative")
    }
    if (n_analyses < 0) {
      return("n_analyses must be non-negative")
    }
  }
)

#' Print method for CorrelationMatrix
#'
#' @name print.CorrelationMatrix
#' @param x A CorrelationMatrix S7 object
#' @param ... Additional arguments passed to print methods
#' @return Invisibly returns the input object
#' @method print CorrelationMatrix
S7::method(print, CorrelationMatrix) <- function(x, ...) {
  cat("<wpgsd::CorrelationMatrix>\n")
  cat("  @ matrix        : num [", nrow(x@matrix), " × ", ncol(x@matrix), "] correlation matrix\n", sep = "")
  cat("  @ n_hypotheses  : int", x@n_hypotheses, "\n")
  cat("  @ n_analyses    : int", x@n_analyses, "\n")
  cat("  @ column_names  : chr [1:", length(x@column_names), "] ", sep = "")
  if (length(x@column_names) > 0) {
    cat('"', paste(head(x@column_names, 3), collapse = '" "'), '"', sep = "")
    if (length(x@column_names) > 3) cat(" ...")
  }
  cat("\n")
  
  if (nrow(x@matrix) > 0) {
    cat("\nCorrelation Matrix:\n")
    # Print matrix with column names
    matrix_to_print <- x@matrix
    colnames(matrix_to_print) <- x@column_names
    rownames(matrix_to_print) <- x@column_names
    print(round(matrix_to_print, 4))
  }
  
  invisible(x)
}

#' Create a new CorrelationMatrix object
#'
#' @description
#' Constructor function to create a CorrelationMatrix S7 object with validation.
#' This function provides a user-friendly interface to create CorrelationMatrix objects.
#'
#' @param matrix A numeric matrix representing the correlation matrix
#' @param n_hypotheses Integer number of hypotheses (optional, inferred if not provided)
#' @param n_analyses Integer number of analyses (optional, inferred if not provided)  
#' @param column_names Character vector of column names (optional, generated if not provided)
#'
#' @return A CorrelationMatrix S7 object
#'
#' @examples
#' # Create a simple 2x2 correlation matrix
#' corr_mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
#' corr_obj <- new_correlation_matrix(matrix = corr_mat, n_hypotheses = 1, n_analyses = 2)
#' print(corr_obj)
#'
#' @export
new_correlation_matrix <- function(matrix = matrix(numeric(), nrow = 0, ncol = 0),
                                  n_hypotheses = 0L,
                                  n_analyses = 0L,
                                  column_names = character()) {
  CorrelationMatrix(
    matrix = matrix,
    n_hypotheses = n_hypotheses,
    n_analyses = n_analyses,
    column_names = column_names
  )
}

#' Convert matrix to CorrelationMatrix object
#'
#' @description
#' Converts a regular numeric matrix to a CorrelationMatrix S7 object with validation.
#'
#' @param matrix A numeric correlation matrix
#' @param n_hypotheses Integer number of hypotheses (optional)
#' @param n_analyses Integer number of analyses (optional)
#'
#' @return A CorrelationMatrix S7 object
#'
#' @examples
#' corr_mat <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
#' corr_obj <- as_correlation_matrix(corr_mat, n_hypotheses = 1, n_analyses = 2)
#' print(corr_obj)
#'
#' @export
as_correlation_matrix <- function(matrix, n_hypotheses = 0L, n_analyses = 0L) {
  new_correlation_matrix(
    matrix = matrix,
    n_hypotheses = n_hypotheses,
    n_analyses = n_analyses
  )
}

#' Extract correlation submatrix
#'
#' @description
#' Extract a submatrix from a CorrelationMatrix object based on analysis numbers
#' or hypothesis indices.
#'
#' @param x A CorrelationMatrix S7 object
#' @param analysis Optional vector of analysis numbers to include
#' @param hypotheses Optional vector of hypothesis indices to include
#'
#' @return A new CorrelationMatrix object containing only the specified subset
#'
#' @examples
#' library(tibble)
#' 
#' # Create sample data and correlation matrix
#' event_data <- tibble(
#'   H1 = c(1, 2, 1, 1, 2, 1),
#'   H2 = c(1, 2, 2, 1, 2, 2),
#'   Analysis = c(1, 1, 1, 2, 2, 2),
#'   Event = c(155, 160, 85, 305, 320, 170)
#' )
#' corr_matrix <- generate_corr(event_data)
#' corr_obj <- new_correlation_matrix(
#'   matrix = corr_matrix,
#'   n_hypotheses = 2,
#'   n_analyses = 2
#' )
#' 
#' # Extract subset for analysis 1 only
#' subset_corr <- subset_correlation_matrix(corr_obj, analysis = 1)
#' 
#' @export
subset_correlation_matrix <- function(x, analysis = NULL, hypotheses = NULL) {
  if (!S7::S7_inherits(x, CorrelationMatrix)) {
    stop("x must be a CorrelationMatrix object")
  }
  
  n_hypotheses <- x@n_hypotheses
  n_analyses <- x@n_analyses
  
  # Default to all if not specified
  if (is.null(analysis)) analysis <- seq_len(n_analyses)
  if (is.null(hypotheses)) hypotheses <- seq_len(n_hypotheses)
  
  # Create indices for subsetting
  indices <- integer(0)
  new_column_names <- character(0)
  
  for (a in analysis) {
    for (h in hypotheses) {
      idx <- (a - 1) * n_hypotheses + h
      if (idx <= length(x@column_names)) {
        indices <- c(indices, idx)
        new_column_names <- c(new_column_names, x@column_names[idx])
      }
    }
  }
  
  # Extract submatrix
  if (length(indices) > 0) {
    subset_matrix <- x@matrix[indices, indices, drop = FALSE]
    
    new_correlation_matrix(
      matrix = subset_matrix,
      n_hypotheses = length(hypotheses),
      n_analyses = length(analysis),
      column_names = new_column_names
    )
  } else {
    new_correlation_matrix()
  }
}

#' Generate S7 CorrelationMatrix from EventTable or event data
#'
#' @description
#' Enhanced version of generate_corr() that returns a CorrelationMatrix S7 object
#' with proper validation and type safety. Uses the new compute_correlations()
#' function for mathematically rigorous correlation computation.
#'
#' This function requires an EventTable S7 object as input and returns a 
#' CorrelationMatrix with column ordering that matches generate_corr() 
#' (Analysis then Hypothesis: H1A1, H2A1, H1A2, H2A2, ...).
#'
#' @param event_table An EventTable S7 object containing validated event count data
#' @param check Logical indicating whether to perform input validation (default TRUE)
#'
#' @return A CorrelationMatrix S7 object containing the correlation matrix with
#'   proper validation and metadata, ordered by Analysis then Hypothesis
#'
#' @examples
#' library(tibble)
#' 
#' # Create EventTable S7 object
#' event_data <- tibble(
#'   H1 = c(1, 2, 1, 1, 2, 1),
#'   H2 = c(1, 2, 2, 1, 2, 2),
#'   Analysis = c(1, 1, 1, 2, 2, 2),
#'   Event = c(155, 160, 85, 305, 320, 170)
#' )
#' event_table <- new_event_table(data = event_data)
#' corr_matrix_s7 <- generate_corr_s7(event_table)
#' print(corr_matrix_s7)
#'
#' @export
generate_corr_s7 <- function(event_table, check = TRUE) {
  # Require EventTable S7 object
  if (!S7::S7_inherits(event_table, EventTable)) {
    stop("Input must be an EventTable S7 object. Use new_event_table() to create one.")
  }
  
  # Extract data from EventTable
  event_data <- event_table@data
  n_hypotheses <- event_table@n_hypotheses
  n_analyses <- event_table@n_analyses
  
  # Generate the correlation matrix using new rigorous function
  corr_matrix <- compute_correlations(event_data, check = check, return_matrix = TRUE)
  
  # Reorder matrix to match generate_corr() (Analysis then Hypothesis)
  # Current ordering: H1A1, H1A2, H2A1, H2A2 (hypothesis-major)
  # Desired ordering: H1A1, H2A1, H1A2, H2A2 (analysis-major)
  
  # Create new column names in analysis-major order
  col_names_new <- character(n_hypotheses * n_analyses)
  idx <- 1
  for (k in seq_len(n_analyses)) {
    for (m in seq_len(n_hypotheses)) {
      col_names_new[idx] <- paste0("H", m, "_A", k)
      idx <- idx + 1
    }
  }
  
  # Create mapping from hypothesis-major to analysis-major ordering
  reorder_idx <- integer(n_hypotheses * n_analyses)
  for (k in seq_len(n_analyses)) {
    for (m in seq_len(n_hypotheses)) {
      # Current position in hypothesis-major ordering (H1A1, H1A2, H2A1, H2A2)
      old_pos <- (m - 1) * n_analyses + k
      # Desired position in analysis-major ordering (H1A1, H2A1, H1A2, H2A2)  
      new_pos <- (k - 1) * n_hypotheses + m
      reorder_idx[new_pos] <- old_pos
    }
  }
  
  # Reorder matrix
  corr_matrix_reordered <- corr_matrix[reorder_idx, reorder_idx]
  colnames(corr_matrix_reordered) <- col_names_new
  rownames(corr_matrix_reordered) <- col_names_new
  
  # Create and return CorrelationMatrix S7 object
  new_correlation_matrix(
    matrix = corr_matrix_reordered,
    n_hypotheses = n_hypotheses,
    n_analyses = n_analyses,
    column_names = col_names_new
  )
}
