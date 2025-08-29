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
