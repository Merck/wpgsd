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

#' @importFrom S7 new_class new_object

#' EventTable S7 Class
#' 
#' Represents event count data structure used in generate_corr() and generate_event_table()
#' 
#' @param data A tibble containing H1, H2, Analysis, and Event columns
#' 
#' @export
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
    }
  }
)

# TODO: Add S7 method definitions later
# #' Print method for EventTable
# S7::method(show, EventTable) <- function(object) { ... }
# 
# #' Summary method for EventTable  
# S7::method(summary, EventTable) <- function(object, ...) { ... }

#' Subset method for EventTable
#' 
#' @param x An EventTable object
#' @param analysis Analysis number(s) to subset
#' @param hypotheses Hypothesis indices to subset
#' @param ... Additional arguments (unused)
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

#' Convert tibble to EventTable
#' 
#' @param data A tibble with H1, H2, Analysis, Event columns
#' 
#' @export
as_event_table <- function(data) {
  if (S7::S7_inherits(data, EventTable)) {
    return(data)
  }
  EventTable(data = data)
}

#' Validate EventTable data format
#' 
#' @param data A data frame or tibble to validate
#' 
#' @return TRUE if valid, stops with error message if not
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
