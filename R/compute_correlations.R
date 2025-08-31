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

#' Check event data for correlation computation
#'
#' @description
#' Validates input event data for computing correlations between test statistics.
#' Performs comprehensive checks on data structure, values, and completeness.
#'
#' @param event A data frame with columns H1, H2, Analysis, and Event containing
#'   event count data for correlation computation
#'
#' @return Invisibly returns TRUE if all checks pass, otherwise throws an error
#'
#' @examples
#' library(tibble)
#' 
#' # Valid event data
#' event_data <- tibble(
#'   H1 = c(1, 2, 1, 1, 2, 1),
#'   H2 = c(1, 2, 2, 1, 2, 2),
#'   Analysis = c(1, 1, 1, 2, 2, 2),
#'   Event = c(155, 160, 85, 305, 320, 170)
#' )
#' check_event_data(event_data)
#'
#' @export
check_event_data <- function(event) {
  # Check that event is a data frame
  if (!is.data.frame(event)) {
    stop("'event' must be a data frame", call. = FALSE)
  }
  
  # Check required columns exist
  required_cols <- c("H1", "H2", "Analysis", "Event")
  missing_cols <- setdiff(required_cols, names(event))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  # Check H1 <= H2 for each row
  if (any(event$H1 > event$H2, na.rm = TRUE)) {
    stop("H1 must be <= H2 for all rows", call. = FALSE)
  }
  
  # Check uniqueness of H1, H2, Analysis combinations
  combo_check <- paste(event$H1, event$H2, event$Analysis, sep = "_")
  if (any(duplicated(combo_check))) {
    stop("Combinations of H1, H2, Analysis must be unique", call. = FALSE)
  }
  
  # Check Event is non-negative integer
  if (any(!is.numeric(event$Event) | event$Event < 0 | event$Event != floor(event$Event), na.rm = TRUE)) {
    stop("Event must be non-negative integers", call. = FALSE)
  }
  
  # Check Analysis values are sequential positive integers starting from 1
  unique_analyses <- sort(unique(as.integer(event$Analysis)))
  expected_analyses <- seq_len(max(unique_analyses))
  if (!identical(unique_analyses, expected_analyses)) {
    stop("Analysis values must be sequential positive integers starting from 1", call. = FALSE)
  }
  if (length(unique_analyses) <= 1) {
    stop("Analysis must have more than one unique value", call. = FALSE)
  }
  
  # Check H1 values are sequential integers starting from 1
  unique_h1 <- sort(unique(as.integer(event$H1)))
  expected_h1 <- seq_len(max(unique_h1))
  if (!identical(unique_h1, expected_h1)) {
    stop("H1 values must be sequential positive integers starting from 1", call. = FALSE)
  }
  if (length(unique_h1) <= 1) {
    stop("H1 must have more than one unique value", call. = FALSE)
  }
  
  # Check H2 values are sequential integers starting from 1
  unique_h2 <- sort(unique(as.integer(event$H2)))
  expected_h2 <- seq_len(max(unique_h2))
  if (!identical(unique_h2, expected_h2)) {
    stop("H2 values must be sequential positive integers starting from 1", call. = FALSE)
  }
  if (length(unique_h2) <= 1) {
    stop("H2 must have more than one unique value", call. = FALSE)
  }
  
  # Check that for each analysis k, if there's a row with H1 < H2, 
  # then there's also a row with H1 == H2
  K <- max(unique_analyses)
  for (k in seq_len(K)) {
    analysis_data <- event[event$Analysis == k, ]
    off_diagonal <- analysis_data[analysis_data$H1 < analysis_data$H2, ]
    
    if (nrow(off_diagonal) > 0) {
      # Check diagonal entries exist
      diagonal_data <- analysis_data[analysis_data$H1 == analysis_data$H2, ]
      if (nrow(diagonal_data) == 0) {
        stop("For Analysis ", k, ", off-diagonal entries exist but no diagonal entries found", 
             call. = FALSE)
      }
    }
  }
  
  # Check that for any off-diagonal entry, both corresponding diagonal entries exist
  for (i in seq_len(nrow(event))) {
    h1 <- event$H1[i]
    h2 <- event$H2[i]
    analysis <- event$Analysis[i]
    
    if (h1 < h2) {  # Off-diagonal entry
      # Check H1=h1, H2=h1 exists for this analysis
      diag_h1 <- event[event$H1 == h1 & event$H2 == h1 & event$Analysis == analysis, ]
      if (nrow(diag_h1) == 0) {
        stop("Missing diagonal entry: H1=", h1, ", H2=", h1, ", Analysis=", analysis, 
             call. = FALSE)
      }
      
      # Check H1=h2, H2=h2 exists for this analysis
      diag_h2 <- event[event$H1 == h2 & event$H2 == h2 & event$Analysis == analysis, ]
      if (nrow(diag_h2) == 0) {
        stop("Missing diagonal entry: H1=", h2, ", H2=", h2, ", Analysis=", analysis, 
             call. = FALSE)
      }
    }
  }
  
  invisible(TRUE)
}

#' Compute correlations from event data
#'
#' @description
#' Computes correlations between test statistics using the mathematically correct
#' formulation that ensures positive definiteness and proper asymptotic properties.
#'
#' @param event A data frame with columns H1, H2, Analysis, and Event containing
#'   event count data for correlation computation
#' @param check Logical indicating whether to perform input validation (default: TRUE)
#' @param return_matrix Logical indicating whether to return as matrix (TRUE) or 
#'   data frame (FALSE). Default: TRUE
#'
#' @return If return_matrix=TRUE, returns a symmetric correlation matrix. 
#'   If return_matrix=FALSE, returns a data frame with columns H1, H2, Analysis1, 
#'   Analysis2, Correlation.
#'
#' @examples
#' library(tibble)
#' 
#' # Sample event data
#' event_data <- tribble(
#'   ~H1, ~H2, ~Analysis, ~Event,
#'   1, 1, 1, 80,
#'   2, 2, 1, 100,
#'   1, 2, 1, 60,
#'   1, 1, 2, 120,
#'   2, 2, 2, 150,
#'   1, 2, 2, 80
#' )
#' 
#' # Get correlation matrix
#' corr_matrix <- compute_correlations(event_data)
#' 
#' # Get detailed data frame
#' corr_df <- compute_correlations(event_data, return_matrix = FALSE)
#'
#' @export
compute_correlations <- function(event, check = TRUE, return_matrix = TRUE) {
  # Perform input validation if requested
  if (check) {
    check_event_data(event)
  }
  
  M <- max(event$H1, event$H2)
  K <- max(event$Analysis)
  
  results <- data.frame(
    H1 = integer(0),
    H2 = integer(0),
    Analysis1 = integer(0),
    Analysis2 = integer(0),
    Correlation = numeric(0)
  )
  
  # Compute correlations for all pairs of test statistics
  for (m1 in seq_len(M)) {
    for (k1 in seq_len(K)) {
      for (m2 in seq_len(M)) {
        for (k2 in seq_len(K)) {
          if (m1 == m2 && k1 == k2) {
            # Diagonal correlation is 1
            corr_val <- 1
          } else if (m1 == m2) {
            # Same hypothesis, different analyses: n_mk1 / sqrt(n_mk1 * n_mk2)
            n_mk1_row <- event[event$H1 == m1 & event$H2 == m1 & event$Analysis == k1, ]
            n_mk2_row <- event[event$H1 == m2 & event$H2 == m2 & event$Analysis == k2, ]
            
            if (nrow(n_mk1_row) > 0 && nrow(n_mk2_row) > 0) {
              n_mk1 <- n_mk1_row$Event[1]
              n_mk2 <- n_mk2_row$Event[1]
              if (k1 <= k2) {
                corr_val <- n_mk1 / sqrt(n_mk1 * n_mk2)
              } else {
                corr_val <- n_mk2 / sqrt(n_mk1 * n_mk2)
              }
            } else {
              next  # Skip if data not available
            }
          } else if (k1 == k2) {
            # Same analysis, different hypotheses: n_m1m2k / sqrt(n_m1k * n_m2k)
            # Find the off-diagonal entry
            if (m1 < m2) {
              off_diag_row <- event[event$H1 == m1 & event$H2 == m2 & event$Analysis == k1, ]
            } else {
              off_diag_row <- event[event$H1 == m2 & event$H2 == m1 & event$Analysis == k1, ]
            }
            
            n_m1k_row <- event[event$H1 == m1 & event$H2 == m1 & event$Analysis == k1, ]
            n_m2k_row <- event[event$H1 == m2 & event$H2 == m2 & event$Analysis == k1, ]
            
            if (nrow(off_diag_row) > 0 && nrow(n_m1k_row) > 0 && nrow(n_m2k_row) > 0) {
              n_m1m2k <- off_diag_row$Event[1]
              n_m1k <- n_m1k_row$Event[1]
              n_m2k <- n_m2k_row$Event[1]
              corr_val <- n_m1m2k / sqrt(n_m1k * n_m2k)
            } else {
              next  # Skip if data not available
            }
          } else {
            # Different hypotheses and analyses: n_m1m2k1 / sqrt(n_m1k1 * n_m2k2)
            # Find the off-diagonal entry for the earlier analysis
            if (m1 < m2) {
              off_diag_row <- event[event$H1 == m1 & event$H2 == m2 & event$Analysis == min(k1, k2), ]
            } else {
              off_diag_row <- event[event$H1 == m2 & event$H2 == m1 & event$Analysis == min(k1, k2), ]
            }
            
            n_m1k1_row <- event[event$H1 == m1 & event$H2 == m1 & event$Analysis == k1, ]
            n_m2k2_row <- event[event$H1 == m2 & event$H2 == m2 & event$Analysis == k2, ]
            
            if (nrow(off_diag_row) > 0 && nrow(n_m1k1_row) > 0 && nrow(n_m2k2_row) > 0) {
              n_m1m2_early <- off_diag_row$Event[1]
              n_m1k1 <- n_m1k1_row$Event[1]
              n_m2k2 <- n_m2k2_row$Event[1]
              corr_val <- n_m1m2_early / sqrt(n_m1k1 * n_m2k2)
            } else {
              next  # Skip if data not available
            }
          }
          
          # Add result
          results <- rbind(results, data.frame(
            H1 = m1,
            H2 = m2, 
            Analysis1 = k1,
            Analysis2 = k2,
            Correlation = corr_val
          ))
        }
      }
    }
  }
  
  if (return_matrix) {
    return(gen_corr(results, M = M, K = K))
  } else {
    return(results)
  }
}

#' Convert correlation data frame to correlation matrix
#'
#' @description
#' Transforms a data frame of correlation results into a symmetric correlation matrix
#' with proper labeling.
#'
#' @param corr_df A data frame with columns H1, H2, Analysis1, Analysis2, Correlation
#' @param M Number of hypotheses (if NULL, inferred from data)
#' @param K Number of analyses (if NULL, inferred from data)
#'
#' @return A symmetric correlation matrix of size (M*K) x (M*K)
#'
#' @examples
#' # Create sample correlation data frame
#' corr_df <- data.frame(
#'   H1 = c(1, 1, 2, 1, 2, 2),
#'   H2 = c(1, 2, 2, 1, 1, 2),
#'   Analysis1 = c(1, 1, 1, 2, 2, 2),
#'   Analysis2 = c(1, 1, 1, 2, 2, 2),
#'   Correlation = c(1, 0.5, 1, 1, 1, 1)
#' )
#' 
#' corr_matrix <- gen_corr(corr_df, M = 2, K = 2)
#'
#' @export
gen_corr <- function(corr_df, M = NULL, K = NULL) {
  # Infer M and K if not provided
  if (is.null(M)) M <- max(corr_df$H1, corr_df$H2)
  if (is.null(K)) K <- max(corr_df$Analysis1, corr_df$Analysis2)
  
  # Create matrix dimensions: M * K
  n_dim <- M * K
  
  # Initialize correlation matrix
  corr_matrix <- matrix(0, nrow = n_dim, ncol = n_dim)
  
  # Create row and column names (hypothesis-major order: H1A1, H1A2, H2A1, H2A2)
  row_names <- character(n_dim)
  for (m in seq_len(M)) {
    for (k in seq_len(K)) {
      idx <- (m - 1) * K + k
      row_names[idx] <- paste0("H", m, "_A", k)
    }
  }
  rownames(corr_matrix) <- row_names
  colnames(corr_matrix) <- row_names
  
  # Fill matrix using proper indexing (hypothesis-major)
  for (i in seq_len(nrow(corr_df))) {
    h1 <- corr_df$H1[i]
    h2 <- corr_df$H2[i]
    k1 <- corr_df$Analysis1[i]
    k2 <- corr_df$Analysis2[i]
    corr_val <- corr_df$Correlation[i]
    
    # Convert to matrix indices (hypothesis-major: H1A1, H1A2, H2A1, H2A2)
    row_idx <- (h1 - 1) * K + k1
    col_idx <- (h2 - 1) * K + k2
    
    # Fill both symmetric positions
    corr_matrix[row_idx, col_idx] <- corr_val
    corr_matrix[col_idx, row_idx] <- corr_val
  }
  
  return(corr_matrix)
}
