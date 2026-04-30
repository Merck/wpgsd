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

#' Generate correlation matrix based on event counts
#'
#' @param event Event count of each hypothesis at each analysis, including
#'   event count of the intersection of hypotheses.
#'   It contains 4 columns: `H1`, `H2`, `Analysis`, `Event`.
#'   `H1` needs to be listed as 1, 2, 3, etc. as numbers.
#'
#' @return A correlation matrix.
#'
#' @importFrom dplyr filter select %>%
#'
#' @export
#'
#' @examples
#' # Build the transition matrix
#' m <- matrix(c(
#'   0, 0.5, 0.5,
#'   0.5, 0, 0.5,
#'   0.5, 0.5, 0
#' ), nrow = 3, byrow = TRUE)
#' # initialize weights
#' w <- c(1 / 3, 1 / 3, 1 / 3)
#'
#' # Input event count of intersection of paired hypotheses - Table 2
#' event <- tibble::tribble(
#'   ~H1, ~H2, ~Analysis, ~Event,
#'   1, 1, 1, 155,
#'   2, 2, 1, 160,
#'   3, 3, 1, 165,
#'   1, 2, 1, 85,
#'   1, 3, 1, 85,
#'   2, 3, 1, 85,
#'   1, 1, 2, 305,
#'   2, 2, 2, 320,
#'   3, 3, 2, 335,
#'   1, 2, 2, 170,
#'   1, 3, 2, 170,
#'   2, 3, 2, 170
#' )
#'
#' # Generate correlation from events
#' gs_corr <- generate_corr(event)
generate_corr <- function(event) {
  elem <- event %>% subset(H1 == H2)
  inter <- event %>% subset(H1 != H2)
  n_hypotheses <- max(as.numeric(elem$H1))
  n_analyses <- max(elem$Analysis)

  # Diagonal
  D <- diag(elem$Event)

  # Within hypothesis across analyses
  # For each hypothesis i and each pair of analyses (j, k) where j < k,

  # the shared event count is the diagonal entry at the earlier analysis j.
  if (n_analyses > 1) {
    for (i in 1:n_hypotheses) {
      for (j in 1:(n_analyses - 1)) {
        count <- D[(j - 1) * n_hypotheses + i, (j - 1) * n_hypotheses + i]
        for (k in (j + 1):n_analyses) {
          D[(j - 1) * n_hypotheses + i, (k - 1) * n_hypotheses + i] <- count
          D[(k - 1) * n_hypotheses + i, (j - 1) * n_hypotheses + i] <- count
        }
      }
    }
  }

  # Between hypotheses
  for (i in 1:(n_hypotheses - 1)) {
    for (j in (i + 1):n_hypotheses) {
      for (k in 1:n_analyses) {
        count1 <- as.numeric(event %>%
          subset(((H1 == i & H2 == j) | (H1 == j & H2 == i)) & Analysis == k) %>%
          select(Event))[1]
        for (l in k:n_analyses) {
          D[n_hypotheses * (l - 1) + i, n_hypotheses * (k - 1) + j] <- count1
          D[n_hypotheses * (l - 1) + j, n_hypotheses * (k - 1) + i] <- count1
          D[n_hypotheses * (k - 1) + j, n_hypotheses * (l - 1) + i] <- count1
          D[n_hypotheses * (k - 1) + i, n_hypotheses * (l - 1) + j] <- count1
        }
      }
    }
  }

  corr_mat <- d_corr(D)

  col_names <- NULL
  for (k in 1:n_analyses) {
    for (i in 1:n_hypotheses) {
      name_tmp <- paste("H", i, "_A", k, sep = "")
      col_names <- c(col_names, name_tmp)
    }
  }

  colnames(corr_mat) <- col_names

  return(corr_mat)
}
