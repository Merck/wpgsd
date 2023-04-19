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
#' gs_corr <- generate_corr_new(event)
generate_corr_new <- function(event) {
  elem <- event %>% subset(H1 == H2)
  inter <- event %>% subset(H1 != H2)
  n_hypotheses <- max(as.numeric(elem$H1))
  n_analyses <- max(elem$Analysis)

  # Diagonal
  D <- diag(elem$Event)

  # Within hypothesis across analyses
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

  # Between hypotheses and analyses
  for (i in 1:(n_hypotheses - 1)) {
    for (j in c((i + 1):n_hypotheses)) {
      for (k in 1:n_analyses) {
        count1 <- as.numeric(event %>%
          subset(((H1 == i & H2 == j) | (H1 == j & H2 == i)) & Analysis == k) %>%
          select(Event))[1]
        for (l in (k:n_analyses)) {
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

a1 <- 80
b1 <- 100
ab1 <- 60
a2 <- 120
b2 <- 150
ab2 <- 80

event <- tibble::tribble(
  ~H1, ~H2, ~Analysis, ~Event,
  1, 1, 1, a1,
  2, 2, 1, b1,
  1, 2, 1, ab1,
  1, 1, 2, a2,
  2, 2, 2, b2,
  1, 2, 2, ab2
)

test_that("2 endpoints 2 analysis correlation as expected", {
  corr <- generate_corr_new(event)
  corr_test <- matrix(
    c(
      1, ab1 / sqrt(a1 * b1), a1 / sqrt(a1 * a2), ab1 / sqrt(a1 * b2),
      ab1 / sqrt(a1 * b1), 1, ab1 / sqrt(a2 * b1), b1 / sqrt(b1 * b2),
      a1 / sqrt(a1 * a2), ab1 / sqrt(a2 * b1), 1, ab2 / sqrt(a2 * b2),
      ab1 / sqrt(a1 * b2), b1 / sqrt(b1 * b2), ab2 / sqrt(a2 * b2), 1
    ),
    nrow = 4, byrow = TRUE
  )

  expect_equal(matrix(corr %>% as.numeric(), nrow = 4, byrow = TRUE), corr_test)
})


# this is a 2 hypothesis, 3 analysis example
test_that("2 hypotheses 3 analysis correlation as expected", {
  event <- tibble::tribble(
    ~Analysis, ~H1, ~H2, ~Event,
    1, 1, 1, 147,
    1, 2, 2, 167,
    1, 1, 2, 88,
    2, 1, 1, 278,
    2, 2, 2, 289,
    2, 1, 2, 158,
    3, 1, 1, 342,
    3, 2, 2, 350,
    3, 1, 2, 192
  )
  corr <- generate_corr_new(event)
  n_hypotheses <- 2
  n_analyses <- 3
  corr_test <- diag(1, n_hypotheses * n_analyses)
  for (k in 1:n_analyses) {
    for (l in k:n_analyses) {
      for (i in 1:(n_hypotheses)) {
        countkii <- as.numeric((event %>% filter(H1 == i & H2 == i & Analysis == k))$Event)
        for (j in i:n_hypotheses) {
          countkjj <- as.numeric((event %>% filter(H1 == j & H2 == j & Analysis == k))$Event)
          countljj <- as.numeric((event %>% filter(H1 == j & H2 == j & Analysis == l))$Event)
          countlii <- as.numeric((event %>% filter(H1 == i & H2 == i & Analysis == l))$Event)
          countkij <- as.numeric((event %>% filter(H1 == i & H2 == j & Analysis == k))$Event)
          corr_test[(k - 1) * n_hypotheses + i, (l - 1) * n_hypotheses + j] <- countkij / sqrt(countkii * countljj)
          corr_test[(k - 1) * n_hypotheses + j, (l - 1) * n_hypotheses + i] <- countkij / sqrt(countkjj * countlii)
          corr_test[(l - 1) * n_hypotheses + i, (k - 1) * n_hypotheses + j] <- countkij / sqrt(countlii * countkjj)
          corr_test[(l - 1) * n_hypotheses + j, (k - 1) * n_hypotheses + i] <- countkij / sqrt(countljj * countkii)
        }
      }
    }
  }

  expect_equal(matrix(corr %>% as.numeric(), nrow = 6, byrow = TRUE), corr_test)
})
