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

#' Perform closed testing procedure
#'
#' @param bounds A tibble of nominal p-value boundaries from [generate_bounds()]
#'   containing columns `Analysis`, `Hypotheses`, `H1`, `H2`, etc.
#' @param p_obs A tibble of observed p-values containing columns
#'   `Analysis`, `H1`, `H2`, etc.
#'
#' @return An outcome matrix summarizing the testing results.
#'
#' @importFrom dplyr %>% filter num_range select
#'
#' @export
#'
#' @examples
#' p_obs <- dplyr::bind_rows(
#'   tibble::tibble(Analysis = 1, H1 = 0.001, H2 = 0.001),
#'   tibble::tibble(Analysis = 2, H1 = 0.001, H2 = 0.001)
#' )
#' bound <- tibble::tribble(
#'   ~Analysis, ~Hypotheses, ~H1, ~H2,
#'   1, "H1", 0.02, NA,
#'   1, "H1, H2", 0.0001, 0.00001,
#'   1, "H2", NA, 0.003,
#'   2, "H1", 0.02, NA,
#'   2, "H1, H2", 0.02, 0.00001,
#'   2, "H2", NA, 0.003
#' )
#'
#' closed_test <- closed_test(bound, p_obs)
closed_test <- function(bounds, p_obs) {
  n_analyses <- max(p_obs$Analysis)
  n_hypotheses <- ncol(p_obs) - 1

  result <- NULL

  for (i in 1:n_analyses) {
    # results comparing p-value with bound at current analysis
    p_tmp <- p_obs %>%
      filter(Analysis == i) %>%
      select(num_range("H", 1:n_hypotheses))
    bounds_tmp <- bounds %>%
      filter(Analysis == i) %>%
      select(num_range("H", 1:n_hypotheses))
    test_raw <- c(unlist(p_tmp)) < t(bounds_tmp)

    # number of intersection hypothesis
    n_inter <- ncol(test_raw)

    # initial testing result of each intersection hypothesis
    test_inter <- apply(test_raw, 2, any, na.rm = TRUE)

    # if a hypothesis was rejected in a previous analysis, then all
    # intersection hypothesis including that hypothesis is rejected
    if (i != 1) {
      # previous testing results
      prev_res <- apply(result %>% dplyr::select(num_range("H", 1:n_hypotheses)), 2, any)
      # hypothesis number that was rejected in any previous analyses
      prev_reject <- c(1:n_hypotheses)[prev_res]
      # intersection hypothesis that includes previous rejected hypothesis
      inter_reject <- matrix(!is.na(test_raw[prev_reject, ]), ncol = n_inter)
      indx_inter_reject <- c(1:n_inter)[apply(inter_reject, 2, sum) > 0]
      # convert testing result to TRUE for above intersection hypothesis
      test_inter[indx_inter_reject] <- TRUE
    }

    # testing result of each elementary hypothesis
    test_tmp <- rep(NA, n_hypotheses)
    for (j in 1:n_hypotheses) {
      indx <- !is.na(test_raw[j, ])
      test_elem <- all(test_inter[indx])
      test_tmp[j] <- test_elem
    }
    names(test_tmp) <- paste("H", 1:n_hypotheses, sep = "")
    test_tmp <- data.frame(t(test_tmp))
    test_tmp$Analysis <- paste("Analysis", i)
    result <- dplyr::bind_rows(result, test_tmp)
  }

  result[result == TRUE] <- "Success"
  result[result == FALSE] <- "Fail"
  rownames(result) <- NULL

  return(result)
}
