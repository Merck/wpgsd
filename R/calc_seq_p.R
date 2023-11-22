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

#' Calculate sequential p-values for interaction/elementary hypothesis
#'
#' @param test_analysis The index of the analysis to be tested, such as 1, 2, ...
#' @param test_hypothesis A character of the tested interaction/elementary hypothesis,
#'   such as `"H1, H2, H3"`, `H1, H2`, `"H1"`.
#' @param p_obs Observed p-values up to `test_analysis`.
#' @param n_analysis Total number of analysis.
#' @param alpha_spending_type Type Boundary type.
#'   - `0` - Bonferroni. Separate alpha spending for each hypotheses.
#'   - `1` - Fixed alpha spending for all hypotheses. Method 3a in the manuscript.
#'   - `2` - Overall alpha spending for all hypotheses. Method 3b in the manuscript.
#'   - `3` - Separate alpha spending for each hypotheses. Method 3c in the manuscript.
#' @param initial_weight Initial weight assigned to the elementary hypothesis.
#' @param transition_mat Transition matrix.
#' @param z_corr Correlation matrix of the Z statistics.
#' @param spending_fun Spending function.
#' @param spending_fun_par Parameter of the spending function.
#' @param info_frac Information fractions.
#' @param interval Interval to search the uniroot.
#'
#' @return The sequential p-values of the `test_hypothesis` at the `test_analysis`.
#'
#' @importFrom dplyr %>% filter num_range select
#' @importFrom stats uniroot
#'
#' @export
#'
#' @examples
#' calc_seq_p(
#'   test_analysis = 2,
#'   test_hypothesis = "H1, H2, H3",
#'   p_obs = tibble(
#'     analysis = 1:2,
#'     H1 = c(0.02, 0.0015),
#'     H2 = c(0.01, 0.01),
#'     H3 = c(0.01, 0.004)
#'   ),
#'   alpha_spending_type = 2,
#'   n_analysis = 2,
#'   initial_weight = c(0.3, 0.3, 0.4),
#'   transition_mat = matrix(c(
#'     0.0000000, 0.4285714, 0.5714286,
#'     0.4285714, 0.0000000, 0.5714286,
#'     0.5000000, 0.5000000, 0.0000000
#'   ), nrow = 3, byrow = TRUE),
#'   z_corr = matrix(
#'     c(
#'       1.0000000, 0.7627701, 0.6666667, 0.7071068, 0.5393599, 0.4714045,
#'       0.7627701, 1.0000000, 0.6992059, 0.5393599, 0.7071068, 0.4944132,
#'       0.6666667, 0.6992059, 1.0000000, 0.4714045, 0.4944132, 0.7071068,
#'       0.7071068, 0.5393599, 0.4714045, 1.0000000, 0.7627701, 0.6666667,
#'       0.5393599, 0.7071068, 0.4944132, 0.7627701, 1.0000000, 0.6992059,
#'       0.4714045, 0.4944132, 0.7071068, 0.6666667, 0.6992059, 1.0000000
#'     ),
#'     nrow = 6, byrow = TRUE
#'   ),
#'   spending_fun = gsDesign::sfHSD,
#'   spending_fun_par = -4,
#'   info_frac = c(0.5, 1),
#'   interval = c(1e-4, 0.2)
#' )
calc_seq_p <- function(
    test_analysis = 2,
    test_hypothesis = "H1, H2, H3",
    p_obs = tibble(
      analysis = 1:2,
      H1 = c(0.02, 0.0015),
      H2 = c(0.01, 0.01),
      H3 = c(0.01, 0.004)
    ),
    alpha_spending_type = 3,
    n_analysis = 2,
    initial_weight = c(0.3, 0.3, 0.4),
    transition_mat = matrix(c(
      0.0000000, 0.4285714, 0.5714286,
      0.4285714, 0.0000000, 0.5714286,
      0.5000000, 0.5000000, 0.0000000
    ), nrow = 3, byrow = TRUE),
    z_corr = matrix(
      c(
        1.0000000, 0.7627701, 0.6666667, 0.7071068, 0.5393599, 0.4714045,
        0.7627701, 1.0000000, 0.6992059, 0.5393599, 0.7071068, 0.4944132,
        0.6666667, 0.6992059, 1.0000000, 0.4714045, 0.4944132, 0.7071068,
        0.7071068, 0.5393599, 0.4714045, 1.0000000, 0.7627701, 0.6666667,
        0.5393599, 0.7071068, 0.4944132, 0.7627701, 1.0000000, 0.6992059,
        0.4714045, 0.4944132, 0.7071068, 0.6666667, 0.6992059, 1.0000000
      ),
      nrow = 6, byrow = TRUE
    ),
    spending_fun = gsDesign::sfHSD,
    spending_fun_par = -4,
    info_frac = c(0.5, 1),
    interval = c(1e-4, 0.2)) {
  foo <- function(x) {
    all_hypothesis <- strsplit(test_hypothesis, split = ", ") %>% unlist()
    all_hypothesis_idx <- as.numeric(gsub(".*?([0-9]+).*", "\\1", all_hypothesis))

    ans <- generate_bounds(
      type = alpha_spending_type,
      k = n_analysis,
      w = initial_weight,
      m = transition_mat,
      corr = z_corr,
      alpha = x,
      sf = spending_fun,
      sfparm = spending_fun_par,
      t = info_frac
    ) %>%
      arrange(Analysis) %>%
      filter(Analysis <= test_analysis, Hypotheses == test_hypothesis)

    p_diff <- NULL
    for (hhh in all_hypothesis) {
      p_diff_new <- p_obs[[hhh]] - ans[[hhh]]
      p_diff <- c(p_diff, p_diff_new)
    }

    return(min(p_diff))
  }

  seq_p <- uniroot(foo, lower = interval[1], upper = interval[2])$root

  return(seq_p)
}
