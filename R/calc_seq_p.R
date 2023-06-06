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
#' such as `"H1, H2, H3"`, `H1, H2`, `"H1"`.
#' @param p_obs Observed p-values
#' @param n_analysis Total number of analysis
#' @param alpha_spending_type Type Boundary type.
#'   - `0` = Bonferroni. Separate alpha spending for each hypotheses.
#'   - `1` = Fixed alpha spending for all hypotheses. Method 3a in the manuscript.
#'   - `2` = Overall alpha spending for all hypotheses. Method 3b in the manuscript.
#'   - `3` = Separate alpha spending for each hypotheses. Method 3c in the manuscript.
#' @param initial_weight Initial weight assigned to the elementary hypothesis
#' @param z_corr Correlation matrix of the Z statistics
#' @param spending_fun Spending function
#' @param spending_fun_par Parameter of the spending function
#' @param info_frac Information fractions
#' @param interval Interval to search the uniroot
#' @return The sequential p-values of the `test_hypothesis` at the `test_analysis`
#'
#' @importFrom dplyr %>% filter num_range select
#' @importFrom stats uniroot
#' @importFrom stringr str_split
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
calc_seq_p <- function(test_analysis = 1, # stage of interest
                       test_hypothesis = "H1, H2, H3",
                       p_obs,             # observed p-value
                       alpha_spending_type = 3,
                       n_analysis = 2,
                       initial_weight,
                       transition_mat,
                       z_corr,
                       spending_fun,
                       spending_fun_par,
                       info_frac,
                       interval = c(1e-4, 0.2) # interval for uniroot
){
  foo <- function(x){
    all_hypothesis <- stringr::str_split(test_hypothesis, pattern = ", ") %>% unlist()
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
      filter(Analysis == test_analysis, Hypotheses == test_hypothesis)
    
    p_bound <- NULL
    for (hhh in all_hypothesis) {
      p_bound <- c(p_bound, ans[[hhh]])
    }
    
    return(min(p_obs[all_hypothesis_idx] - p_bound))
  }
  
  seq_p <- uniroot(foo, lower = interval[1], upper = interval[2])$root
  
  return(seq_p)
}