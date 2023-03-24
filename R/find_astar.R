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

#' Utility function for root-finding to compute crossing probabilities
#' with the overall alpha spending approach
#'
#' @param a Cumulative overall alpha spending up to current analysis.
#' @param alpha_prev alpha boundary at previous interim analyses using
#'   the WPGSD approach.
#' @param astar Total nominal alpha level at current analysis from
#'   the WPGSD approach.
#' @param w Vector of alpha weights at current analysis.
#' @param sig Correlation matrix of previous and current analyses test statistics.
#' @param maxpts GenzBretz function maximum number of function values as integer.
#' @param abseps GenzBretz function absolute error tolerance.
#' @param ... Additional arguments.
#'
#' @return Difference. Should be 0 with `a` and `astar` identified.
#'
#' @export
#'
#' @examples
#' # Input event count of intersection of paired hypotheses - Table 2
#' my_event <- tibble::tribble(
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
#' my_corr <- generate_corr(my_event)
#'
#' # Find the inflation factor for H1, H2 at analysis 1
#' find_astar(
#'   a = 0.0008708433,
#'   alpha_prev = NULL,
#'   aprime = c(0.0004588644, 0.0004119789),
#'   astar = 1,
#'   w = c(0.5, 0.5),
#'   sig = my_corr[
#'     colnames(my_corr) %in% c("H1_A1", "H2_A1"),
#'     colnames(my_corr) %in% c("H1_A1", "H2_A1")
#'   ]
#' )
find_astar <- function(a, alpha_prev = NULL, astar, w, sig, maxpts = 50000, abseps = 0.00001, ...) {
  # Remove column name for proper pmvnorm run
  colnames(sig) <- NULL

  if (is.null(alpha_prev)) {
    res <- 1 - a - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = qnorm(1 - w * astar),
      sigma = sig,
      algorithm = mvtnorm::GenzBretz(maxpts = maxpts, abseps = abseps)
    )
  } else {
    res <- 1 - a - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = c(qnorm(1 - alpha_prev), qnorm(1 - w * astar)),
      sigma = sig,
      algorithm = mvtnorm::GenzBretz(maxpts = maxpts, abseps = abseps)
    )
  }
  return(res)
}
