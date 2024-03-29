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

#' Convert event matrix to correlation matrix
#'
#' @param D Event matrix.
#'
#' @return Correlation matrix.
#'
#' @noRd
#'
#' @examples
#' d_corr(D = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3))
d_corr <- function(D) {
  B <- matrix(0, nrow = nrow(D), ncol = nrow(D))
  diag(B) <- 1 / sqrt(diag(D))
  return(B %*% D %*% B)
}
