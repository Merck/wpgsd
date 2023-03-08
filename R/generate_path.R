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

#' Generate path for Merck studies on CPI drive
#'
#' @param env environment. prod or test.
#' @param project project. e.g. mk3475-tnbc
#' @param deliverables a vector containing deliverable folder name of each analysis. e.g. c("prot001-ia01", "prot001-ia02")
#'
#' @return data path(s) pointing to the folders containing data sets of each analysis
#' @export
#'
#' @examples 
#' paths <- generate_path(env = "prod",
#'                        project = "mk3475-tnbc",
#'                        deliverables = c("prot355-ia01-eff", "prot355-ia02-eff"))
#' paths
#' 
generate_path <- function(env, project, deliverables){
  paths <- NULL
  for (i in 1:length(deliverables)){
    tmp <- paste("/opt/bardsar/", env, "/", project, "/", deliverables[i], "/dataanalysis", sep = "") 
    tmp <- tolower(tmp)
    paths <- c(paths, tmp)
  }
  return(paths)
}

