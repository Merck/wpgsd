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

#'  Generate table of event counts from ADSL and ADTTE SAS datasets
#'
#' @param paths A vector of paths for analysis datasets. Length should be equal to the number of analyses completed
#' @param h_select Selection criterion for each hypothesis. Should be a tibble containing 2 columns of Hypothesis and Crit
#' @param adsl_name SAS dataset name for subject-level analysis data. Usually it is "adsl".
#' @param adtte_name SAS dataset name for time-to-event analysis data. Usually it is "adtte".
#' @param key_var Key variable to join the adsl and adtte datasets. e.g. "USUBJID" or "SUBJID"
#' @param cnsr_var Variable to indicate censoring (1 = censor; 0 = event). e.g. "CNSR".
#'
#' @return A list with two components:
#'   - `event`: an event count table as input for [generate_bounds()].
#'   - `dsets`: analysis datasets of each hypothesis.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect all_of
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' paths <- system.file("extdata/", package = "wpgsd")
#'
#' # Selection criteria for each hypothesis
#' h_select <- tibble::tribble(
#'   ~Hypothesis, ~Crit,
#'   1, "PARAMCD == 'OS' & TRT01P %in% c('Xanomeline High Dose', 'Placebo')",
#'   2, "PARAMCD == 'OS' & TRT01P %in% c('Xanomeline Low Dose', 'Placebo')"
#' )
#'
#' event <- generate_event_table(paths, h_select,
#'   adsl_name = "adsl", adtte_name = "adtte",
#'   key_var = "USUBJID", cnsr_var = "CNSR"
#' )$event
#'
#' event %>%
#'   kableExtra::kbl(caption = "Event Count - Compute from SAS Datasets Example", align = "l") %>%
#'   kableExtra::kable_classic_2(full_width = FALSE)
generate_event_table <- function(paths, h_select,
                                 adsl_name, adtte_name,
                                 key_var, cnsr_var) {
  event <- NULL
  dsets <- list()
  for (i in seq_along(paths)) { # number of path is number of analysis
    path <- paths[i]
    adsl <- haven::read_sas(paste(path, "/", adsl_name, ".sas7bdat", sep = ""))
    adtte <- haven::read_sas(paste(path, "/", adtte_name, ".sas7bdat", sep = ""))
    dset <- dplyr::left_join(adtte, adsl, by = key_var, suffix = c("", ".y"))

    for (j in seq_len(nrow(h_select))) { # number of time-to-event hypotheses
      h_var <- paste("H", j, sep = "")
      crit <- h_select[j, 2]
      dset <- dset %>% dplyr::mutate(!!h_var := ifelse(eval(str2expression(as.character(crit))),
        1, 0
      ))
      event_tmp <- tibble(
        H1 = paste(j),
        H2 = paste(j),
        Analysis = i,
        Event = sum(dset %>%
          filter(eval(str2expression(as.character(cnsr_var))) == 0) %>%
          select(all_of(h_var)), na.rm = TRUE)
      )
      event <- rbind(event, event_tmp)

      if (j > 1) {
        for (k in 1:(j - 1)) {
          h_var_k <- paste("H", k, sep = "")

          event_tmp <- tibble(
            H1 = paste(k),
            H2 = paste(j),
            Analysis = i,
            Event = sum(dset %>%
              filter(eval(str2expression(as.character(cnsr_var))) == 0 & eval(str2expression(as.character(h_var_k))) == 1) %>%
              select(all_of(h_var)), na.rm = TRUE)
          )
          event <- rbind(event, event_tmp)
        }
      }
    }

    dsets[[i]] <- dset
  }

  return(list(event = event, dsets = dsets))
}
