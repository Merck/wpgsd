#' generate_event_table_cc
#'
#' This function generates a table of events for given experimental arms and a control group based on specified hypotheses.
#'
#' @param input_data A dataframe containing at least two columns: one for the population (experimental arms and control)
#'                   and one or more columns for analyses (e.g., interim and final analyses).The analysis columns must be numerical values
#'                   representing the number of events observed during the interim and/or final analyses.
#' @param hypothesis A list containing hypotheses specifying comparisons between experimental arms and the control group,
#'                   as well as comparisons among experimental arms.
#'
#' @return A dataframe with columns:
#'   - `one_hypothesis`: The index of the first selected hypothesis from the provided list.
#'   - `another_hypothesis`: The index of the second selected hypothesis from the provided list.
#'   - `analysis`: The index indicating which analysis is being performed (e.g., interim or final).
#'   - `common_events`: The calculated number of common events associated with the selected hypotheses.
#'
#' @examples
#' input_data <- data.frame(
#'   Population = c("Experimental 1", "Experimental 2", "Experimental 3", "Control"),
#'   IA = c(70, 75, 80, 85), # Interim Analysis values indicating the number of events observed in each group
#'   FA = c(135, 150, 165, 170)
#' )
#'
#' hypothesis <- list(
#'   H1 = "Experimental 1 vs. Control",
#'   H2 = "Experimental 2 vs. Control",
#'   H3 = "Experimental 1 vs. Experimental 2"
#' )
#'
#' generate_event_table_cc(input_data, hypothesis)
#'
generate_event_table_cc <- function(input_data, hypothesis) {
  result_df <- tibble(
    one_hypothesis = integer(),
    another_hypothesis = integer(),
    analysis = integer(),
    common_events = integer()
  )

  # Iterate through the input data to calculate the events
  for (i in 1:length(hypothesis)) { # number of hypothesis
    for (j in i:length(hypothesis)) {
      for (k in 1:(ncol(input_data) - 1)) { # Iterate through the analyses
        if (i != j) {
          hyp_i <- unlist(strsplit(hypothesis[[i]], " vs. "))
          hyp_j <- unlist(strsplit(hypothesis[[j]], " vs. "))
          common_factor <- intersect(hyp_i, hyp_j)
          event <- input_data[input_data$Population == common_factor, k + 1]
        } else {
          event <- input_data[i, k + 1] + input_data[input_data$Population == "Control", k + 1]
        }

        result_df <- rbind(result_df, tibble(
          one_hypothesis = i,
          another_hypothesis = j,
          analysis = k,
          common_events = event
        ))
        result_df <- result_df[order(result_df$analysis), ]
      }
    }
  }
  return(result_df)
}
