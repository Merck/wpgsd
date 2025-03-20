#' This function generates a table of events for given experimental arms and a control group based on specified hypotheses.
#'
#' @param event A dataframe containing the following columns:
#'   - `Population`: A character vector listing the population groups (e.g., experimental arms and control).
#'   - `IA`: A numeric vector indicating the number of events observed in each group during interim analysis.
#'   - `FA`: A numeric vector indicating the number of events observed in each group during final analysis.
#'   The dataframe must contain at least these columns and can include additional analysis columns as needed.
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
#' #------------------------Example of IA and FA
#' event <- data.frame(
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
#' generate_event_table_cc(event, hypothesis)
#'
#' #----------------------Example of two IAs and FA
#' event <- data.frame(
#'   Population = c("Experimental 1", "Experimental 2", "Experimental 3", "Control"),
#'   IA1 = c(70, 75, 80, 85), # First Interim Analysis values indicating the number of events observed in each group
#'   IA2 = c(90, 95, 100, 105), # Second Interim Analysis values indicating the number of events observed in each group
#'   FA = c(135, 150, 165, 170)
#' )
#'
#' hypothesis <- list(
#'   H1 = "Experimental 1 vs. Control",
#'   H2 = "Experimental 2 vs. Control",
#'   H3 = "Experimental 1 vs. Experimental 2"
#' )
#'
#' generate_event_table_cc(event, hypothesis)
generate_event_table_cc <- function(event, hypothesis) {
  result_df <- tibble(
    one_hypothesis = integer(),
    another_hypothesis = integer(),
    analysis = integer(),
    common_events = integer()
  )
  
  # Iterate through the input data to calculate the events
  for (i in 1:length(hypothesis)) { # number of hypothesis
    for (j in i:length(hypothesis)) {
      for (k in 1:(ncol(event) - 1)) { # Iterate through the analyses
        if (i != j) {
          hyp_i <- unlist(strsplit(hypothesis[[i]], " vs. "))
          hyp_j <- unlist(strsplit(hypothesis[[j]], " vs. "))
          common_factor <- intersect(hyp_i, hyp_j)
          eventn <- event[event$Population == common_factor, k + 1]
        } else {
          eventn <- event[i, k + 1] + event[event$Population == "Control", k + 1]
        }
        
        result_df <- rbind(result_df, tibble(
          one_hypothesis = i,
          another_hypothesis = j,
          analysis = k,
          common_events = eventn
        ))
        result_df <- result_df[order(result_df$analysis), ]
      }
    }
  }
  return(result_df)
}