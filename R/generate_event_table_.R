#' This function creates a table summarizing event counts based on specified hypotheses and user input data.
#' It can handle two types of analysis: one comparing experimental groups to a common control and another analyzing the overlap of populations.
#'
#' @param event` dataframe should have the following structure:
#'   - `Population`: A character vector indicating the population groups. For example, "Population 1", "Population 2", "Overall population" in overlap population situation; or experimental arms and control in common control situation.
#'   - `IA`: Numeric vector indicating the number of events observed in each group during interim analysis.
#'   - `FA`: Numeric vector indicating the number of events observed in each group during final analysis.
#'   The dataframe must contain at least these columns and can include additional analysis columns as needed.
#' @param hypothesis A list containing hypotheses that specify the comparisons to be made between the groups:
#'                   - For example:
#'                     - "Experimental 1 vs. Control"
#'                     - "Efficacy in Population 1"
#'
#' @param type A character string specifying the type of analysis to conduct. It should be one of the following:
#'             - `"common_control"`: Analyze the event counts comparing experimental groups to common control.
#'             - `"overlap_population"`: Analyze the event counts to assess overlap in populations.
#'
#' @return A dataframe with four columns:
#'         - `one_hypothesis`: The index of the first selected hypothesis from the provided list.
#'         - `another_hypothesis`: The index of the second selected hypothesis from the provided list.
#'         - `analysis`: The index indicating which analysis is being performed (e.g., interim or final).
#'         - `common_events`: The calculated number of common events associated with the selected hypotheses.
#'
#' @export
#'
#' @examples
#' # ----------------------- Example of common control
#' event <- data.frame(
#'   Population = c("Experimental 1", "Experimental 2", "Experimental 3", "Control"),
#'   IA = c(70, 75, 80, 85), # Interim analysis values indicating the number of events observed in each experimental group.
#'   FA = c(135, 150, 165, 170) # Final analysis values indicating the cumulative number of events observed in each group.
#' )
#'
#' hypothesis <- list(
#'   H1 = "Experimental 1 vs. Control", # Hypothesis comparing Experimental 1 with Control.
#'   H2 = "Experimental 2 vs. Control", # Hypothesis comparing Experimental 2 with Control.
#'   H3 = "Experimental 1 vs. Experimental 2" # Hypothesis comparing Experimental 1 and Experimental 2.
#' )
#'
#' generate_event_table_(event, hypothesis, type = "common_control")
#'
#' # ------------------------ Example of overall population
#' event <- data.frame(
#'   Population = c("Population 1", "Population 2", "Population 1 Intersection 2", "Overall population"),
#'   IA = c(100, 110, 80, 225), # Interim analysis values for the overall population.
#'   FA = c(200, 220, 160, 450) # Final analysis values for the overall population.
#' )
#'
#' hypothesis <- list(
#'   H1 = "Efficacy in Population 1", # Hypothesis assessing efficacy in Population 1.
#'   H2 = "Efficacy in Population 2", # Hypothesis assessing efficacy in Population 2.
#'   H3 = "Efficacy in Overall population" # Hypothesis assessing efficacy in the overall population.
#' )
#'
#' generate_event_table_(event, hypothesis, type = "overlap_population")
#'
generate_event_table_ <- function(event, hypothesis, type = c("common_control", "overlap_population")) {
  type <- match.arg(type)
  
  result_df <- tibble(
    one_hypothesis = integer(),
    another_hypothesis = integer(),
    analysis = integer(),
    common_events = integer()
  )
  
  if (type == "common_control") {
    result_df <- generate_event_table_cc(event, hypothesis) # see generate_event_cc.R
  } else if (type == "overlap_population") {
    result_df <- generate_event_table_ol(event, hypothesis) # see generate_event_ol.R
  }
  return(result_df)
}