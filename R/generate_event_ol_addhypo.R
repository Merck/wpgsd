#' generate_event_table_ol
#'
#' This function generates a table of events for specified populations based on the provided hypotheses.
#'
#' @param input_data A dataframe containing at least two columns: one for the population (e.g., "Population 1", "Population 2") 
#'                   and one or more columns indicating the number of events observed during analyses (e.g., interim and final analyses).
#'                   The first column must be named 'Population', and it should include total counts for each specified population:
#'                   - "Population 1"
#'                   - "Population 2"
#'                   - "Population 1 Intersection 2" (the intersection of events observed in both populations)
#'                   - "Overall population" (the total count of events observed across all populations).
#'                   
#' @param hypothesis A list of strings where each item represents a hypothesis regarding efficacy, formatted as follows:
#'                   - H1: "Efficacy in Population 1"
#'                   - H2: "Efficacy in Population 2"
#'                   - H3: "Efficacy in Overall population"
#'                   Each hypothesis is used for comparisons in the generated event table.
#'
#' @return A dataframe with the following columns:
#'   - `one_hypothesis`: The index of the first selected hypothesis from the provided list.
#'   - `another_hypothesis`: The index of the second selected hypothesis from the provided list.
#'   - `analysis`: The index indicating which analysis is being performed (e.g., interim or final).
#'   - `common_events`: The calculated number of common events associated with the selected hypotheses.
#'
#' @export
#'
#' @examples
#' input_data <- data.frame(
#'   Population = c("Population 1", "Population 2", "Population 1 Intersection 2", "Overall population"),
#'   IA = c(100, 110, 80, 225), # Interim Analysis values indicating the number of events observed in each group
#'   FA = c(200, 220, 160, 450)
#' )
#'
#' hypothesis <- list(
#'   H1 = "Efficacy in Population 1",
#'   H2 = "Efficacy in Population 2",
#'   H3 = "Efficacy in Overall population"
#' )
#'
#' generate_event_table_ol(input_data, hypothesis)

generate_event_table_ol <- function(input_data, hypothesis) {
  result_df <- tibble(
    one_hypothesis = integer(),
    another_hypothesis = integer(),
    analysis = integer(),
    common_events = integer()
  )

  for (i in 1:length(hypothesis)) {
    for (j in i:length(hypothesis)) {
      for (k in 1:(ncol(input_data) - 1)) {
        hyp_i <- unlist(strsplit(hypothesis[[i]], "Efficacy in "))[2]
        hyp_j <- unlist(strsplit(hypothesis[[j]], "Efficacy in "))[2]

        common_factor <- intersect(hyp_i, hyp_j)

        if (length(common_factor) > 0) {
          if ("Overall population" %in% c(hyp_i, hyp_j)) {
            event <- input_data[input_data$Population == "Overall population", k + 1]
          } else {
            event <- input_data[i, k + 1]
          }
        } else if ("Overall population" %in% c(hyp_i, hyp_j)) {
          event <- input_data[i, k + 1]
        } else {
          event <- input_data[input_data$Population == "Population 1 Intersection 2", k + 1]
        }

        result_df <- rbind(result_df, tibble(
          one_hypothesis = i,
          another_hypothesis = j,
          analysis = k,
          common_events = event
        ))
        result_df<- result_df[order(result_df$analysis), ]
      }
    }
  }

  return(result_df)
}

 input_data <- data.frame(
   Population = c("Population 1", "Population 2", "Population 1 Intersection 2", "Overall population"),
   IA = c(100, 110, 80, 225), # Interim Analysis values indicating the number of events observed in each group
   FA = c(200, 220, 160, 450)
 )

 hypothesis <- list(
   H1 = "Efficacy in Population 1",
   H2 = "Efficacy in Population 2",
   H3 = "Efficacy in Overall population"
 )

 generate_event_table_ol(input_data, hypothesis)