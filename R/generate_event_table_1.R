#' Generate table of event counts from user input, including two conditions: common control and overlap population
#' 
#'
#' @param input_data user input of population at IA and FA 
#' @param hypothesis Illustrate groups want to compare
#' @param type either common control or overlap population
#'
#' @return dataframe with 4 columns:
#'         - one_hypothsis: hypothesis number one
#'         - another_hypothesis: another hypothesis to compare
#'         - analysis: IA or FA
#'         - common_events: commen events number under one hypothesis and another hypothesis
#'         
#' @export
#'
#' @examples 
#' # ----------------------- Example of common control
#'input_data <- data.frame(
#'  Population = c("Experimental 1", "Experimental 2", "Experimental 3", "Control"),
#'  IA = c(70, 75, 80, 85),
#'  FA = c(135, 150, 165, 170)
#')
#'
#'hypothesis <- list(H1 = "Experimental 1 vs. Control",
#'                   H2 = "Experimental 2 vs. Control",
#'                   H3 = "Experimental 1 vs. Experimental 2")
#'
#'# Generate event table using the updated function
#'result_table <- generate_event_table(input_data, hypothesis, type = "common_control")
#'
#'# Sort the result table by analysis
#'sorted_data <- result_table[order(result_table$analysis), ]
#'print(sorted_data)
#'
#'# ------------------------ Example of overall population
#'input_data <- data.frame(
#'  Population = c("Population 1", "Population 2", "Population 1 ∩2", "Overall population"),
#'  IA = c(100, 110, 80, 225),
#'  FA = c(200, 220, 160, 450)
#')
#'
#'hypothesis <- list(H1 = "Efficacy in Population 1", 
#'                   H2 = "Efficacy in Population 2",
#'                   H3 = "Efficacy in Overall population")
#'
#'# Generate event table using the updated function
#'result_table <- generate_event_table(input_data, hypothesis, type = "overlap_population")
#'
#'# Sort the result table by analysis
#'sorted_data <- result_table[order(result_table$analysis), ]
#'print(sorted_data)



generate_event_table <- function(input_data, hypothesis, type = c("common_control", "overlap_population")) {
  type <- match.arg(type)
  
  result_df <- tibble(
    one_hypothesis = integer(),
    another_hypothesis = integer(),
    analysis = integer(),
    common_events = integer()
  )
  
  if (type == "common_control") {
    result_df <- generate_event_table_cc(input_data, hypothesis) #see generate_event_cc_addhypo.R
  } else if (type == "overlap_population") {
    result_df <- generate_event_table_ol(input_data, hypothesis) #see generate_event_ol_addhypo.R
  }
  result_df <- result_df[!duplicated(result_df), ]
  result_df <- result_df[order(result_df$one_hypothesis, result_df$another_hypothesis, result_df$analysis), ]
  return(result_df)
}


