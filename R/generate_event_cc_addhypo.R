#' generate event table in common control
#'
#' @param input_data input data in common control situation
#' @param hypothesis comparison group
#'
#' @return
#' @export
#'
#' @examples
#' input_data <- data.frame(
#'   Population = c("Experimental 1", "Experimental 2", "Experimental 3", "Control"),
#'   IA = c(70, 75, 80, 85),
#'   FA = c(135, 150, 165, 170)
#' )
#'
#' hypothesis <- list(
#'   H1 = "Experimental 1 vs. Control",
#'   H2 = "Experimental 2 vs. Control",
#'   H3 = "Experimental 1 vs. Experimental 2"
#' )
#'
#' result_table <- generate_event_table_cc(input_data, hypothesis)
#' sorted_data <- result_table[order(result_table$analysis), ]
#' print(sorted_data)
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
    for (j in i:length(hypothesis)) { #
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
      }
    }
  }

  # Remove duplicate rows
  result_df <- result_df[!duplicated(result_df), ]

  # Sort the output by H1, H2, and analysis
  result_df <- result_df[order(result_df$one_hypothesis, result_df$another_hypothesis, result_df$analysis), ]

  return(result_df)
}
