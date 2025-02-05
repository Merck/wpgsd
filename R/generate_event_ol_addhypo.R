#' generate event table in overall population
#'
#' @param input_data input data in overall population situation
#' @param hypothesis comparison group
#'
#' @return
#' @export
#'
#' @examples
#' input_data <- data.frame(
#'   Population = c("Population 1", "Population 2", "Population 1 ∩ 2", "Overall population"),
#'   IA = c(100, 110, 80, 225),
#'   FA = c(200, 220, 160, 450)
#' )
#'
#' hypothesis <- list(
#'   H1 = "Efficacy in Population 1",
#'   H2 = "Efficacy in Population 2",
#'   H3 = "Efficacy in Overall population"
#' )
#'
#' result_table <- generate_event_table_ol(input_data, hypothesis)
#' sorted_data <- result_table[order(result_table$analysis), ]
#' print(sorted_data)
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
          event <- input_data[input_data$Population == "Population 1 ∩ 2", k + 1]
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

  result_df <- result_df[!duplicated(result_df), ]
  result_df <- result_df[order(result_df$one_hypothesis, result_df$another_hypothesis, result_df$analysis), ]

  return(result_df)
}
