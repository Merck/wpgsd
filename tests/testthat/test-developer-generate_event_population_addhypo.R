test_that("Generate event table ol returns the expected sorted data", {
  expected_sorted_data <- tibble(
    one_hypothesis = as.integer(c(1, 1, 1, 2, 2, 3, 1, 1, 1, 2, 2, 3)),
    another_hypothesis = as.integer(c(1, 2, 3, 2, 3, 3, 1, 2, 3, 2, 3, 3)),
    analysis = as.integer(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)),
    common_events = c(100, 80, 100, 110, 110, 225, 200, 160, 200, 220, 220, 450)
  )

  input_data <- data.frame(
    Population = c("Population 1", "Population 2", "Population 1 $\u2229$ 2", "Overall population"),
    IA = c(100, 110, 80, 225),
    FA = c(200, 220, 160, 450)
  )

  hypothesis <- list(
    H1 = "Efficacy in Population 1",
    H2 = "Efficacy in Population 2",
    H3 = "Efficacy in Overall population"
  )


  result_table <- generate_event_table_ol(input_data, hypothesis)
  sorted_data <- result_table[order(result_table$analysis), ]

  expect_identical(sorted_data, expected_sorted_data)
})
