test_that("Generate event table returns the expected sorted data", {
  expected_data <- tibble(
    one_hypothesis = as.integer(c(1, 1, 1, 2, 2, 3, 1, 1, 1, 2, 2, 3)),
    another_hypothesis = as.integer(c(1, 2, 3, 2, 3, 3, 1, 2, 3, 2, 3, 3)),
    analysis = as.integer(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)),
    common_events = c(155, 85, 70, 160, 75, 165, 305, 170, 135, 320, 150, 335)
  )

  event_data <- data.frame(
    Population = c("Experimental 1", "Experimental 2", "Experimental 3", "Control"),
    IA = c(70, 75, 80, 85),
    FA = c(135, 150, 165, 170)
  )

  hypothesis <- list(
    H1 = "Experimental 1 vs. Control",
    H2 = "Experimental 2 vs. Control",
    H3 = "Experimental 1 vs. Experimental 2"
  )

  result_table <- generate_event_table_cc(event_data, hypothesis)


  expect_identical(result_table, expected_data)
})
