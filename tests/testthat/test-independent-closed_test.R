p_obs <- bind_rows(tibble(Analysis = 1, H1 = 0.001, H2 = 0.001),
                   tibble(Analysis = 2, H1 = 0.001, H2 = 0.001))

bound <- tribble(
  ~Analysis, ~ Hypotheses, ~H1, ~H2,
  1, 'H1', 0.02, NA,
  1, 'H1, H2', 0.0001, 0.00001,
  1, 'H2', NA, 0.003,
  2, 'H1', 0.02, NA,
  2, 'H1, H2', 0.02, 0.00001,
  2, 'H2', NA, 0.003
) 

closed_test <- closed_test (bound, p_obs)
closed_test_a1<- closed_test %>% filter(Analysis=='Analysis 1')
closed_test_a2<- closed_test %>% filter(Analysis=='Analysis 2')


test_that("closed_test reject hypothesis as expected", {
  expect_equal(closed_test_a1$H1, 'Fail')
  expect_equal(closed_test_a1$H2, 'Fail')
  expect_equal(closed_test_a2$H1, 'Success')
  expect_equal(closed_test_a2$H2, 'Success')
})

