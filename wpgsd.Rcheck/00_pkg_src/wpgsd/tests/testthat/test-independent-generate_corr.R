test_that("2 endpoints 2 analysis correlation as expected", {
  a1 <- 80
  b1 <- 100
  ab1 <- 60
  a2 <- 120
  b2 <- 150
  ab2 <- 80

  event <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, a1,
    2, 2, 1, b1,
    1, 2, 1, ab1,
    1, 1, 2, a2,
    2, 2, 2, b2,
    1, 2, 2, ab2
  )

  corr <- generate_corr(event)
  corr_test <- matrix(
    c(
      1, ab1 / sqrt(a1 * b1), a1 / sqrt(a1 * a2), ab1 / sqrt(a1 * b2),
      ab1 / sqrt(a1 * b1), 1, ab1 / sqrt(a2 * b1), b1 / sqrt(b1 * b2),
      a1 / sqrt(a1 * a2), ab1 / sqrt(a2 * b1), 1, ab2 / sqrt(a2 * b2),
      ab1 / sqrt(a1 * b2), b1 / sqrt(b1 * b2), ab2 / sqrt(a2 * b2), 1
    ),
    nrow = 4, byrow = TRUE
  )

  expect_equal(matrix(corr %>% as.numeric(), nrow = 4, byrow = TRUE), corr_test)
})
