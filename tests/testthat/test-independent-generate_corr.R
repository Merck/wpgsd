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

test_that("2 endpoints 3 analyses correlation is valid and correct", {
  # Event counts: H1 (subgroup), H2 (overall), intersection = H1 (nested)
  a <- c(50, 80, 100) # H1 events at analyses 1, 2, 3
  b <- c(80, 130, 170) # H2 events at analyses 1, 2, 3
  ab <- c(50, 80, 100) # Intersection events (= H1 for nested populations)

  event <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, a[1], 2, 2, 1, b[1], 1, 2, 1, ab[1],
    1, 1, 2, a[2], 2, 2, 2, b[2], 1, 2, 2, ab[2],
    1, 1, 3, a[3], 2, 2, 3, b[3], 1, 2, 3, ab[3]
  )

  corr <- generate_corr(event)

  # Matrix should be 6x6
  expect_equal(nrow(corr), 6)
  expect_equal(ncol(corr), 6)

  # All diagonal entries should be 1
  expect_equal(diag(corr), rep(1, 6))

  # All entries should be in [-1, 1] (with floating point tolerance)
  expect_true(all(corr >= -1 - 1e-10 & corr <= 1 + 1e-10))

  # Matrix should be positive definite
  expect_true(all(eigen(corr)$values > 0))

  # Check specific entries (use tolerance for named vector comparison):
  # corr(H1_A1, H1_A2) = a[1] / sqrt(a[1] * a[2])
  expect_equal(as.numeric(corr[1, 3]), a[1] / sqrt(a[1] * a[2]))
  # corr(H1_A1, H1_A3) = a[1] / sqrt(a[1] * a[3])
  expect_equal(as.numeric(corr[1, 5]), a[1] / sqrt(a[1] * a[3]))
  # corr(H1_A2, H1_A3) = a[2] / sqrt(a[2] * a[3])
  expect_equal(as.numeric(corr[3, 5]), a[2] / sqrt(a[2] * a[3]))
  # corr(H2_A1, H2_A3) = b[1] / sqrt(b[1] * b[3])
  expect_equal(as.numeric(corr[2, 6]), b[1] / sqrt(b[1] * b[3]))

  # Cross-hypothesis: corr(H1_A1, H2_A2) = ab[1] / sqrt(a[1] * b[2])
  expect_equal(as.numeric(corr[1, 4]), ab[1] / sqrt(a[1] * b[2]))
  # corr(H1_A2, H2_A3) = ab[2] / sqrt(a[2] * b[3])
  expect_equal(as.numeric(corr[3, 6]), ab[2] / sqrt(a[2] * b[3]))
})
