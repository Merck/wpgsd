test_that("2 endpoints 2 analysis correlation as expected", {
  res <- test_generate_corr_new()
  a1 <- res$a1
  b1 <- res$b1
  ab1 <- res$ab1
  a2 <- res$a2
  b2 <- res$b2
  ab2 <- res$ab2
  event <- res$event

  corr <- generate_corr_new(event)
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

# This is a 2 hypothesis, 3 analysis example
test_that("2 hypotheses 3 analysis correlation as expected", {
  event <- tibble::tribble(
    ~Analysis, ~H1, ~H2, ~Event,
    1, 1, 1, 147,
    1, 2, 2, 167,
    1, 1, 2, 88,
    2, 1, 1, 278,
    2, 2, 2, 289,
    2, 1, 2, 158,
    3, 1, 1, 342,
    3, 2, 2, 350,
    3, 1, 2, 192
  )
  corr <- generate_corr_new(event)
  n_hypotheses <- 2
  n_analyses <- 3
  corr_test <- diag(1, n_hypotheses * n_analyses)
  for (k in 1:n_analyses) {
    for (l in k:n_analyses) {
      for (i in 1:(n_hypotheses)) {
        countkii <- as.numeric((event %>% filter(H1 == i & H2 == i & Analysis == k))$Event)
        for (j in i:n_hypotheses) {
          countkjj <- as.numeric((event %>% filter(H1 == j & H2 == j & Analysis == k))$Event)
          countljj <- as.numeric((event %>% filter(H1 == j & H2 == j & Analysis == l))$Event)
          countlii <- as.numeric((event %>% filter(H1 == i & H2 == i & Analysis == l))$Event)
          countkij <- as.numeric((event %>% filter(H1 == i & H2 == j & Analysis == k))$Event)
          corr_test[(k - 1) * n_hypotheses + i, (l - 1) * n_hypotheses + j] <- countkij / sqrt(countkii * countljj)
          corr_test[(k - 1) * n_hypotheses + j, (l - 1) * n_hypotheses + i] <- countkij / sqrt(countkjj * countlii)
          corr_test[(l - 1) * n_hypotheses + i, (k - 1) * n_hypotheses + j] <- countkij / sqrt(countlii * countkjj)
          corr_test[(l - 1) * n_hypotheses + j, (k - 1) * n_hypotheses + i] <- countkij / sqrt(countljj * countkii)
        }
      }
    }
  }

  expect_equal(matrix(corr %>% as.numeric(), nrow = 6, byrow = TRUE), corr_test)
})
