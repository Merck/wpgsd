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

test_that("2 hypotheses 3 analysis correlation as expected", {
  event <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, 147,
    2, 2, 1, 167,
    1, 2, 1, 88,
    1, 1, 2, 278,
    2, 2, 2, 289,
    1, 2, 2, 158,
    1, 1, 3, 342,
    2, 2, 3, 350,
    1, 2, 3, 192
  )

  corr <- generate_corr(event)
  n_hypotheses <- 2
  n_analyses <- 3
  corr_test <- diag(1, n_hypotheses * n_analyses)

  for (k in 1:n_analyses) {
    for (l in k:n_analyses) {
      for (i in 1:n_hypotheses) {
        countkii <- as.numeric((event %>% dplyr::filter(H1 == i & H2 == i & Analysis == k))$Event)
        for (j in i:n_hypotheses) {
          countkjj <- as.numeric((event %>% dplyr::filter(H1 == j & H2 == j & Analysis == k))$Event)
          countljj <- as.numeric((event %>% dplyr::filter(H1 == j & H2 == j & Analysis == l))$Event)
          countlii <- as.numeric((event %>% dplyr::filter(H1 == i & H2 == i & Analysis == l))$Event)
          countkij <- as.numeric((event %>% dplyr::filter(H1 == i & H2 == j & Analysis == k))$Event)
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
