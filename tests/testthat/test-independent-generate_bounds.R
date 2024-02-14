test_that("BH bounds replicate tables A3 and A4", {
  # Example 1 BH weighting results in Table A3 and A4
  set.seed(1234)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Ex1 BH ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Transition matrix in Figure A1
  m <- matrix(c(
    0, 3 / 7, 4 / 7,
    3 / 7, 0, 4 / 7,
    1 / 2, 1 / 2, 0
  ), nrow = 3, byrow = TRUE)
  # Initial weights
  w <- c(0.3, 0.3, 0.4)

  # Event count of intersection of paired hypotheses - Table 1
  event <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, 100,
    2, 2, 1, 110,
    3, 3, 1, 225,
    1, 2, 1, 80,
    1, 3, 1, 100,
    2, 3, 1, 110,
    1, 1, 2, 200,
    2, 2, 2, 220,
    3, 3, 2, 450,
    1, 2, 2, 160,
    1, 3, 2, 200,
    2, 3, 2, 220
  )
  event

  # Generate correlation from events
  corr <- generate_corr(event)
  corr # correlation matrix in Table 3

  # WPGSD bounds, spending method 3b
  bound_WPGSD <- generate_bounds(
    type = 2, k = 2, w = w, m = m, corr = corr, alpha = 0.025,
    sf = gsDesign::sfHSD,
    sfparm = -4,
    t = c(min(100 / 200, 110 / 220, 225 / 450), 1)
  )

  # Bonferroni bounds
  bound_Bonf <- generate_bounds(
    type = 0, k = 2, w = w, m = m, corr = corr, alpha = 0.025,
    sf = list(gsDesign::sfHSD, gsDesign::sfHSD, gsDesign::sfHSD),
    sfparm = list(-4, -4, -4),
    t = list(c(0.5, 1), c(0.5, 1), c(0.5, 1))
  )

  # Combine and back-calculate xi
  bounds <- dplyr::left_join(
    bound_Bonf,
    bound_WPGSD,
    by = c("Hypotheses", "Analysis"),
    suffix = c(".B", ".W")
  )
  bounds <- bounds %>%
    dplyr::rowwise() %>%
    dplyr::mutate(xi = sum(H1.W, H2.W, H3.W, na.rm = TRUE) /
      sum(H1.B, H2.B, H3.B, na.rm = TRUE))
  # Reorder for output
  bounds$order <- rep(c(5, 2, 1, 3, 6, 4, 7), 2)
  bounds <- bounds %>% dplyr::arrange(Analysis, order)

  # Z-statistics boundary, Table A4
  zbounds <- bounds %>%
    dplyr::mutate(
      zH1.B = -qnorm(H1.B),
      zH2.B = -qnorm(H2.B),
      zH3.B = -qnorm(H3.B),
      zH1.W = -qnorm(H1.W),
      zH2.W = -qnorm(H2.W),
      zH3.W = -qnorm(H3.W)
    ) %>%
    select(Analysis, Hypotheses, zH1.B, zH2.B, zH3.B, zH1.W, zH2.W, zH3.W)


  # Table A3
  # Test H1
  # From publication Weighted Bonferroni
  A3_result1 <- c(
    0.0009, 0.0015, 0.0013, NA,
    0.003, NA, NA, 0.007, 0.0118,
    0.0101, NA, 0.0238, NA, NA
  )

  A3_result1_test <- round(bounds$H1.B, 4)

  expect_equal(A3_result1_test, A3_result1)

  # WPGSD

  wA3_result1 <- c(
    0.0011, 0.0017, 0.0014, NA,
    0.003, NA, NA, 0.0092, 0.0144,
    0.0116, NA, 0.0238, NA, NA
  )

  wA3_result1_test <- round(bounds$H1.W, 4)

  expect_equal(wA3_result1_test, wA3_result1)


  # Table A4
  # Test H1
  # From publication Weighted Bonferroni
  A4_result1 <- c(
    3.12, 2.97, 3.02, NA, 2.75, NA, NA,
    2.46, 2.26, 2.32, NA, 1.98, NA, NA
  )

  A4_result1_test <- round(zbounds$zH1.B, 2)

  expect_equal(A4_result1_test, A4_result1)

  # WPGSD
  wA4_result1 <- c(
    3.08, 2.93, 2.99, NA, 2.75, NA, NA,
    2.36, 2.19, 2.27, NA, 1.98, NA, NA
  )

  wA4_result1_test <- round(zbounds$zH1.W, 2)

  expect_equal(wA4_result1_test, wA4_result1)


  ########################
  # Next, reproduce H2?
  ########################

  A3_result2 <- c(
    0.0009, 0.0015, NA, 0.0013, NA,
    0.003, NA, 0.007, 0.0118, NA, 0.0101, NA, 0.0238, NA
  )

  A3_result2_test <- round(bounds$H2.B, 4)

  expect_equal(A3_result2_test, A3_result2)

  # WPGSD

  wA3_result2 <- c(
    0.0011, 0.0017, NA, 0.0014, NA,
    0.003, NA, 0.0092, 0.0144,
    NA, 0.0118, NA, 0.0238, NA
  )

  wA3_result2_test <- round(bounds$H2.W, 4)

  expect_equal(wA3_result2_test, wA3_result2)

  # A4

  A4_result2 <- c(
    3.12, 2.97, NA, 3.02, NA, 2.75, NA, 2.46, 2.26, NA, 2.32,
    NA, 1.98, NA
  )

  A4_result2_test <- round(zbounds$zH2.B, 2)

  expect_equal(A4_result2_test, A4_result2)

  # WPGSD
  wA4_result2 <- c(
    3.08, 2.93, NA, 2.99, NA, 2.75, NA, 2.36, 2.19,
    NA, 2.26, NA, 1.98, NA
  )

  wA4_result2_test <- round(zbounds$zH2.W, 2)

  expect_equal(wA4_result2_test, wA4_result2)


  ########################
  # Next, reproduce H3?
  ########################

  A3_result3 <- c(
    0.0012, NA, 0.0017, 0.0017, NA, NA, 0.003, 0.0094, NA,
    0.0135, 0.0135, NA, NA, 0.0238
  )

  A3_result3_test <- round(bounds$H3.B, 4)

  expect_equal(A3_result3_test, A3_result3)

  # WPGSD

  wA3_result3 <- c(
    0.0014, NA, 0.0018, 0.0019, NA, NA, 0.003, 0.0123, NA,
    0.0155, 0.0158, NA, NA, 0.0238
  )

  wA3_result3_test <- round(bounds$H3.W, 4)

  expect_equal(wA3_result3_test, wA3_result3)

  # A4

  A4_result3 <- c(3.04, NA, 2.93, 2.93, NA, NA, 2.75, 2.35, NA, 2.21, 2.21, NA, NA, 1.98)

  A4_result3_test <- round(zbounds$zH3.B, 2)

  expect_equal(A4_result3_test, A4_result3)

  # WPGSD
  wA4_result3 <- c(2.99, NA, 2.9, 2.9, NA, NA, 2.75, 2.25, NA, 2.16, 2.15, NA, NA, 1.98)

  wA4_result3_test <- round(zbounds$zH3.W, 2)

  expect_equal(wA4_result3_test, wA4_result3)
})


test_that("BH bounds replicate tables A6 and A7", {
  # From wpgsd github:
  # Example 2 BH weighting results in Table A6 and A7
  set.seed(1234)

  # Transition matrix in Figure A2
  m <- matrix(c(
    0, 0.5, 0.5,
    0.5, 0, 0.5,
    0.5, 0.5, 0
  ), nrow = 3, byrow = TRUE)
  # Initial weights
  w <- c(1 / 3, 1 / 3, 1 / 3)

  # Event count of intersection of paired hypotheses - Table 2
  event <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, 155,
    2, 2, 1, 160,
    3, 3, 1, 165,
    1, 2, 1, 85,
    1, 3, 1, 85,
    2, 3, 1, 85,
    1, 1, 2, 305,
    2, 2, 2, 320,
    3, 3, 2, 335,
    1, 2, 2, 170,
    1, 3, 2, 170,
    2, 3, 2, 170
  )
  event

  # Generate correlation from events
  corr <- generate_corr(event)
  corr # correlation matrix in Table 4

  # WPGSD bounds, spending method 3c
  bound_WPGSD <- generate_bounds(
    type = 3, k = 2, w = w, m = m, corr = corr, alpha = 0.025,
    sf = list(gsDesign::sfLDOF, gsDesign::sfLDOF, gsDesign::sfLDOF),
    sfparm = list(0, 0, 0),
    t = list(c(155 / 305, 1), c(160 / 320, 1), c(165 / 335, 1))
  )

  # Bonferroni bounds
  bound_Bonf <- generate_bounds(
    type = 0, k = 2, w = w, m = m, corr = corr, alpha = 0.025,
    sf = list(gsDesign::sfLDOF, gsDesign::sfLDOF, gsDesign::sfLDOF),
    sfparm = list(0, 0, 0),
    t = list(c(155 / 305, 1), c(160 / 320, 1), c(165 / 335, 1))
  )

  bounds <- dplyr::left_join(
    bound_Bonf,
    bound_WPGSD,
    by = c("Hypotheses", "Analysis"),
    suffix = c(".B", ".W")
  )

  # Reorder for output
  bounds$order <- rep(c(5, 2, 1, 3, 6, 4, 7), 2)
  bounds <- bounds %>% arrange(Analysis, order)

  # Table A6
  bounds

  bounds <- bounds %>% arrange(Analysis, order)

  # Z-statistics boundary, Table A7
  zbounds <- bounds %>%
    mutate(
      zH1.B = -qnorm(H1.B),
      zH2.B = -qnorm(H2.B),
      zH3.B = -qnorm(H3.B),
      zH1.W = -qnorm(H1.W),
      zH2.W = -qnorm(H2.W),
      zH3.W = -qnorm(H3.W)
    ) %>%
    select(Analysis, Hypotheses, zH1.B, zH2.B, zH3.B, zH1.W, zH2.W, zH3.W)


  # Table A6
  # Test H1
  # From publication Weighted Bonferroni
  A6_result1 <- c(
    0.00021488,
    0.000458864,
    0.000458864,
    NA,
    0.001665671,
    NA,
    NA,
    0.008259394,
    0.0123448,
    0.0123448,
    NA,
    0.024455472,
    NA,
    NA
  )

  A6_result1_test <- round(bounds$H1.B, 4)

  expect_equal(A6_result1_test, round(A6_result1, 4))

  # H2

  A6_result2 <- c(
    0.000190676,
    0.000411979,
    NA,
    0.000411979,
    NA,
    0.001525323,
    NA,
    0.0082675,
    0.012360191,
    NA,
    0.012360191,
    NA,
    0.024499778,
    NA
  )

  A6_result2_test <- round(bounds$H2.B, 4)

  expect_equal(A6_result2_test, round(A6_result2, 4))

  # H3

  A6_result3 <- c(
    0.000170445,
    NA,
    0.000372343,
    0.000372343,
    NA,
    NA,
    0.001404398,
    0.008274305,
    NA,
    0.012373258,
    0.012373258,
    NA,
    NA,
    0.024538108
  )

  A6_result3_test <- round(bounds$H3.B, 4)

  expect_equal(A6_result3_test, round(A6_result3, 4))

  ########
  # WPGSD
  ########

  wA6_result1 <- c(
    0.000224701,
    0.000471076,
    0.000470177,
    NA,
    0.001665671,
    NA,
    NA,
    0.009491524,
    0.013508544,
    0.013452942,
    NA,
    0.024455472,
    NA,
    NA
  )

  wA6_result1_test <- round(bounds$H1.W, 4)

  expect_equal(wA6_result1_test, round(wA6_result1, 4))



  wA6_result2 <- c(
    0.00019939,
    0.000422943,
    NA,
    0.000421453,
    NA,
    0.001525323,
    NA,
    0.00950084,
    0.013525386,
    NA,
    0.01341849,
    NA,
    0.024499778,
    NA
  )

  wA6_result2_test <- round(bounds$H2.W, 4)

  expect_equal(wA6_result2_test, round(wA6_result2, 4))


  wA6_result3 <- c(
    0.000178234,
    NA,
    0.000381523,
    0.000380905,
    NA,
    NA,
    0.001404398,
    0.00950866,
    NA,
    0.013483955,
    0.013432676,
    NA,
    NA,
    0.024538108
  )

  wA6_result3_test <- round(bounds$H3.W, 4)

  expect_equal(wA6_result3_test, round(wA6_result3, 4))


  # Z-statistics


  # Table A7
  A7_result1 <- c(
    3.521099809,
    3.314604451,
    3.314604451,
    NA,
    2.93538486,
    NA,
    NA,
    2.397246926,
    2.246225001,
    2.246225001,
    NA,
    1.969367163,
    NA,
    NA
  )

  A7_result1_test <- round(zbounds$zH1.B, 4)

  expect_equal(A7_result1_test, round(A7_result1, 4))


  A7_result2 <- c(
    3.552662921,
    3.34461863,
    NA,
    3.34461863,
    NA,
    2.962588043,
    NA,
    2.396887486,
    2.245744429,
    NA,
    2.245744429,
    NA,
    1.968595527,
    NA
  )

  A7_result2_test <- round(zbounds$zH2.B, 4)

  expect_equal(A7_result2_test, round(A7_result2, 4))



  A7_result3 <- c(
    3.582064348,
    NA,
    3.372575697,
    3.372575697,
    NA,
    NA,
    2.987923795,
    2.396585993,
    NA,
    2.24533684,
    2.24533684,
    NA,
    NA,
    1.967928919
  )

  A7_result3_test <- round(zbounds$zH3.B, 4)

  expect_equal(A7_result3_test, round(A7_result3, 4))





  # Table A7
  wA7_result1 <- c(
    3.509232997,
    3.307254785,
    3.307789645,
    NA,
    2.93538486,
    NA,
    NA,
    2.345863682,
    2.21127083,
    2.212880514,
    NA,
    1.969367163,
    NA,
    NA
  )

  wA7_result1_test <- round(zbounds$zH1.W, 4)

  expect_equal(wA7_result1_test, round(wA7_result1, 4))


  wA7_result2 <- c(
    3.540889382,
    3.337326516,
    NA,
    3.338307071,
    NA,
    2.962588043,
    NA,
    2.345498009,
    2.210784367,
    NA,
    2.213880806,
    NA,
    1.968595527,
    NA
  )


  wA7_result2_test <- round(zbounds$zH2.W, 4)

  expect_equal(wA7_result2_test, round(wA7_result2, 4))



  wA7_result3 <- c(
    3.570376445,
    NA,
    3.365863239,
    3.366309865,
    NA,
    NA,
    2.987923795,
    2.345191286,
    NA,
    2.21198197,
    2.213468666,
    NA,
    NA,
    1.967928919
  )

  wA7_result3_test <- round(zbounds$zH3.W, 4)

  expect_equal(wA7_result3_test, round(wA7_result3, 4))
})
