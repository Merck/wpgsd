test_that("CorrelationMatrix can be created with valid data", {
  # Create simple 2x2 correlation matrix
  corr_mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  corr_obj <- CorrelationMatrix(
    matrix = corr_mat,
    n_hypotheses = 1L,
    n_analyses = 2L,
    column_names = c("H1_A1", "H1_A2")
  )

  # Check that object was created successfully
  expect_true(S7::S7_inherits(corr_obj, CorrelationMatrix))
  expect_equal(corr_obj@n_hypotheses, 1L)
  expect_equal(corr_obj@n_analyses, 2L)
  expect_equal(nrow(corr_obj@matrix), 2)
  expect_equal(ncol(corr_obj@matrix), 2)
  expect_equal(corr_obj@column_names, c("H1_A1", "H1_A2"))
})

test_that("CorrelationMatrix validates matrix properties", {
  # Test non-square matrix
  invalid_mat1 <- matrix(c(1, 0.5, 0.5), nrow = 1)
  expect_error(
    CorrelationMatrix(matrix = invalid_mat1, n_hypotheses = 1L, n_analyses = 3L),
    "Matrix must be square"
  )

  # Test non-symmetric matrix
  invalid_mat2 <- matrix(c(1, 0.3, 0.5, 1), nrow = 2)
  expect_error(
    CorrelationMatrix(matrix = invalid_mat2, n_hypotheses = 1L, n_analyses = 2L),
    "Correlation matrix must be symmetric"
  )

  # Test diagonal not equal to 1
  invalid_mat3 <- matrix(c(0.8, 0.3, 0.3, 1), nrow = 2)
  expect_error(
    CorrelationMatrix(matrix = invalid_mat3, n_hypotheses = 1L, n_analyses = 2L),
    "Diagonal elements of correlation matrix must be 1"
  )

  # Test off-diagonal elements outside [-1, 1]
  invalid_mat4 <- matrix(c(1, 1.2, 1.2, 1), nrow = 2)
  expect_error(
    CorrelationMatrix(matrix = invalid_mat4, n_hypotheses = 1L, n_analyses = 2L),
    "Off-diagonal elements must be between -1 and 1"
  )

  # Test non-positive definite matrix
  invalid_mat5 <- matrix(c(1, 0.9, 0.9, 0.9, 1, -0.9, 0.9, -0.9, 1), nrow = 3) # This should be non-PD
  expect_error(
    CorrelationMatrix(matrix = invalid_mat5, n_hypotheses = 1L, n_analyses = 3L),
    "Correlation matrix must be positive semi-definite"
  )
})

test_that("CorrelationMatrix validates dimension consistency", {
  # Test inconsistent dimensions
  corr_mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  expect_error(
    CorrelationMatrix(matrix = corr_mat, n_hypotheses = 2L, n_analyses = 2L),
    "Matrix dimensions.*don't match n_hypotheses.*n_analyses"
  )

  # Test wrong column names length
  expect_error(
    CorrelationMatrix(
      matrix = corr_mat,
      n_hypotheses = 1L,
      n_analyses = 2L,
      column_names = c("H1_A1") # Too few names
    ),
    "Length of column_names.*must equal matrix dimensions"
  )
})

test_that("CorrelationMatrix auto-infers dimensions", {
  # Test auto-inference from column names
  corr_mat <- matrix(c(
    1, 0.5, 0.3, 0.2,
    0.5, 1, 0.4, 0.3,
    0.3, 0.4, 1, 0.5,
    0.2, 0.3, 0.5, 1
  ), nrow = 4)

  corr_obj <- CorrelationMatrix(
    matrix = corr_mat,
    column_names = c("H1_A1", "H2_A1", "H1_A2", "H2_A2")
  )

  expect_equal(corr_obj@n_hypotheses, 2L)
  expect_equal(corr_obj@n_analyses, 2L)
})

test_that("CorrelationMatrix generates column names automatically", {
  corr_mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  corr_obj <- CorrelationMatrix(
    matrix = corr_mat,
    n_hypotheses = 1L,
    n_analyses = 2L
  )

  expect_equal(corr_obj@column_names, c("H1_A1", "H1_A2"))
})

test_that("as_correlation_matrix converter works", {
  corr_mat <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
  corr_obj <- as_correlation_matrix(corr_mat, n_hypotheses = 1L, n_analyses = 2L)

  expect_true(S7::S7_inherits(corr_obj, CorrelationMatrix))
  expect_equal(corr_obj@matrix, corr_mat)
})

test_that("subset_correlation_matrix works correctly", {
  # Create a larger correlation matrix (2 hypotheses, 2 analyses = 4x4)
  corr_mat <- matrix(c(
    1, 0.5, 0.3, 0.2,
    0.5, 1, 0.4, 0.3,
    0.3, 0.4, 1, 0.5,
    0.2, 0.3, 0.5, 1
  ), nrow = 4)

  corr_obj <- CorrelationMatrix(
    matrix = corr_mat,
    n_hypotheses = 2L,
    n_analyses = 2L,
    column_names = c("H1_A1", "H2_A1", "H1_A2", "H2_A2")
  )

  # Subset by analysis 1 only
  subset_obj <- subset_correlation_matrix(corr_obj, analysis = 1)
  expect_equal(subset_obj@n_analyses, 1L)
  expect_equal(subset_obj@n_hypotheses, 2L)
  expect_equal(nrow(subset_obj@matrix), 2)

  # Subset by hypothesis 1 only
  subset_obj2 <- subset_correlation_matrix(corr_obj, hypotheses = 1)
  expect_equal(subset_obj2@n_hypotheses, 1L)
  expect_equal(subset_obj2@n_analyses, 2L)
  expect_equal(nrow(subset_obj2@matrix), 2)
})

test_that("generate_corr_s7 works with EventTable", {
  library(tibble)

  event_data <- tibble(
    H1 = c(1, 2, 1, 1, 2, 1),
    H2 = c(1, 2, 2, 1, 2, 2),
    Analysis = c(1, 1, 1, 2, 2, 2),
    Event = c(155, 160, 85, 305, 320, 170)
  )

  # Test with EventTable object
  event_table <- EventTable(data = event_data)
  corr_s7 <- generate_corr_s7(event_table)

  expect_true(S7::S7_inherits(corr_s7, CorrelationMatrix))
  expect_equal(corr_s7@n_hypotheses, 2L)
  expect_equal(corr_s7@n_analyses, 2L)
  expect_equal(length(corr_s7@column_names), 4)
  expect_true(isSymmetric(corr_s7@matrix, tol = 1e-10))

  # Test with EventTable object - generate_corr_s7 now requires EventTable input
  # Creating EventTable from the same data
  event_table_df <- EventTable(data = event_data)
  corr_s7_df <- generate_corr_s7(event_table_df)

  expect_true(S7::S7_inherits(corr_s7_df, CorrelationMatrix))
  expect_equal(corr_s7_df@matrix, corr_s7@matrix)
})

test_that("generate_corr_s7 produces mathematically valid correlations", {
  library(tibble)

  event_data <- tibble(
    H1 = c(1, 2, 1, 1, 2, 1),
    H2 = c(1, 2, 2, 1, 2, 2),
    Analysis = c(1, 1, 1, 2, 2, 2),
    Event = c(155, 160, 85, 305, 320, 170)
  )

  # Create EventTable for S7 function
  event_table <- EventTable(data = event_data)

  # Test new function produces valid correlation matrix
  s7_corr <- generate_corr_s7(event_table)

  # Should be symmetric
  expect_true(isSymmetric(s7_corr@matrix, tol = 1e-10))

  # Diagonal should be 1
  expect_equal(as.numeric(diag(s7_corr@matrix)), rep(1, 4))

  # Off-diagonal elements should be in [-1, 1]
  off_diag <- s7_corr@matrix[upper.tri(s7_corr@matrix) | lower.tri(s7_corr@matrix)]
  expect_true(all(off_diag >= -1 & off_diag <= 1))

  # Should be positive semi-definite
  eigenvals <- eigen(s7_corr@matrix, only.values = TRUE)$values
  expect_true(all(eigenvals >= -1e-10))
})

test_that("CorrelationMatrix print method works", {
  corr_mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  corr_obj <- CorrelationMatrix(
    matrix = corr_mat,
    n_hypotheses = 1L,
    n_analyses = 2L,
    column_names = c("H1_A1", "H1_A2")
  )

  # Test that print doesn't error
  expect_output(print(corr_obj), "CorrelationMatrix")
  expect_output(print(corr_obj), "n_hypotheses.*1")
  expect_output(print(corr_obj), "n_analyses.*2")
})
