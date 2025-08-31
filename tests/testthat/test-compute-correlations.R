test_that("check_event_data validates basic structure", {
  library(tibble)
  
  # Test non-data.frame input
  expect_error(check_event_data(c(1, 2, 3)), "'event' must be a data frame")
  
  # Test missing columns
  incomplete_df <- data.frame(H1 = 1, H2 = 1)
  expect_error(check_event_data(incomplete_df), "Missing required columns: Analysis, Event")
})

test_that("check_event_data validates H1 <= H2 constraint", {
  library(tibble)
  
  invalid_order <- tibble(
    H1 = c(2, 1),
    H2 = c(1, 1),
    Analysis = c(1, 1),
    Event = c(100, 100)
  )
  expect_error(check_event_data(invalid_order), "H1 must be <= H2 for all rows")
})

test_that("check_event_data validates uniqueness", {
  library(tibble)
  
  duplicate_combo <- tibble(
    H1 = c(1, 1, 1),
    H2 = c(1, 1, 1),
    Analysis = c(1, 1, 1),
    Event = c(100, 100, 100)
  )
  expect_error(check_event_data(duplicate_combo), "Combinations of H1, H2, Analysis must be unique")
})

test_that("check_event_data validates Event values", {
  library(tibble)
  
  # Test negative events
  negative_events <- tibble(
    H1 = c(1, 2),
    H2 = c(1, 2),
    Analysis = c(1, 1),
    Event = c(-1, 100)
  )
  expect_error(check_event_data(negative_events), "Event must be non-negative integers")
  
  # Test non-integer events
  non_integer_events <- tibble(
    H1 = c(1, 2),
    H2 = c(1, 2),
    Analysis = c(1, 1),
    Event = c(100.5, 100)
  )
  expect_error(check_event_data(non_integer_events), "Event must be non-negative integers")
})

test_that("check_event_data validates sequential values", {
  library(tibble)
  
  # Test non-sequential Analysis
  non_sequential_analysis <- tibble(
    H1 = c(1, 2, 1, 2),
    H2 = c(1, 2, 1, 2),
    Analysis = c(1, 1, 3, 3),  # Missing Analysis = 2
    Event = c(100, 100, 100, 100)
  )
  expect_error(check_event_data(non_sequential_analysis), 
               "Analysis values must be sequential positive integers starting from 1")
  
  # Test non-sequential H1
  non_sequential_h1 <- tibble(
    H1 = c(1, 3, 1, 3),  # Missing H1 = 2
    H2 = c(1, 3, 1, 3),
    Analysis = c(1, 1, 2, 2),
    Event = c(100, 100, 100, 100)
  )
  expect_error(check_event_data(non_sequential_h1), 
               "H1 values must be sequential positive integers starting from 1")
})

test_that("check_event_data validates diagonal requirements", {
  library(tibble)
  
  # Test missing diagonal entries - off-diagonal exists but corresponding diagonal is missing
  missing_diagonal <- tibble(
    H1 = c(1, 2, 1, 1, 1),  # Analysis 2 has off-diagonal H1=1, H2=2 but missing H1=2, H2=2
    H2 = c(1, 2, 2, 1, 2),  
    Analysis = c(1, 1, 1, 2, 2), 
    Event = c(100, 100, 80, 120, 85)  
  )
  expect_error(check_event_data(missing_diagonal), "Missing diagonal entry")
})

test_that("check_event_data passes with valid data", {
  library(tibble)
  
  valid_data <- tibble(
    H1 = c(1, 2, 1, 1, 2, 1),
    H2 = c(1, 2, 2, 1, 2, 2),
    Analysis = c(1, 1, 1, 2, 2, 2),
    Event = c(155, 160, 85, 305, 320, 170)
  )
  
  expect_true(check_event_data(valid_data))
})

test_that("compute_correlations returns data frame when requested", {
  library(tibble)
  
  valid_data <- tibble(
    H1 = c(1, 2, 1, 1, 2, 1),
    H2 = c(1, 2, 2, 1, 2, 2),
    Analysis = c(1, 1, 1, 2, 2, 2),
    Event = c(155, 160, 85, 305, 320, 170)
  )
  
  result_df <- compute_correlations(valid_data, return_matrix = FALSE)
  
  expect_true(is.data.frame(result_df))
  expect_true(all(c("H1", "H2", "Analysis1", "Analysis2", "Correlation") %in% names(result_df)))
  expect_true(nrow(result_df) > 0)  # Should have some correlations
  expect_true(all(result_df$Correlation >= -1 & result_df$Correlation <= 1))  # Valid correlations
})

test_that("compute_correlations returns symmetric matrix by default", {
  library(tibble)
  
  valid_data <- tibble(
    H1 = c(1, 2, 1, 1, 2, 1),
    H2 = c(1, 2, 2, 1, 2, 2),
    Analysis = c(1, 1, 1, 2, 2, 2),
    Event = c(155, 160, 85, 305, 320, 170)
  )
  
  corr_matrix <- compute_correlations(valid_data)
  
  expect_true(is.matrix(corr_matrix))
  expect_true(isSymmetric(corr_matrix, tol = 1e-10))
  expect_equal(nrow(corr_matrix), 4)  # 2 hypotheses * 2 analyses
  expect_equal(ncol(corr_matrix), 4)
  expect_equal(as.numeric(diag(corr_matrix)), rep(1, 4))
})

test_that("compute_correlations matrix has proper names", {
  library(tibble)
  
  valid_data <- tibble(
    H1 = c(1, 2, 1, 1, 2, 1),
    H2 = c(1, 2, 2, 1, 2, 2),
    Analysis = c(1, 1, 1, 2, 2, 2),
    Event = c(155, 160, 85, 305, 320, 170)
  )
  
  corr_matrix <- compute_correlations(valid_data)
  expected_names <- c("H1_A1", "H1_A2", "H2_A1", "H2_A2")  # Hypothesis-major order
  
  expect_equal(colnames(corr_matrix), expected_names)
  expect_equal(rownames(corr_matrix), expected_names)
})

test_that("gen_corr creates proper matrix from data frame", {
  # Create sample correlation data frame
  corr_df <- data.frame(
    H1 = c(1, 1, 2, 1, 2, 2),
    H2 = c(1, 2, 2, 1, 1, 2),
    Analysis1 = c(1, 1, 1, 2, 2, 2),
    Analysis2 = c(1, 1, 1, 2, 2, 2),
    Correlation = c(1, 0.5, 1, 1, 1, 1)
  )
  
  corr_matrix <- gen_corr(corr_df, M = 2, K = 2)
  
  expect_true(is.matrix(corr_matrix))
  expect_true(isSymmetric(corr_matrix))
  expect_equal(as.numeric(diag(corr_matrix)), rep(1, 4))
  expect_equal(nrow(corr_matrix), 4)
  expect_equal(ncol(corr_matrix), 4)
})

test_that("compute_correlations skips validation when check=FALSE", {
  library(tibble)
  
  # This would normally fail validation but should work with check=FALSE
  invalid_data <- tibble(
    H1 = c(2, 1),  # H1 > H2, which violates validation
    H2 = c(1, 1),
    Analysis = c(1, 1),
    Event = c(100, 100)
  )
  
  # Should throw error with check=TRUE
  expect_error(compute_correlations(invalid_data, check = TRUE))
  
  # Should work with check=FALSE (though may produce unexpected results)
  expect_no_error(compute_correlations(invalid_data, check = FALSE))
})

test_that("generate_corr_s7 works with new correlation functions", {
  library(tibble)
  
  valid_data <- tibble(
    H1 = c(1, 2, 1, 1, 2, 1),
    H2 = c(1, 2, 2, 1, 2, 2),
    Analysis = c(1, 1, 1, 2, 2, 2),
    Event = c(155, 160, 85, 305, 320, 170)
  )
  
  # Test with EventTable (now required input type)
  event_table <- new_event_table(valid_data)
  corr_s7 <- generate_corr_s7(event_table)
  expect_true(S7::S7_inherits(corr_s7, CorrelationMatrix))
  expect_true(isSymmetric(corr_s7@matrix, tol = 1e-10))
  
  # Test that matrix dimensions are correct
  expect_equal(nrow(corr_s7@matrix), 4)  # 2 hypotheses x 2 analyses
  expect_equal(ncol(corr_s7@matrix), 4)
  
  # Test that matrix has proper column ordering (Analysis then Hypothesis)
  expected_colnames <- c("H1_A1", "H2_A1", "H1_A2", "H2_A2")
  expect_equal(colnames(corr_s7@matrix), expected_colnames)
})

test_that("new correlation functions produce consistent results", {
  library(tibble)
  
  valid_data <- tibble(
    H1 = c(1, 2, 1, 1, 2, 1),
    H2 = c(1, 2, 2, 1, 2, 2),
    Analysis = c(1, 1, 1, 2, 2, 2),
    Event = c(155, 160, 85, 305, 320, 170)
  )
  
  # Get correlation matrix and data frame
  corr_matrix <- compute_correlations(valid_data, return_matrix = TRUE)
  corr_df <- compute_correlations(valid_data, return_matrix = FALSE)
  
  # Convert data frame back to matrix
  matrix_from_df <- gen_corr(corr_df, M = 2, K = 2)
  
  # Should be identical
  expect_equal(corr_matrix, matrix_from_df)
})
