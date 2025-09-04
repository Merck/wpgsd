test_that("EventTable can be created with valid data", {
  # Create valid event data
  event_data <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, 155,
    2, 2, 1, 160,
    1, 2, 1, 85,
    1, 1, 2, 305,
    2, 2, 2, 320,
    1, 2, 2, 170
  )

  # Create EventTable
  event_table <- EventTable(data = event_data)

  # Check that object was created successfully
  expect_true(S7::S7_inherits(event_table, EventTable))
  expect_equal(event_table@n_hypotheses, 2L)
  expect_equal(event_table@n_analyses, 2L)
  expect_equal(nrow(event_table@data), 6)
})

test_that("EventTable validates required columns", {
  # Missing Event column
  invalid_data1 <- tibble::tibble(
    H1 = c(1, 2),
    H2 = c(1, 2),
    Analysis = c(1, 1)
  )

  expect_error(
    EventTable(data = invalid_data1),
    "EventTable requires columns: Event"
  )

  # Missing H1 column
  invalid_data2 <- tibble::tibble(
    H2 = c(1, 2),
    Analysis = c(1, 1),
    Event = c(100, 200)
  )

  expect_error(
    EventTable(data = invalid_data2),
    "EventTable requires columns: H1"
  )
})

test_that("EventTable validates data types and values", {
  # Non-numeric H1
  invalid_data1 <- tibble::tibble(
    H1 = c("A", "B"),
    H2 = c(1, 2),
    Analysis = c(1, 1),
    Event = c(100, 200)
  )

  expect_error(
    EventTable(data = invalid_data1),
    "@data\\$H1 and @data\\$H2 must be numeric"
  )

  # Negative hypothesis index
  invalid_data2 <- tibble::tibble(
    H1 = c(-1, 2),
    H2 = c(1, 2),
    Analysis = c(1, 1),
    Event = c(100, 200)
  )

  expect_error(
    EventTable(data = invalid_data2),
    "Hypothesis indices \\(H1, H2\\) must be positive integers"
  )

  # Negative event count
  invalid_data3 <- tibble::tibble(
    H1 = c(1, 2),
    H2 = c(1, 2),
    Analysis = c(1, 1),
    Event = c(-100, 200)
  )

  expect_error(
    EventTable(data = invalid_data3),
    "Event counts must be non-negative"
  )
})

test_that("EventTable basic functionality works", {
  event_data <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, 155,
    2, 2, 1, 160
  )

  event_table <- EventTable(data = event_data)

  # Test that object has correct properties
  expect_true(S7::S7_inherits(event_table, EventTable))
  expect_equal(event_table@n_hypotheses, 2L)
  expect_equal(event_table@n_analyses, 1L)
  expect_equal(nrow(event_table@data), 2)
})

# Print method test removed for now as we're using default S7 print
# TODO: Add custom print method test when implemented

test_that("subset_event_table works correctly", {
  event_data <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, 155,
    2, 2, 1, 160,
    1, 2, 1, 85,
    1, 1, 2, 305,
    2, 2, 2, 320,
    1, 2, 2, 170
  )

  event_table <- EventTable(data = event_data)

  # Subset by analysis
  subset_a1 <- subset_event_table(event_table, analysis = 1)
  expect_equal(subset_a1@n_analyses, 1L)
  expect_equal(nrow(subset_a1@data), 3)

  # Subset by hypotheses
  subset_h1 <- subset_event_table(event_table, hypotheses = c(1))
  expect_equal(nrow(subset_h1@data), 2) # Only H1-H1 pairs

  # Error for non-EventTable input
  expect_error(
    subset_event_table(data.frame(), analysis = 1),
    "x must be an EventTable object"
  )
})

test_that("as_event_table conversion works", {
  event_data <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, 155,
    2, 2, 1, 160
  )

  # Convert tibble to EventTable
  event_table <- as_event_table(event_data)
  expect_true(S7::S7_inherits(event_table, EventTable))

  # Should return same object if already EventTable
  event_table2 <- as_event_table(event_table)
  expect_identical(event_table, event_table2)
})

test_that("validate_event_table_data works", {
  # Valid data should pass
  valid_data <- tibble::tribble(
    ~H1, ~H2, ~Analysis, ~Event,
    1, 1, 1, 155,
    2, 2, 1, 160
  )

  expect_true(validate_event_table_data(valid_data))

  # Invalid data should fail
  invalid_data <- tibble::tribble(
    ~H1, ~H2, ~Analysis, # Missing Event column
    1, 1, 1,
    2, 2, 1
  )

  expect_error(
    validate_event_table_data(invalid_data),
    "Missing required columns: Event"
  )
})

test_that("EventTable validation - mathematical requirements", {
  # Test 1: Event counts must be non-decreasing across analyses for fixed H1,H2
  invalid_data_1 <- data.frame(
    H1 = c(1, 1, 1, 1),
    H2 = c(1, 1, 2, 2),
    Analysis = c(1, 2, 1, 2),
    Event = c(10, 8, 5, 7) # H1=1, H2=1 decreases from 10 to 8
  )
  expect_error(
    EventTable(data = invalid_data_1),
    "Event counts must be non-decreasing across analyses"
  )

  # Test 2: Diagonal elements must have Event >= off-diagonal elements
  invalid_data_2 <- data.frame(
    H1 = c(1, 1, 2, 2),
    H2 = c(1, 2, 1, 2),
    Analysis = c(1, 1, 1, 1),
    Event = c(5, 7, 6, 8) # H1=1,H2=1 (5) < H1=1,H2=2 (7) at Analysis=1
  )
  expect_error(
    EventTable(data = invalid_data_2),
    "Diagonal entry.*has Event.*< off-diagonal Event"
  )

  # Test 3: Valid data satisfying both requirements
  valid_data <- data.frame(
    H1 = c(1, 1, 1, 1, 2, 2, 2, 2),
    H2 = c(1, 1, 2, 2, 1, 1, 2, 2),
    Analysis = c(1, 2, 1, 2, 1, 2, 1, 2),
    Event = c(10, 15, 8, 12, 8, 12, 9, 14) # Non-decreasing and diagonals >= off-diagonals
  )
  et <- EventTable(data = valid_data)
  expect_true(S7::S7_inherits(et, EventTable))
  expect_equal(et@n_hypotheses, 2)
  expect_equal(et@n_analyses, 2)
})

test_that("EventTable validation - missing diagonal entries", {
  # Missing diagonal H1=1, H2=1
  incomplete_data <- data.frame(
    H1 = c(1, 2, 2),
    H2 = c(2, 1, 2),
    Analysis = c(1, 1, 1),
    Event = c(5, 6, 8)
  )
  expect_error(
    EventTable(data = incomplete_data),
    "Missing diagonal entry: H1=1, H2=1"
  )
})
