#!/usr/bin/env Rscript

# Demo of EventTable S7 class validation requirements
# Run this with: Rscript examples/validation_demo.R

library(wpgsd)

cat("=== EventTable S7 Class Validation Demo ===\n\n")

# Example 1: Valid data that satisfies all requirements
cat("1. Creating valid EventTable:\n")
valid_data <- data.frame(
  H1 = c(1, 1, 1, 1, 2, 2, 2, 2),
  H2 = c(1, 1, 2, 2, 1, 1, 2, 2),
  Analysis = c(1, 2, 1, 2, 1, 2, 1, 2),
  Event = c(10, 15, 8, 12, 8, 12, 9, 14)  # Non-decreasing and diagonals >= off-diagonals
)

et <- EventTable(data = valid_data)
print(et)
cat("\n")

# Example 2: Invalid data - events decrease across analyses  
cat("2. Attempting to create EventTable with decreasing events:\n")
invalid_data_1 <- data.frame(
  H1 = c(1, 1, 1, 1),
  H2 = c(1, 1, 2, 2),
  Analysis = c(1, 2, 1, 2),
  Event = c(10, 8, 5, 7)  # H1=1, H2=1 decreases from 10 to 8
)

tryCatch({
  EventTable(data = invalid_data_1)
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})
cat("\n")

# Example 3: Invalid data - diagonal elements less than off-diagonal
cat("3. Attempting to create EventTable with invalid diagonal constraint:\n")
invalid_data_2 <- data.frame(
  H1 = c(1, 1, 2, 2),
  H2 = c(1, 2, 1, 2),
  Analysis = c(1, 1, 1, 1),
  Event = c(5, 7, 6, 8)  # H1=1,H2=1 (5) < H1=1,H2=2 (7)
)

tryCatch({
  EventTable(data = invalid_data_2)
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})
cat("\n")

# Example 4: Invalid data - missing diagonal entry
cat("4. Attempting to create EventTable with missing diagonal entry:\n")
incomplete_data <- data.frame(
  H1 = c(1, 2, 2),
  H2 = c(2, 1, 2),
  Analysis = c(1, 1, 1),
  Event = c(5, 6, 8)
)

tryCatch({
  EventTable(data = incomplete_data)
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})

cat("\nValidation requirements ensure mathematical consistency for correlation calculations!\n")
