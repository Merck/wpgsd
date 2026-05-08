# Example integration of EventTable S7 class with existing wpgsd functions
# This shows how EventTable can work with generate_corr()

library(wpgsd)
library(tibble)

# Example 1: Using EventTable with generate_corr()
# Create event data using the EventTable class
event_data_raw <- tibble::tribble(
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

# Create EventTable object
event_table <- EventTable(data = event_data_raw)
print("Created EventTable:")
print(event_table)

# Use with generate_corr (currently expects tibble/data.frame)
# The @data slot extracts the underlying tibble
correlation_matrix <- generate_corr(event_table@data)
print("Generated correlation matrix:")
print(correlation_matrix)

# Example 2: Data validation before processing
cat("\nExample 2: Data validation\n")
cat("==========================\n")

# Try to create EventTable with invalid data
tryCatch({
  invalid_data <- tibble::tibble(
    H1 = c(1, 2, -1),  # Invalid: negative hypothesis index
    H2 = c(1, 2, 3),
    Analysis = c(1, 1, 1),
    Event = c(100, 200, 150)
  )
  
  invalid_table <- EventTable(data = invalid_data)
}, error = function(e) {
  cat("Validation caught error:", e$message, "\n")
})

# Example 3: Subsetting capabilities
cat("\nExample 3: Subsetting\n")
cat("=====================\n")

# Get only first analysis
analysis_1 <- subset_event_table(event_table, analysis = 1)
cat("Analysis 1 events:\n")
print(analysis_1@data)

# Get only H1-H2 hypothesis pairs
h1_h2_events <- subset_event_table(event_table, hypotheses = c(1, 2))
cat("\nH1-H2 hypothesis events:\n")
print(h1_h2_events@data)

# Example 4: Summary information
cat("\nExample 4: Summary\n")
cat("==================\n")
summary(event_table)
