# Test script for EventTable S7 class
# This demonstrates how to use the new EventTable class

library(wpgsd)
library(tibble)

# Create sample event data as used in the package
event_data <- tibble::tribble(
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
event_table <- EventTable(data = event_data)

# Print the object
print(event_table)

# Get summary
summary(event_table)

# Subset by analysis
subset_analysis_1 <- subset_event_table(event_table, analysis = 1)
print("Analysis 1 subset:")
print(subset_analysis_1)

# Subset by hypotheses
subset_h1_h2 <- subset_event_table(event_table, hypotheses = c(1, 2))
print("H1-H2 subset:")
print(subset_h1_h2)

# Convert existing tibble to EventTable
converted_table <- as_event_table(event_data)
print("Converted table:")
print(converted_table)

# Validate data
validate_event_table_data(event_data)
print("Data validation passed!")
