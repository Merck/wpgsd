# EventTable S7 Class Implementation

## Overview

The `EventTable` S7 class provides a type-safe, validated data structure for representing event count data used in the wpgsd package. This is the first step in converting the wpgsd package to use S7 classes throughout.

## Features

### Core Properties
- **data**: A tibble containing the event count data with required columns `H1`, `H2`, `Analysis`, `Event`
- **n_hypotheses**: Automatically calculated number of hypotheses
- **n_analyses**: Automatically calculated number of analyses

### Validation
- Validates presence of required columns (`H1`, `H2`, `Analysis`, `Event`)
- Ensures proper data types (all numeric)
- Validates logical constraints:
  - Hypothesis indices must be positive integers
  - Analysis numbers must be positive integers  
  - Event counts must be non-negative
- Enforces mathematical consistency requirements:
  - For a fixed H1, H2 pair, Event counts must be non-decreasing as Analysis increases
  - For off-diagonal entries (H1 ≠ H2), diagonal entries must exist with Event ≥ off-diagonal Event for the same Analysis
  - These constraints ensure proper mathematical properties for correlation matrix calculations

### Methods
- **print()**: Clean formatted output showing key information
- **summary()**: Detailed summary including event count statistics
- **subset_event_table()**: Subset by analysis or hypotheses
- **as_event_table()**: Convert tibble to EventTable
- **validate_event_table_data()**: Validate data format before processing

## Usage Examples

### Basic Usage
```r
library(wpgsd)

# Create event data
event_data <- tibble::tribble(
  ~H1, ~H2, ~Analysis, ~Event,
  1, 1, 1, 155,
  2, 2, 1, 160,
  1, 2, 1, 85,
  1, 1, 2, 305,
  2, 2, 2, 320,
  1, 2, 2, 170
)

# Create EventTable object
event_table <- EventTable(data = event_data)
print(event_table)
```

### Data Validation
```r
# The constructor automatically validates data
tryCatch({
  invalid_data <- tibble::tibble(
    H1 = c(1, -2),  # Invalid: negative hypothesis index
    H2 = c(1, 2),
    Analysis = c(1, 1),
    Event = c(100, 200)
  )
  EventTable(data = invalid_data)
}, error = function(e) {
  cat("Validation error:", e$message)
})
```

### Subsetting
```r
# Subset by analysis
analysis_1 <- subset_event_table(event_table, analysis = 1)

# Subset by hypotheses
h1_h2 <- subset_event_table(event_table, hypotheses = c(1, 2))
```

### Integration with Existing Functions
```r
# Use with existing wpgsd functions
correlation_matrix <- generate_corr(event_table@data)
```

## Files Created

- `R/s7_classes.R`: Main S7 class definition
- `tests/testthat/test-s7-event-table.R`: Comprehensive unit tests
- `examples/test_event_table.R`: Basic usage examples
- `examples/event_table_integration.R`: Integration with existing functions

## Dependencies

- Added `S7` to package imports in `DESCRIPTION`
- Uses existing dependencies: `tibble`, `dplyr`

## Benefits

1. **Type Safety**: Prevents invalid data from being passed to wpgsd functions
2. **Validation**: Automatic validation of data format and constraints
3. **Documentation**: Self-documenting data structures
4. **Method Dispatch**: Extensible with specialized methods
5. **User Experience**: Clear error messages and helpful summaries

## Next Steps

This EventTable implementation provides the foundation for converting the wpgsd package to S7 classes. Future steps include:

1. Create `CorrelationMatrix` S7 class for `generate_corr()` output
2. Create `Bounds` S7 class for `generate_bounds()` output  
3. Update existing functions to accept/return S7 objects
4. Maintain backward compatibility with existing tibble/data.frame inputs

## Testing

Run the comprehensive test suite:
```r
testthat::test_file("tests/testthat/test-s7-event-table.R")
```

The tests cover:
- Object creation with valid data
- Validation of required columns
- Data type and value validation
- Print and summary methods
- Subsetting functionality
- Data conversion utilities
