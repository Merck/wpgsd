# Validate EventTable Data Format

Validate that a data.frame or tibble has the correct structure and data
types required for creating an EventTable object.

## Usage

``` r
validate_event_table_data(data)
```

## Arguments

- data:

  A data.frame or tibble to validate

## Value

`TRUE` if validation passes (invisible), otherwise stops with
descriptive error message

## Details

This function checks that:

- Required columns (H1, H2, Analysis, Event) are present

- All columns are numeric

- Hypothesis indices (H1, H2) are positive

- Analysis numbers are positive

- Event counts are non-negative

## Examples

``` r
library(tibble)

# Valid data passes silently
valid_data <- tibble(
  H1 = c(1, 2),
  H2 = c(1, 2),
  Analysis = c(1, 1),
  Event = c(100, 200)
)
validate_event_table_data(valid_data) # Returns TRUE

# Invalid data throws error
if (FALSE) { # \dontrun{
invalid_data <- tibble(H1 = c(1, 2)) # Missing columns
validate_event_table_data(invalid_data) # Error
} # }
```
