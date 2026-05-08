# Check event data for correlation computation

This function validates event data before correlation computation. It
ensures the data has the correct structure and satisfies all
mathematical requirements for computing correlations.

## Usage

``` r
check_event_data(event)
```

## Arguments

- event:

  A data.frame or tibble containing event data with columns H1, H2,
  Analysis, and Event

## Value

`TRUE` if validation passes (invisible), otherwise stops with
descriptive error message

## Details

This function performs comprehensive validation including:

- Required columns and data types

- H1 \<= H2 requirement for correlation computation

- Sequential hypothesis and analysis indices

- Diagonal entries exist for all off-diagonal entries

- Unique combinations of H1, H2, Analysis

## Examples

``` r
library(tibble)

# Valid event data
event_data <- tibble(
  H1 = c(1, 2, 1, 1, 2, 1),
  H2 = c(1, 2, 2, 1, 2, 2),
  Analysis = c(1, 1, 1, 2, 2, 2),
  Event = c(155, 160, 85, 305, 320, 170)
)
check_event_data(event_data)
```
