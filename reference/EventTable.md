# EventTable S7 Class

Create a type-safe S7 EventTable object that represents event count data
structure used in
[`generate_corr()`](https://merck.github.io/wpgsd/reference/generate_corr.md)
and
[`generate_event_table()`](https://merck.github.io/wpgsd/reference/generate_event_table.md).
This class provides validation and computed properties for hypothesis
and analysis counts.

## Usage

``` r
EventTable(data = tibble::tibble())
```

## Arguments

- data:

  A tibble or data.frame containing the required columns:

  - `H1`: First hypothesis index (numeric, positive integers)

  - `H2`: Second hypothesis index (numeric, positive integers)

  - `Analysis`: Analysis number (numeric, positive integers)

  - `Event`: Event count (numeric, non-negative)

## Value

An EventTable S7 object with validated data and computed properties

## Details

The EventTable class automatically validates the input data and
computes:

- `n_hypotheses`: Maximum hypothesis index across H1 and H2 columns

- `n_analyses`: Maximum analysis number

The class ensures data integrity by validating that:

- All required columns are present

- H1, H2, Analysis are positive integers and sequential

- Event counts are non-negative (can be decimals)

- For S7 validation: Event counts non-decreasing across analyses for
  fixed H1, H2

- For S7 validation: Diagonal entries have Event \>= corresponding
  off-diagonal entries

## Examples

``` r
library(tibble)

# Create valid event data
event_data <- tibble(
  H1 = c(1L, 2L, 1L, 1L, 2L, 1L),
  H2 = c(1L, 2L, 2L, 1L, 2L, 2L),
  Analysis = c(1L, 1L, 1L, 2L, 2L, 2L),
  Event = c(155, 160, 85, 305, 320, 170)
)

# Create EventTable object
event_table <- EventTable(data = event_data)

# Access properties
print(event_table@n_hypotheses) # Number of hypotheses
#> [1] 2
print(event_table@n_analyses) # Number of analyses
#> [1] 2
```
