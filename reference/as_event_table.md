# Convert Data to EventTable

Convert a tibble or data.frame to an EventTable S7 object, or return the
object unchanged if it's already an EventTable.

## Usage

``` r
as_event_table(data)
```

## Arguments

- data:

  A tibble, data.frame, or EventTable object containing the required
  columns (H1, H2, Analysis, Event)

## Value

An EventTable S7 object

## Examples

``` r
library(tibble)

# Convert tibble to EventTable
event_data <- tibble(
  H1 = c(1, 2),
  H2 = c(1, 2),
  Analysis = c(1, 1),
  Event = c(100, 200)
)

event_table <- as_event_table(event_data)

# If already EventTable, returns unchanged
same_table <- as_event_table(event_table)
identical(event_table, same_table) # TRUE
#> [1] TRUE
```
