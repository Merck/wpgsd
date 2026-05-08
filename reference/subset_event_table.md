# Subset EventTable by Analysis or Hypotheses

Extract a subset of an EventTable object based on analysis numbers or
hypothesis indices.

## Usage

``` r
subset_event_table(x, analysis = NULL, hypotheses = NULL)
```

## Arguments

- x:

  An EventTable S7 object

- analysis:

  Optional vector of analysis numbers to include

- hypotheses:

  Optional vector of hypothesis indices to include

## Value

A new EventTable object containing only the specified subset

## Examples

``` r
library(tibble)

# Create sample data
event_data <- tibble(
  H1 = c(1, 2, 3, 1, 2, 3),
  H2 = c(1, 2, 3, 1, 2, 3),
  Analysis = c(1, 1, 1, 2, 2, 2),
  Event = c(155, 160, 165, 305, 320, 335)
)
event_table <- EventTable(data = event_data)

# Subset by analysis
analysis_1 <- subset_event_table(event_table, analysis = 1)

# Subset by hypotheses
h1_h2_only <- subset_event_table(event_table, hypotheses = c(1, 2))
```
