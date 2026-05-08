# Compute correlations from event data

Computes correlations between test statistics using the mathematically
correct formulation that ensures positive definiteness and proper
asymptotic properties.

## Usage

``` r
compute_correlations(event, check = TRUE, return_matrix = TRUE)
```

## Arguments

- event:

  A data frame with columns H1, H2, Analysis, and Event containing event
  count data for correlation computation

- check:

  Logical indicating whether to perform input validation (default: TRUE)

- return_matrix:

  Logical indicating whether to return as matrix (TRUE) or data frame
  (FALSE). Default: TRUE

## Value

If return_matrix=TRUE, returns a symmetric correlation matrix. If
return_matrix=FALSE, returns a data frame with columns H1, H2,
Analysis1, Analysis2, Correlation.

## Examples

``` r
library(tibble)

# Sample event data
event_data <- tribble(
  ~H1, ~H2, ~Analysis, ~Event,
  1, 1, 1, 80,
  2, 2, 1, 100,
  1, 2, 1, 60,
  1, 1, 2, 120,
  2, 2, 2, 150,
  1, 2, 2, 80
)

# Get correlation matrix
corr_matrix <- compute_correlations(event_data)

# Get detailed data frame
corr_df <- compute_correlations(event_data, return_matrix = FALSE)
```
