# Extract correlation submatrix

Extract a submatrix from a CorrelationMatrix object based on analysis
numbers or hypothesis indices.

## Usage

``` r
subset_correlation_matrix(x, analysis = NULL, hypotheses = NULL)
```

## Arguments

- x:

  A CorrelationMatrix S7 object

- analysis:

  Optional vector of analysis numbers to include

- hypotheses:

  Optional vector of hypothesis indices to include

## Value

A new CorrelationMatrix object containing only the specified subset

## Examples

``` r
library(tibble)

# Create sample data and correlation matrix
event_data <- tibble(
  H1 = c(1, 2, 1, 1, 2, 1),
  H2 = c(1, 2, 2, 1, 2, 2),
  Analysis = c(1, 1, 1, 2, 2, 2),
  Event = c(155, 160, 85, 305, 320, 170)
)
corr_matrix <- generate_corr(event_data)
corr_obj <- CorrelationMatrix(
  matrix = corr_matrix,
  n_hypotheses = 2L,
  n_analyses = 2L
)

# Extract subset for analysis 1 only
subset_corr <- subset_correlation_matrix(corr_obj, analysis = 1)
```
