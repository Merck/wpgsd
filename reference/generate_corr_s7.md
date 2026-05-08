# Generate S7 CorrelationMatrix from EventTable or event data

Enhanced version of generate_corr() that returns a CorrelationMatrix S7
object with proper validation and type safety. Uses the new
compute_correlations() function for mathematically rigorous correlation
computation.

This function requires an EventTable S7 object as input and returns a
CorrelationMatrix with column ordering that matches generate_corr()
(Analysis then Hypothesis: H1A1, H2A1, H1A2, H2A2, ...).

## Usage

``` r
generate_corr_s7(event_table, check = TRUE)
```

## Arguments

- event_table:

  An EventTable S7 object containing validated event count data

- check:

  Logical indicating whether to perform input validation (default TRUE)

## Value

A CorrelationMatrix S7 object containing the correlation matrix with
proper validation and metadata, ordered by Analysis then Hypothesis

## Examples

``` r
library(tibble)

# Create EventTable S7 object
event_data <- tibble(
  H1 = c(1, 2, 1, 1, 2, 1),
  H2 = c(1, 2, 2, 1, 2, 2),
  Analysis = c(1, 1, 1, 2, 2, 2),
  Event = c(155, 160, 85, 305, 320, 170)
)
event_table <- EventTable(data = event_data)
corr_matrix_s7 <- generate_corr_s7(event_table)
print(corr_matrix_s7)
#> <wpgsd::CorrelationMatrix>
#>  @ matrix      : num [1:4, 1:4] 1 0.54 0.713 0.382 0.54 ...
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "H1_A1" "H2_A1" "H1_A2" "H2_A2"
#>  ..  ..$ : chr [1:4] "H1_A1" "H2_A1" "H1_A2" "H2_A2"
#>  @ n_hypotheses: int 2
#>  @ n_analyses  : int 2
#>  @ column_names: chr [1:4] "H1_A1" "H2_A1" "H1_A2" "H2_A2"
```
