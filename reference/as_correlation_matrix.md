# Convert matrix to CorrelationMatrix object

Converts a regular numeric matrix to a CorrelationMatrix S7 object with
validation.

## Usage

``` r
as_correlation_matrix(matrix, n_hypotheses = 0L, n_analyses = 0L)
```

## Arguments

- matrix:

  A numeric correlation matrix

- n_hypotheses:

  Integer number of hypotheses (optional)

- n_analyses:

  Integer number of analyses (optional)

## Value

A CorrelationMatrix S7 object

## Examples

``` r
corr_mat <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
corr_obj <- as_correlation_matrix(corr_mat, n_hypotheses = 1, n_analyses = 2)
print(corr_obj)
#> <wpgsd::CorrelationMatrix>
#>  @ matrix      : num [1:2, 1:2] 1 0.3 0.3 1
#>  @ n_hypotheses: int 1
#>  @ n_analyses  : int 2
#>  @ column_names: chr [1:2] "H1_A1" "H1_A2"
```
