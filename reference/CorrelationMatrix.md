# CorrelationMatrix S7 Class

Create a type-safe S7 CorrelationMatrix object that represents
correlation matrices used in the wpgsd package. This class provides
validation for matrix properties such as symmetry and positive
definiteness.

## Usage

``` r
CorrelationMatrix(
  matrix = matrix(numeric(), nrow = 0, ncol = 0),
  n_hypotheses = 0L,
  n_analyses = 0L,
  column_names = character()
)
```

## Arguments

- matrix:

  A numeric matrix representing correlations

- n_hypotheses:

  Integer number of hypotheses

- n_analyses:

  Integer number of analyses

- column_names:

  Character vector of column names

## Value

A CorrelationMatrix S7 object with validated matrix and metadata

## Details

The CorrelationMatrix class validates that:

- Matrix is symmetric (with tolerance 1e-12)

- Matrix is positive definite

- Diagonal elements are 1 (within tolerance)

- Dimensions are consistent with n_hypotheses and n_analyses

## Examples

``` r
# Create a simple 2x2 correlation matrix
corr_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
corr_obj <- CorrelationMatrix(
  matrix = corr_matrix,
  n_hypotheses = 1L,
  n_analyses = 2L,
  column_names = colnames(corr_matrix)
)

print(corr_obj)
#> <wpgsd::CorrelationMatrix>
#>  @ matrix      : num [1:2, 1:2] 1 0.5 0.5 1
#>  @ n_hypotheses: int 1
#>  @ n_analyses  : int 2
#>  @ column_names: chr [1:2] "H1_A1" "H1_A2"
```
