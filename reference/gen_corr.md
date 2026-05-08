# Convert correlation data frame to correlation matrix

Transforms a data frame of correlation results into a symmetric
correlation matrix with proper labeling.

## Usage

``` r
gen_corr(corr_df, M = NULL, K = NULL)
```

## Arguments

- corr_df:

  A data frame with columns H1, H2, Analysis1, Analysis2, Correlation

- M:

  Number of hypotheses (if NULL, inferred from data)

- K:

  Number of analyses (if NULL, inferred from data)

## Value

A symmetric correlation matrix of size (M*K) x (M*K)

## Examples

``` r
# Create sample correlation data frame
corr_df <- data.frame(
  H1 = c(1, 1, 2, 1, 2, 2),
  H2 = c(1, 2, 2, 1, 1, 2),
  Analysis1 = c(1, 1, 1, 2, 2, 2),
  Analysis2 = c(1, 1, 1, 2, 2, 2),
  Correlation = c(1, 0.5, 1, 1, 1, 1)
)

corr_matrix <- gen_corr(corr_df, M = 2, K = 2)
```
