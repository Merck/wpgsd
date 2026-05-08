# Utility function for root-finding to compute inflation factor xi with the separate alpha spending approach

Utility function for root-finding to compute inflation factor xi with
the separate alpha spending approach

## Usage

``` r
find_xi(
  a,
  alpha_prev = NULL,
  aprime,
  xi,
  sig,
  maxpts = 50000,
  abseps = 1e-05,
  ...
)
```

## Arguments

- a:

  Sum of cumulative alpha spending from the Bonferroni approach.

- alpha_prev:

  alpha boundary at previous interim analyses using the MTP approach.

- aprime:

  Nominal alpha boundary from the Bonferroni approach.

- xi:

  Inflation factor.

- sig:

  Correlation matrix of previous and current analyses test statistics.

- maxpts:

  GenzBretz function maximum number of function values as integer.

- abseps:

  GenzBretz function absolute error tolerance.

- ...:

  Additional arguments.

## Value

Difference. Should be 0 with `xi` identified.

## Examples

``` r
# Input event count of intersection of paired hypotheses - Table 2
my_event <- tibble::tribble(
  ~H1, ~H2, ~Analysis, ~Event,
  1, 1, 1, 155,
  2, 2, 1, 160,
  3, 3, 1, 165,
  1, 2, 1, 85,
  1, 3, 1, 85,
  2, 3, 1, 85,
  1, 1, 2, 305,
  2, 2, 2, 320,
  3, 3, 2, 335,
  1, 2, 2, 170,
  1, 3, 2, 170,
  2, 3, 2, 170
)

# Generate correlation from events
my_corr <- generate_corr(my_event)

# Find the inflation factor for H1, H2 at analysis 1
find_xi(
  a = 0.0008708433,
  alpha_prev = NULL,
  aprime = c(0.0004588644, 0.0004119789),
  xi = 1,
  sig = my_corr[
    colnames(my_corr) %in% c("H1_A1", "H2_A1"),
    colnames(my_corr) %in% c("H1_A1", "H2_A1")
  ]
)
#> [1] -2.237679e-05
#> attr(,"error")
#> [1] 1e-15
#> attr(,"msg")
#> [1] "Normal Completion"
```
