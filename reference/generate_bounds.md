# Compute p-value boundaries of the parametric MTP method with overall alpha spending for all hypotheses

Compute p-value boundaries of the parametric MTP method with overall
alpha spending for all hypotheses

## Usage

``` r
generate_bounds(
  type = 1,
  k = 2,
  w = w,
  m = m,
  corr = corr,
  alpha = 0.025,
  cum_alpha = NULL,
  maxpts = 50000,
  abseps = 1e-05,
  tol = 1e-10,
  sf = gsDesign::sfHSD,
  sfparm = -4,
  t = c(0.5, 1),
  ...
)
```

## Arguments

- type:

  Boundary type.

  - `0` = Bonferroni. Separate alpha spending for each hypotheses.

  - `1` = Fixed alpha spending for all hypotheses. Method 3a in the
    manuscript.

  - `2` = Overall alpha spending for all hypotheses. Method 3b in the
    manuscript.

  - `3` = Separate alpha spending for each hypotheses. Method 3c in the
    manuscript.

- k:

  Number of analyses up to the current analysis.

- w:

  Initial weights.

- m:

  Transition matrix.

- corr:

  Correlation matrix of all test statistics up to the current analysis.
  dim = k \* length(w).

- alpha:

  Overall alpha.

- cum_alpha:

  Cumulative alpha spent at each analysis. Only required for `type = 1`.

- maxpts:

  GenzBretz function maximum number of function values as integer.

- abseps:

  GenzBretz function absolute error tolerance.

- tol:

  Find root tolerance.

- sf:

  A list of alpha spending functions to spend alpha for each hypotheses.

  - If `type = 0` or `3` then length equals to number of hypotheses.

  - If `type = 1` then `sf` is not needed.

  - If `type = 2` then only the first component is used.

- sfparm:

  A list of parameters to be supplied to sfs.

  - If `type = 0` or `3` then length equals to number of hypotheses.

  - If `type = 1` then `sfparm` is not needed.

  - If `type = 2` then only the first component is used.

- t:

  A list of information fraction used for alpha spending, may be
  different from the actual information fraction. Each component
  corresponds to a hypothesis.

  - If `type = 0` or `3` then length equals to number of hypotheses.

  - If `type = 1` then `t` is not needed.

  - If `type = 2` then only the first component is used.

- ...:

  Additional arguments.

## Value

A tibble with `k * (2^(n_hypotheses - 1))` rows of p-value boundaries.
Inflation factor is also provided if `type = 3`.

## Examples

``` r
# Build the transition matrix
m <- matrix(c(
  0, 0.5, 0.5,
  0.5, 0, 0.5,
  0.5, 0.5, 0
), nrow = 3, byrow = TRUE)

# Initialize weights
w <- c(1 / 3, 1 / 3, 1 / 3)

# Input information fraction
IF_IA <- c(155 / 305, 160 / 320, 165 / 335)

# Input event count of intersection of paired hypotheses - Table 2
event <- tibble::tribble(
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
gs_corr <- generate_corr(event)

# Generate bounds
generate_bounds(
  type = 3,
  k = 2,
  w = w,
  m = m,
  corr = gs_corr,
  alpha = 0.025,
  sf = list(gsDesign::sfLDOF, gsDesign::sfLDOF, gsDesign::sfLDOF),
  sfparm = list(0, 0, 0),
  t = list(c(IF_IA[1], 1), c(IF_IA[2], 1), c(IF_IA[3], 1))
)
#> # A tibble: 14 × 6
#>    Analysis Hypotheses        H1        H2        H3    xi
#>       <int> <chr>          <dbl>     <dbl>     <dbl> <dbl>
#>  1        1 H1          0.00167  NA        NA         1   
#>  2        1 H1, H2      0.000471  0.000423 NA         1.03
#>  3        1 H1, H2, H3  0.000223  0.000198  0.000177  1.04
#>  4        1 H1, H3      0.000470 NA         0.000382  1.02
#>  5        1 H2         NA         0.00153  NA         1   
#>  6        1 H2, H3     NA         0.000421  0.000381  1.02
#>  7        1 H3         NA        NA         0.00140   1   
#>  8        2 H1          0.0245   NA        NA         1   
#>  9        2 H1, H2      0.0135    0.0135   NA         1.09
#> 10        2 H1, H2, H3  0.00949   0.00950   0.00951   1.15
#> 11        2 H1, H3      0.0135   NA         0.0135    1.09
#> 12        2 H2         NA         0.0245   NA         1   
#> 13        2 H2, H3     NA         0.0134    0.0134    1.09
#> 14        2 H3         NA        NA         0.0245    1   
```
