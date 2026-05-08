# Perform closed testing procedure

Perform closed testing procedure

## Usage

``` r
closed_test(bounds, p_obs)
```

## Arguments

- bounds:

  A tibble of nominal p-value boundaries from
  [`generate_bounds()`](https://merck.github.io/wpgsd/reference/generate_bounds.md)
  containing columns `Analysis`, `Hypotheses`, `H1`, `H2`, etc.

- p_obs:

  A tibble of observed p-values containing columns `Analysis`, `H1`,
  `H2`, etc.

## Value

An outcome matrix summarizing the testing results.

## Examples

``` r
p_obs <- dplyr::bind_rows(
  tibble::tibble(Analysis = 1, H1 = 0.001, H2 = 0.001),
  tibble::tibble(Analysis = 2, H1 = 0.001, H2 = 0.001)
)
bound <- tibble::tribble(
  ~Analysis, ~Hypotheses, ~H1, ~H2,
  1, "H1", 0.02, NA,
  1, "H1, H2", 0.0001, 0.00001,
  1, "H2", NA, 0.003,
  2, "H1", 0.02, NA,
  2, "H1, H2", 0.02, 0.00001,
  2, "H2", NA, 0.003
)

closed_test <- closed_test(bound, p_obs)
```
