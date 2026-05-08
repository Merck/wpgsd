# Calculate sequential p-values for interaction/elementary hypothesis

Calculate sequential p-values for interaction/elementary hypothesis

## Usage

``` r
calc_seq_p(
  test_analysis = 2,
  test_hypothesis = "H1, H2, H3",
  p_obs = tibble::tibble(analysis = 1:2, H1 = c(0.02, 0.0015), H2 = c(0.01, 0.01), H3 =
    c(0.01, 0.004)),
  alpha_spending_type = 2,
  n_analysis = 2,
  initial_weight = c(0.3, 0.3, 0.4),
  transition_mat = matrix(c(0, 0.4285714, 0.5714286, 0.4285714, 0, 0.5714286, 0.5, 0.5,
    0), nrow = 3, byrow = TRUE),
  z_corr = matrix(c(1, 0.7627701, 0.6666667, 0.7071068, 0.5393599, 0.4714045, 0.7627701,
    1, 0.6992059, 0.5393599, 0.7071068, 0.4944132, 0.6666667, 0.6992059, 1, 0.4714045,
    0.4944132, 0.7071068, 0.7071068, 0.5393599, 0.4714045, 1, 0.7627701, 0.6666667,
    0.5393599, 0.7071068, 0.4944132, 0.7627701, 1, 0.6992059, 0.4714045, 0.4944132,
    0.7071068, 0.6666667, 0.6992059, 1), nrow = 6, byrow = TRUE),
  spending_fun = gsDesign::sfHSD,
  spending_fun_par = -4,
  info_frac = c(0.5, 1),
  interval = c(1e-04, 0.2)
)
```

## Arguments

- test_analysis:

  The index of the analysis to be tested, such as 1, 2, ...

- test_hypothesis:

  A character of the tested interaction/elementary hypothesis, such as
  `"H1, H2, H3"`, `H1, H2`, `"H1"`.

- p_obs:

  Observed p-values up to `test_analysis`.

- alpha_spending_type:

  Type Boundary type.

  - `0` - Bonferroni. Separate alpha spending for each hypotheses.

  - `1` - Fixed alpha spending for all hypotheses. Method 3a in the
    manuscript.

  - `2` - Overall alpha spending for all hypotheses. Method 3b in the
    manuscript.

  - `3` - Separate alpha spending for each hypotheses. Method 3c in the
    manuscript.

- n_analysis:

  Total number of analysis.

- initial_weight:

  Initial weight assigned to the elementary hypothesis.

- transition_mat:

  Transition matrix.

- z_corr:

  Correlation matrix of the Z statistics.

- spending_fun:

  Spending function.

- spending_fun_par:

  Parameter of the spending function.

- info_frac:

  Information fractions.

- interval:

  Interval to search the uniroot.

## Value

The sequential p-values of the `test_hypothesis` at the `test_analysis`.

## Examples

``` r
# \donttest{
calc_seq_p(
  test_analysis = 2,
  test_hypothesis = "H1, H2, H3",
  p_obs = tibble::tibble(
    analysis = 1:2,
    H1 = c(0.02, 0.0015),
    H2 = c(0.01, 0.01),
    H3 = c(0.01, 0.004)
  ),
  alpha_spending_type = 2,
  n_analysis = 2,
  initial_weight = c(0.3, 0.3, 0.4),
  transition_mat = matrix(c(
    0.0000000, 0.4285714, 0.5714286,
    0.4285714, 0.0000000, 0.5714286,
    0.5000000, 0.5000000, 0.0000000
  ), nrow = 3, byrow = TRUE),
  z_corr = matrix(
    c(
      1.0000000, 0.7627701, 0.6666667, 0.7071068, 0.5393599, 0.4714045,
      0.7627701, 1.0000000, 0.6992059, 0.5393599, 0.7071068, 0.4944132,
      0.6666667, 0.6992059, 1.0000000, 0.4714045, 0.4944132, 0.7071068,
      0.7071068, 0.5393599, 0.4714045, 1.0000000, 0.7627701, 0.6666667,
      0.5393599, 0.7071068, 0.4944132, 0.7627701, 1.0000000, 0.6992059,
      0.4714045, 0.4944132, 0.7071068, 0.6666667, 0.6992059, 1.0000000
    ),
    nrow = 6, byrow = TRUE
  ),
  spending_fun = gsDesign::sfHSD,
  spending_fun_par = -4,
  info_frac = c(0.5, 1),
  interval = c(1e-4, 0.2)
)
#> [1] 0.004514193
# }
```
