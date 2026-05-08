# Correlated test statistics for

The weighted parametric group sequential design (WPGSD) (Anderson et al.
(2022)) approach allows one to take advantage of the known correlation
structure in constructing efficacy bounds to control family-wise error
rate (FWER) for a group sequential design. Here correlation may be due
to common observations in nested populations, due to common observations
in overlapping populations, or due to common observations in the control
arm.

## Methodologies to calculate correlations

Suppose that in a group sequential trial there are $`m`$ elementary null
hypotheses $`H_i`$, $`i \in I={1,...,m},`$ and there are $`K`$ analyses.
Let $`k=1,2,...K`$ be the index for the interim analyses and final
analyses. For any nonempty set $`J \subseteq I`$, we denote the
intersection hypothesis $`H_J=\cap_{j \in J}H_j`$. We note that $`H_I`$
is the global null hypothesis.

We assume the plan is for all hypotheses to be tested at each of the
$`k`$ planned analyses if the trial continues to the end for all
hypotheses. We further assume that the distribution of the
$`m \times K`$ tests of $`m`$ individual hypotheses at all $`k`$
analyses is multivariate normal with a completely known correlation
matrix.

Let $`Z_{ik}`$ be the standardized normal test statistic for hypothesis
$`i \in I`$, analysis $`1 \le k \le K`$. Let $`n_{ik}`$ be the number of
events collected cumulatively through stage $`k`$ for hypothesis $`i`$.
Then $`n_{i \wedge i',k \wedge k'}`$ is the number of events included in
both $`Z_{ik}`$ and $`i`$, $`i' \in I`$, $`1 \le k`$, $`k' \le K`$. The
key for the parametric tests is to utilize the correlation among the
test statistics. The correlation between $`Z_{ik}`$ and $`Z_{i'k'}`$ is
``` math
Corr(Z_{ik},Z_{i'k'})=\frac{n_{i \wedge i',k \wedge k'}}{\sqrt{n_{ik}\times n_{i'k'}}}
```
.

## Examples

We borrow an Example 1 from Anderson et al. (2022) here. Assume a
two-arm controlled clinical trial with one primary endpoint and three
patient populations defined by the status of two biomarkers, A and B:

- Biomarker A positive (population 1),
- Biomarker B positive (population 2),
- Overall population (population 3).

The 3 primary elementary null hypotheses are:

- **H1**: the experimental treatment is no different than control in
  population 1
- **H2**: the experimental treatment is no different than control in
  population 2
- **H3**: the experimental treatment is no different than control in the
  overall population

Assume an interim analysis and a final analysis are planned for the
study. The number of events observed at each analysis in each population
as well as the intersections of populations 1 and 2 to are assumed to be

``` r

library(dplyr)
library(tibble)
library(gt)
```

``` r

event_tb <- tribble(
  ~Population, ~"Number of Event at IA", ~"Number of Event at FA",
  "Population 1", 100, 200,
  "Population 2", 110, 220,
  "Overlap of Population 1 and 2", 80, 160,
  "Overall Population", 225, 450
)
event_tb %>%
  gt() %>%
  tab_header(title = "Number of events in each population")
```

| Number of events in each population |  |  |
|----|----|----|
| Population | Number of Event at IA | Number of Event at FA |
| Population 1 | 100 | 200 |
| Population 2 | 110 | 220 |
| Overlap of Population 1 and 2 | 80 | 160 |
| Overall Population | 225 | 450 |

### Correlation of tests for different populations at the same analysis

Let’s consider the situation at the interim analysis. The correlation
between tests of H1 and H2 will be
``` math
Corr(Z_{11},Z_{21})=\frac{n_{1 \wedge 2,1 \wedge 1}}{\sqrt{n_{11}\times n_{21}}}=\frac{80}{\sqrt{100\times 110}}=0.76.
```

The 80 in the numerator is the overlap number of events of population 1
and population 2 at the interim analysis.

### Correlation for tests of the same population at different analyses

We consider the correlation between tests for population 1 at the IA and
FA. The correlation will thus be:
``` math
Corr(Z_{11},Z_{12})=\frac{n_{1 \wedge 1,1 \wedge 2}}{\sqrt{n_{11}\times n_{12}}}=100/\sqrt{100\times 200}=0.71.
```

### Correlation of tests at different analyses for different populations

Next we consider the correlation between the tests of population 1 at
the interim analysis and population 2 at the final analyses. The
correlation will be
``` math
\text{Corr}(Z_{11},Z_{22})=\frac{n_{1 \wedge 1,2 \wedge 2}}{\sqrt{n_{11}\times n_{22}}}=\frac{80}{\sqrt{100\times 220}}=0.54.
```
The 80 in the numerator is the overlap number of events of population 1
in interim analysis and population 2 in final analysis; i.e., the events
in both populations 1 and 2 at the interim analysis.

## Generate the correlation matrix by `s7_generate_corr()`

Now we know how to calculate the correlation values under different
situations, and the `s7_generate_corr()` function was built based on
this logic. We can directly calculate the complete correlation matrix
for all tests via the function.

First, we need an event table including the event counts for testing
individual hypotheses and pairwise intersection hypotheses for the
study.

- `H1, H2` are each the index of a hypothesis. We will have the index
  for `H1` be $`<=`$ the index for `H2` to avoid duplication of pairs.
- `Analysis` indicates which of sequentially indexed analyses, interim
  and final, is being referred to.
- `Event` is the common number of events observed for testing both `H1`
  and `H2` at the specified `Analysis`.

For example: `H1=1`, `H2=1`, `Analysis=1`, `Event=100`indicates that in
the first population, there are 100 events for comparing the
experimental treatment to control at the interim analysis. Thus, when
`H1=H2`, we are looking at the number of events used for testing the
simple hypothesis specified in `H1=H2`.

For testing an intersection hypothesis with `H1 != H2` and a given
`Analysis` value, we are looking at the number of events that are used
in testing both `H1` and `H2`. For our example above, if `H1=1`, `H2=2`,
`Analysis=2`, `Event=160` indicates that the number of overlapping cases
for comparing the experimental treatment to the control in population 1
and 2 which is 160.

The column names needed for input to `s7_generate_corr()` are `H1`,
`H2`, `Analysis`, `Event`. We take the event counts from above
accordingly.

``` r

library(wpgsd)
# The event table
event <- tibble::tribble(
  ~H1, ~H2, ~Analysis, ~Event,
  1, 1, 1, 100,
  2, 2, 1, 110,
  3, 3, 1, 225,
  1, 2, 1, 80,
  1, 3, 1, 100,
  2, 3, 1, 110,
  1, 1, 2, 200,
  2, 2, 2, 220,
  3, 3, 2, 450,
  1, 2, 2, 160,
  1, 3, 2, 200,
  2, 3, 2, 220
)
```

By specifying this as the S7 type `EventTable` we get input checks to
ensure no missing values or inconsistencies.

``` r

event <- as_event_table(event)
event
```

    ## <wpgsd::EventTable> function (data = tibble::tibble())  
    ##  @ data        : tibble [12 × 4] (S3: tbl_df/tbl/data.frame)
    ##  $ H1      : num [1:12] 1 2 3 1 1 2 1 2 3 1 ...
    ##  $ H2      : num [1:12] 1 2 3 2 3 3 1 2 3 2 ...
    ##  $ Analysis: num [1:12] 1 1 1 1 1 1 2 2 2 2 ...
    ##  $ Event   : num [1:12] 100 110 225 80 100 110 200 220 450 160 ...
    ##  @ n_hypotheses: int 3
    ##  @ n_analyses  : int 2

For human readability of the data table:

``` r

event@data
```

    ## # A tibble: 12 × 4
    ##       H1    H2 Analysis Event
    ##    <dbl> <dbl>    <dbl> <dbl>
    ##  1     1     1        1   100
    ##  2     2     2        1   110
    ##  3     3     3        1   225
    ##  4     1     2        1    80
    ##  5     1     3        1   100
    ##  6     2     3        1   110
    ##  7     1     1        2   200
    ##  8     2     2        2   220
    ##  9     3     3        2   450
    ## 10     1     2        2   160
    ## 11     1     3        2   200
    ## 12     2     3        2   220

Now we input the above event table to the function of
[`generate_corr_s7()`](https://merck.github.io/wpgsd/reference/generate_corr_s7.md),
and get the correlation matrix as follow.

``` r

corr_mat <- generate_corr_s7(event)
corr_mat
```

    ## <wpgsd::CorrelationMatrix>
    ##  @ matrix      : num [1:6, 1:6] 1 0.763 0.667 0.707 0.539 ...
    ##  .. - attr(*, "dimnames")=List of 2
    ##  ..  ..$ : chr [1:6] "H1_A1" "H2_A1" "H3_A1" "H1_A2" ...
    ##  ..  ..$ : chr [1:6] "H1_A1" "H2_A1" "H3_A1" "H1_A2" ...
    ##  @ n_hypotheses: int 3
    ##  @ n_analyses  : int 2
    ##  @ column_names: chr [1:6] "H1_A1" "H2_A1" "H3_A1" "H1_A2" "H2_A2" "H3_A2"

For human readability, we can format the correlation matrix as a table.

``` r

corr_mat@matrix
```

    ##           H1_A1     H2_A1     H3_A1     H1_A2     H2_A2     H3_A2
    ## H1_A1 1.0000000 0.7627701 0.6666667 0.7071068 0.5393599 0.4714045
    ## H2_A1 0.7627701 1.0000000 0.6992059 0.5393599 0.7071068 0.4944132
    ## H3_A1 0.6666667 0.6992059 1.0000000 0.4714045 0.4944132 0.7071068
    ## H1_A2 0.7071068 0.5393599 0.4714045 1.0000000 0.7627701 0.6666667
    ## H2_A2 0.5393599 0.7071068 0.4944132 0.7627701 1.0000000 0.6992059
    ## H3_A2 0.4714045 0.4944132 0.7071068 0.6666667 0.6992059 1.0000000

## References

Anderson, Keaven M, Zifang Guo, Jing Zhao, and Linda Z Sun. 2022. “A
Unified Framework for Weighted Parametric Group Sequential Design.”
*Biometrical Journal* 64 (7): 1219–39.
