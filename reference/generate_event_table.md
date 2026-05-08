# Generate table of event counts from ADSL and ADTTE datasets

Generate table of event counts from ADSL and ADTTE datasets

## Usage

``` r
generate_event_table(paths, h_select, adsl_name, adtte_name, key_var, cnsr_var)
```

## Arguments

- paths:

  A vector of paths for analysis datasets. Length should be equal to the
  number of analyses completed.

- h_select:

  Selection criterion for each hypothesis. Should be a tibble containing
  2 columns: `Hypothesis` and `Crit`.

- adsl_name:

  SAS dataset name for subject-level analysis data. Usually it is
  `"adsl"`.

- adtte_name:

  SAS dataset name for time-to-event analysis data. Usually it is
  `"adtte"`.

- key_var:

  Key variable to join the `adsl` and `adtte` datasets. For example,
  `"USUBJID"` or `"SUBJID"`.

- cnsr_var:

  Variable to indicate censoring (`1` = censor; `0` = event). For
  example, `"CNSR"`.

## Value

A list with two components:

- `event`: an event count table as input for
  [`generate_bounds()`](https://merck.github.io/wpgsd/reference/generate_bounds.md).

- `dsets`: analysis datasets of each hypothesis.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

paths <- system.file("extdata/", package = "wpgsd")

# Selection criteria for each hypothesis
h_select <- tibble::tribble(
  ~Hypothesis, ~Crit,
  1, "PARAMCD == 'OS' & TRT01P %in% c('Xanomeline High Dose', 'Placebo')",
  2, "PARAMCD == 'OS' & TRT01P %in% c('Xanomeline Low Dose', 'Placebo')"
)

event <- generate_event_table(paths, h_select,
  adsl_name = "adsl", adtte_name = "adtte",
  key_var = "USUBJID", cnsr_var = "CNSR"
)$event

event %>%
  gt::gt() %>%
  gt::tab_header(title = "Event Count - Computed from SAS Datasets Example")


  


Event Count - Computed from SAS Datasets Example
```
