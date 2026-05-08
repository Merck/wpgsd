# This function generates a table of events for specified populations based on the provided hypotheses.

This function generates a table of events for specified populations
based on the provided hypotheses.

## Usage

``` r
generate_event_table_ol(event, hypothesis)
```

## Arguments

- event:

  dataframe should have the following structure:

  - `Population`: A character vector indicating the population groups
    (e.g., "Population 1", "Population 2", "Population 1 Intersection
    2", and "Overall population").

  - `IA`: Numeric vector indicating the number of events observed in
    each group during interim analysis.

  - `FA`: Numeric vector indicating the number of events observed in
    each group during final analysis. The dataframe must contain at
    least these columns and can include additional analysis columns as
    needed.

- hypothesis:

  A list of strings where each item represents a hypothesis regarding
  efficacy, formatted as follows: - H1: "Efficacy in Population 1" - H2:
  "Efficacy in Population 2" - H3: "Efficacy in Overall population" Each
  hypothesis is used for comparisons in the generated event table.

## Value

A dataframe with the following columns:

- `one_hypothesis`: The index of the first selected hypothesis from the
  provided list.

- `another_hypothesis`: The index of the second selected hypothesis from
  the provided list.

- `analysis`: The index indicating which analysis is being performed
  (e.g., interim or final).

- `common_events`: The calculated number of common events associated
  with the selected hypotheses.

## Examples

``` r
#------------------------Example of IA and FA
event <- data.frame(
  Population = c("Population 1", "Population 2", "Population 1 Intersection 2", "Overall population"),
  IA = c(100, 110, 80, 225), # Interim Analysis values indicating the number of events observed in each group
  FA = c(200, 220, 160, 450)
)

hypothesis <- list(
  H1 = "Efficacy in Population 1",
  H2 = "Efficacy in Population 2",
  H3 = "Efficacy in Overall population"
)

generate_event_table_ol(event, hypothesis)
#> # A tibble: 12 × 4
#>    one_hypothesis another_hypothesis analysis common_events
#>             <int>              <int>    <int>         <dbl>
#>  1              1                  1        1           100
#>  2              1                  2        1            80
#>  3              1                  3        1           100
#>  4              2                  2        1           110
#>  5              2                  3        1           110
#>  6              3                  3        1           225
#>  7              1                  1        2           200
#>  8              1                  2        2           160
#>  9              1                  3        2           200
#> 10              2                  2        2           220
#> 11              2                  3        2           220
#> 12              3                  3        2           450

#----------------------Example of two IAs and FA
event <- data.frame(
  Population = c("Population 1", "Population 2", "Population 1 Intersection 2", "Overall population"),
  IA1 = c(100, 110, 80, 225), # First Interim Analysis values indicating the number of events observed in each group
  IA2 = c(120, 130, 90, 240), # Second Interim Analysis values indicating the number of events observed in each group
  FA = c(200, 220, 160, 450)
)

hypothesis <- list(
  H1 = "Efficacy in Population 1",
  H2 = "Efficacy in Population 2",
  H3 = "Efficacy in Overall population"
)

generate_event_table_ol(event, hypothesis)
#> # A tibble: 18 × 4
#>    one_hypothesis another_hypothesis analysis common_events
#>             <int>              <int>    <int>         <dbl>
#>  1              1                  1        1           100
#>  2              1                  2        1            80
#>  3              1                  3        1           100
#>  4              2                  2        1           110
#>  5              2                  3        1           110
#>  6              3                  3        1           225
#>  7              1                  1        2           120
#>  8              1                  2        2            90
#>  9              1                  3        2           120
#> 10              2                  2        2           130
#> 11              2                  3        2           130
#> 12              3                  3        2           240
#> 13              1                  1        3           200
#> 14              1                  2        3           160
#> 15              1                  3        3           200
#> 16              2                  2        3           220
#> 17              2                  3        3           220
#> 18              3                  3        3           450
```
