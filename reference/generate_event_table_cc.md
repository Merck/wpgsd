# This function generates a table of events for given experimental arms and a control group based on specified hypotheses.

This function generates a table of events for given experimental arms
and a control group based on specified hypotheses.

## Usage

``` r
generate_event_table_cc(event, hypothesis)
```

## Arguments

- event:

  A dataframe containing the following columns:

  - `Population`: A character vector listing the population groups
    (e.g., experimental arms and control).

  - `IA`: A numeric vector indicating the number of events observed in
    each group during interim analysis.

  - `FA`: A numeric vector indicating the number of events observed in
    each group during final analysis. The dataframe must contain at
    least these columns and can include additional analysis columns as
    needed.

- hypothesis:

  A list containing hypotheses specifying comparisons between
  experimental arms and the control group, as well as comparisons among
  experimental arms.

## Value

A dataframe with columns:

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
  Population = c("Experimental 1", "Experimental 2", "Experimental 3", "Control"),
  IA = c(70, 75, 80, 85), # Interim Analysis values indicating the number of events observed in each group
  FA = c(135, 150, 165, 170)
)

hypothesis <- list(
  H1 = "Experimental 1 vs. Control",
  H2 = "Experimental 2 vs. Control",
  H3 = "Experimental 1 vs. Experimental 2"
)

generate_event_table_cc(event, hypothesis)
#> # A tibble: 12 × 4
#>    one_hypothesis another_hypothesis analysis common_events
#>             <int>              <int>    <int>         <dbl>
#>  1              1                  1        1           155
#>  2              1                  2        1            85
#>  3              1                  3        1            70
#>  4              2                  2        1           160
#>  5              2                  3        1            75
#>  6              3                  3        1           165
#>  7              1                  1        2           305
#>  8              1                  2        2           170
#>  9              1                  3        2           135
#> 10              2                  2        2           320
#> 11              2                  3        2           150
#> 12              3                  3        2           335

#----------------------Example of two IAs and FA
event <- data.frame(
  Population = c("Experimental 1", "Experimental 2", "Experimental 3", "Control"),
  IA1 = c(70, 75, 80, 85), # First Interim Analysis values indicating the number of events observed in each group
  IA2 = c(90, 95, 100, 105), # Second Interim Analysis values indicating the number of events observed in each group
  FA = c(135, 150, 165, 170)
)

hypothesis <- list(
  H1 = "Experimental 1 vs. Control",
  H2 = "Experimental 2 vs. Control",
  H3 = "Experimental 1 vs. Experimental 2"
)

generate_event_table_cc(event, hypothesis)
#> # A tibble: 18 × 4
#>    one_hypothesis another_hypothesis analysis common_events
#>             <int>              <int>    <int>         <dbl>
#>  1              1                  1        1           155
#>  2              1                  2        1            85
#>  3              1                  3        1            70
#>  4              2                  2        1           160
#>  5              2                  3        1            75
#>  6              3                  3        1           165
#>  7              1                  1        2           195
#>  8              1                  2        2           105
#>  9              1                  3        2            90
#> 10              2                  2        2           200
#> 11              2                  3        2            95
#> 12              3                  3        2           205
#> 13              1                  1        3           305
#> 14              1                  2        3           170
#> 15              1                  3        3           135
#> 16              2                  2        3           320
#> 17              2                  3        3           150
#> 18              3                  3        3           335
```
