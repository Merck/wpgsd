# This function creates a table summarizing event counts based on specified hypotheses and user input data. It can handle two types of analysis: one comparing experimental groups to a common control and another analyzing the overlap of populations.

This function creates a table summarizing event counts based on
specified hypotheses and user input data. It can handle two types of
analysis: one comparing experimental groups to a common control and
another analyzing the overlap of populations.

## Usage

``` r
generate_event_table_(
  event,
  hypothesis,
  type = c("common_control", "overlap_population")
)
```

## Arguments

- event:

  dataframe should have the following structure:

  - `Population`: A character vector indicating the population groups.
    For example, "Population 1", "Population 2", "Overall population" in
    overlap population situation; or experimental arms and control in
    common control situation.

  - `IA`: Numeric vector indicating the number of events observed in
    each group during interim analysis.

  - `FA`: Numeric vector indicating the number of events observed in
    each group during final analysis. The dataframe must contain at
    least these columns and can include additional analysis columns as
    needed.

- hypothesis:

  A list containing hypotheses that specify the comparisons to be made
  between the groups: - For example: - "Experimental 1 vs. Control" -
  "Efficacy in Population 1"

- type:

  A character string specifying the type of analysis to conduct. It
  should be one of the following: - `"common_control"`: Analyze the
  event counts comparing experimental groups to common control. -
  `"overlap_population"`: Analyze the event counts to assess overlap in
  populations.

## Value

A dataframe with four columns: - `one_hypothesis`: The index of the
first selected hypothesis from the provided list. -
`another_hypothesis`: The index of the second selected hypothesis from
the provided list. - `analysis`: The index indicating which analysis is
being performed (e.g., interim or final). - `common_events`: The
calculated number of common events associated with the selected
hypotheses.

## Examples

``` r
# ----------------------- Example of common control
event <- data.frame(
  Population = c("Experimental 1", "Experimental 2", "Experimental 3", "Control"),
  # Interim analysis values indicating the number of events observed in each experimental group.
  IA = c(70, 75, 80, 85),
  # Final analysis values indicating the cumulative number of events observed in each group.
  FA = c(135, 150, 165, 170)
)

hypothesis <- list(
  H1 = "Experimental 1 vs. Control",
  H2 = "Experimental 2 vs. Control",
  H3 = "Experimental 1 vs. Experimental 2"
)

generate_event_table_(event, hypothesis, type = "common_control")
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

# ------------------------ Example of overall population
event <- data.frame(
  Population = c("Population 1", "Population 2", "Population 1 Intersection 2",
                 "Overall population"),
  IA = c(100, 110, 80, 225), # Interim analysis values for the overall population.
  FA = c(200, 220, 160, 450) # Final analysis values for the overall population.
)

hypothesis <- list(
  H1 = "Efficacy in Population 1",
  H2 = "Efficacy in Population 2",
  H3 = "Efficacy in Overall population"
)

generate_event_table_(event, hypothesis, type = "overlap_population")
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
```
