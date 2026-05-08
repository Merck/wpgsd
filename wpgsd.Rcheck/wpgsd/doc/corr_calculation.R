## ----message=FALSE------------------------------------------------------------
library(dplyr)
library(tibble)
library(gt)

## -----------------------------------------------------------------------------
event_tb <- tribble(
  ~Population, ~"Number of Event in IA", ~"Number of Event in FA",
  "Population 1", 100, 200,
  "Population 2", 110, 220,
  "Overlap of Population 1 and 2", 80, 160,
  "Overall Population", 225, 450
)
event_tb %>%
  gt() %>%
  tab_header(title = "Number of events at each population")

## -----------------------------------------------------------------------------
event_tbl <- tribble(
  ~Population, ~"Number of Event in IA",
  "Population 1", 100,
  "Population 2", 110,
  "Overlap in population 1 and 2", 80
)
event_tbl %>%
  gt() %>%
  tab_header(title = "Number of events at each population in example 1")

## -----------------------------------------------------------------------------
Corr1 <- 80 / sqrt(100 * 110)
round(Corr1, 2)

## -----------------------------------------------------------------------------
event_tb2 <- tribble(
  ~Population, ~"Number of Event in IA", ~"Number of Event in FA",
  "Population 1", 100, 200
)
event_tb2 %>%
  gt() %>%
  tab_header(title = "Number of events at each analyses in example 2")

## -----------------------------------------------------------------------------
Corr1 <- 100 / sqrt(100 * 200)
round(Corr1, 2)

## -----------------------------------------------------------------------------
event_tb3 <- tribble(
  ~Population, ~"Number of Event in IA", ~"Number of Event in FA",
  "Population 1", 100, 200,
  "Population 2", 110, 220,
  "Overlap in population 1 and 2", 80, 160
)
event_tb3 %>%
  gt() %>%
  tab_header(title = "Number of events at each population & analyses in example 3")

## -----------------------------------------------------------------------------
Corr1 <- 80 / sqrt(100 * 220)
round(Corr1, 2)

## ----message=FALSE------------------------------------------------------------
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

event %>%
  gt() %>%
  tab_header(title = "Number of events at each population & analyses")

## -----------------------------------------------------------------------------
generate_corr(event)

