## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE,
  fig.align = "center"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(wpgsd)
library(gsDesign)
library(gMCPLite)
library(haven)
library(dplyr)
library(tidyr)
library(reshape2)
library(gt)

## ----eval=FALSE, echo=FALSE---------------------------------------------------
# library(ggplot2)
# library(ggforce)
# 
# ellipse_data <- data.frame(
#   id = c("H1: Population 1", "H2: Population 2", "H3: Overall Population"),
#   x = c(0, 1, 0.5),
#   y = c(0, 0, 0),
#   a = c(1, 1, 2),
#   b = c(0.5, 0.5, 1)
# )
# 
# ragg::agg_png(
#   "vignettes/figures/ex1-pop.png",
#   width = 1920,
#   height = 1920 / 1.618,
#   res = 96,
#   scaling = 2
# )
# ggplot() +
#   geom_ellipse(
#     aes(
#       x0 = x, y0 = y, a = a, b = b, angle = 0, fill = id
#     ),
#     data = ellipse_data[ellipse_data$id == "H3: Overall Population", ],
#     color = NA, alpha = 0.5, show.legend = FALSE
#   ) +
#   geom_ellipse(
#     aes(
#       x0 = x, y0 = y, a = a, b = b, angle = 0, fill = id
#     ),
#     data = ellipse_data[ellipse_data$id != "H3: Overall Population", ],
#     color = NA, alpha = 0.85, show.legend = FALSE
#   ) +
#   geom_text(aes(
#     x = x, y = y, label = id,
#     hjust = ifelse(id == "H1: Population 1", 1.1,
#       ifelse(id == "H2: Population 2", -0.1, 0.5)
#     ),
#     vjust = ifelse(id == "H3: Overall Population", -9, 0.5)
#   ), data = ellipse_data, size = 6) +
#   scale_fill_manual(values = c("#E69F00", "#56B4E9", "#999999"), guide = "none") +
#   coord_fixed() +
#   theme_void()
# dev.off()

## ----echo=FALSE, out.width="90%", fig.cap="Ex1: Populations"------------------
knitr::include_graphics("figures/ex1-pop.png")

## ----out.width="80%"----------------------------------------------------------
# Transition matrix
m <- matrix(c(
  0, 0, 1,
  0, 0, 1,
  0.5, 0.5, 0
), nrow = 3, byrow = TRUE)
# Weight matrix
w <- c(0.3, 0.3, 0.4)

# Multiplicity graph
cbPalette <- c("#999999", "#E69F00", "#56B4E9")

nameHypotheses <- c(
  "H1: Population 1",
  "H2: Population 2",
  "H3: Overall Population"
)

hplot <- hGraph(3,
  alphaHypotheses = w,
  m = m,
  nameHypotheses = nameHypotheses,
  trhw = .2, trhh = .1,
  digits = 5, trdigits = 3, size = 5, halfWid = 1,
  halfHgt = 0.5, offset = 0.2, trprop = 0.4,
  fill = as.factor(c(2, 3, 1)),
  palette = cbPalette,
  wchar = "w"
)
hplot

## ----out.width="80%", echo = FALSE--------------------------------------------
cbPalette <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)
nameHypotheses <- c(
  "H1: Experimental 1 vs Control",
  "H2: Experimental 2 vs Control",
  "H3: Experimental 3 vs Control"
)
m <- matrix(c(
  0, 0.5, 0.5,
  0.5, 0, 0.5,
  0.5, 0.5, 0
), nrow = 3, byrow = TRUE)
alphaHypotheses <- c(1 / 3, 1 / 3, 1 / 3)

hplot <- hGraph(3,
  alphaHypotheses = alphaHypotheses, m = m,
  nameHypotheses = nameHypotheses, trhw = .2, trhh = .1,
  digits = 3, trdigits = 4, size = 5, halfWid = 1.2, halfHgt = 0.5,
  offset = 0.2, trprop = 0.35,
  fill = as.factor(c(2, 3, 1)),
  palette = cbPalette[1:3],
  wchar = "w"
)
hplot

## -----------------------------------------------------------------------------
event <- tribble(
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
  tab_header(title = "Event Count")

## -----------------------------------------------------------------------------
# Alternatively, one can manually enter paths for analysis datasets,
# example below uses an example dataset assuming currently we are at IA1.
paths <- system.file("extdata/", package = "wpgsd")

### Generate event count table from ADSL and ADTTE datasets
# Selection criteria for each hypothesis
h_select <- tribble(
  ~Hypothesis, ~Crit,
  1, "PARAMCD=='OS' & TRT01P %in% c('Xanomeline High Dose', 'Placebo')",
  2, "PARAMCD=='OS' & TRT01P %in% c('Xanomeline Low Dose', 'Placebo')"
)

event2 <- generate_event_table(paths, h_select,
  adsl_name = "adsl", adtte_name = "adtte",
  key_var = "USUBJID", cnsr_var = "CNSR"
)$event

event2 %>%
  gt() %>%
  tab_header(title = "Event Count - Computed from SAS Datasets Example")

## -----------------------------------------------------------------------------
## Generate correlation from events
corr <- generate_corr(event)

corr %>%
  as_tibble() %>%
  gt() %>%
  fmt_number(columns = everything(), decimals = 2) %>%
  tab_header(title = "Correlation Matrix")

## -----------------------------------------------------------------------------
# Bonferroni bounds
bound_Bonf <- generate_bounds(
  type = 0, k = 2, w = w, m = m,
  corr = corr, alpha = 0.025,
  sf = list(sfHSD, sfHSD, sfHSD),
  sfparm = list(-4, -4, -4),
  t = list(c(0.5, 1), c(0.5, 1), c(0.5, 1))
)

bound_Bonf %>%
  gt() %>%
  fmt_number(columns = 3:5, decimals = 4) %>%
  tab_header(title = "Bonferroni bounds")

## -----------------------------------------------------------------------------
set.seed(1234)
# WPGSD bounds, spending approach 1
bound_WPGSD <- generate_bounds(
  type = 2, k = 2, w = w, m = m,
  corr = corr, alpha = 0.025,
  sf = sfHSD,
  sfparm = -4,
  t = c(min(100 / 200, 110 / 220, 225 / 450), 1)
)

bound_WPGSD %>%
  gt() %>%
  fmt_number(columns = 3:5, decimals = 4) %>%
  tab_header(title = "WPGSD bounds")

## ----echo=FALSE---------------------------------------------------------------
# Combine and back-calculate xi
bounds <- left_join(bound_Bonf, bound_WPGSD,
  by = c("Hypotheses", "Analysis"),
  suffix = c(".B", ".W")
)

bounds <- bounds %>%
  rowwise() %>%
  mutate(xi = sum(H1.W, H2.W, H3.W, na.rm = TRUE) /
    sum(H1.B, H2.B, H3.B, na.rm = TRUE))

# Reorder for output
bounds$order <- rep(c(5, 2, 1, 3, 6, 4, 7), 2)
bounds <- bounds %>%
  arrange(Analysis, order) %>%
  select(-order)

# Bonferroni and WPGSD Bounds (Table 6 in the manuscript)
bounds %>%
  gt() %>%
  fmt_number(columns = 3:9, decimals = 4) %>%
  tab_header(title = "Bonferroni and WPGSD Bounds")

## -----------------------------------------------------------------------------
## Observed p-values.
## The tibble must contain columns Analysis, H1, H2 etc for all hypotheses
p_obs <- tribble(
  ~Analysis, ~H1, ~H2, ~H3,
  1, 0.01, 0.0004, 0.03,
  2, 0.05, 0.002, 0.015
)

## Closed testing ##
test_result <- closed_test(bound_WPGSD, p_obs)

p_obs %>%
  gt() %>%
  fmt_number(columns = 2:4, decimals = 8, drop_trailing_zeros = TRUE) %>%
  tab_header("Observed Nominal p-Values")

## -----------------------------------------------------------------------------
test_result %>%
  gt() %>%
  tab_header(title = "Closed Testing Results")

## -----------------------------------------------------------------------------
set.seed(1234)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Ex2 BH ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Transition matrix in Figure A2
m <- matrix(c(
  0, 0.5, 0.5,
  0.5, 0, 0.5,
  0.5, 0.5, 0
), nrow = 3, byrow = TRUE)
# Initial weights
w <- c(1 / 3, 1 / 3, 1 / 3)

# Event count of intersection of paired hypotheses - Table 2
event <- tribble(
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

event %>%
  gt() %>%
  tab_header(title = "Event Count")

## -----------------------------------------------------------------------------
# Generate correlation from events
corr <- generate_corr(event)

# Correlation matrix in Table 4
corr %>%
  as_tibble() %>%
  gt() %>%
  fmt_number(columns = everything(), decimals = 2) %>%
  tab_header(title = "Correlation Matrix")

## -----------------------------------------------------------------------------
# WPGSD bounds, spending method 3c
bound_WPGSD <- generate_bounds(
  type = 3, k = 2, w = w, m = m, corr = corr, alpha = 0.025,
  sf = list(sfLDOF, sfLDOF, sfLDOF),
  sfparm = list(0, 0, 0),
  t = list(c(155 / 305, 1), c(160 / 320, 1), c(165 / 335, 1))
)

# Bonferroni bounds
bound_Bonf <- generate_bounds(
  type = 0, k = 2, w = w, m = m, corr = corr, alpha = 0.025,
  sf = list(sfLDOF, sfLDOF, sfLDOF),
  sfparm = list(0, 0, 0),
  t = list(c(155 / 305, 1), c(160 / 320, 1), c(165 / 335, 1))
)

bounds <- left_join(bound_Bonf, bound_WPGSD,
  by = c("Hypotheses", "Analysis"),
  suffix = c(".B", ".W")
)

# Reorder for output
bounds$order <- rep(c(5, 2, 1, 3, 6, 4, 7), 2)
bounds <- bounds %>%
  arrange(Analysis, order) %>%
  select(-order)

# Table A6
bounds %>%
  gt() %>%
  fmt_number(columns = 3:9, decimals = 4) %>%
  tab_header(title = "Bonferroni and WPGSD Bounds")

