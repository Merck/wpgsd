pkgname <- "wpgsd"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('wpgsd')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("calc_seq_p")
### * calc_seq_p

flush(stderr()); flush(stdout())

### Name: calc_seq_p
### Title: Calculate sequential p-values for interaction/elementary
###   hypothesis
### Aliases: calc_seq_p

### ** Examples




cleanEx()
nameEx("closed_test")
### * closed_test

flush(stderr()); flush(stdout())

### Name: closed_test
### Title: Perform closed testing procedure
### Aliases: closed_test

### ** Examples

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



cleanEx()
nameEx("find_astar")
### * find_astar

flush(stderr()); flush(stdout())

### Name: find_astar
### Title: Utility function for root-finding to compute crossing
###   probabilities with the overall alpha spending approach
### Aliases: find_astar

### ** Examples

# Input event count of intersection of paired hypotheses - Table 2
my_event <- tibble::tribble(
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
my_corr <- generate_corr(my_event)

# Find the inflation factor for H1, H2 at analysis 1
find_astar(
  a = 0.0008708433,
  alpha_prev = NULL,
  aprime = c(0.0004588644, 0.0004119789),
  astar = 1,
  w = c(0.5, 0.5),
  sig = my_corr[
    colnames(my_corr) %in% c("H1_A1", "H2_A1"),
    colnames(my_corr) %in% c("H1_A1", "H2_A1")
  ]
)



cleanEx()
nameEx("find_xi")
### * find_xi

flush(stderr()); flush(stdout())

### Name: find_xi
### Title: Utility function for root-finding to compute inflation factor xi
###   with the separate alpha spending approach
### Aliases: find_xi

### ** Examples

# Input event count of intersection of paired hypotheses - Table 2
my_event <- tibble::tribble(
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
my_corr <- generate_corr(my_event)

# Find the inflation factor for H1, H2 at analysis 1
find_xi(
  a = 0.0008708433,
  alpha_prev = NULL,
  aprime = c(0.0004588644, 0.0004119789),
  xi = 1,
  sig = my_corr[
    colnames(my_corr) %in% c("H1_A1", "H2_A1"),
    colnames(my_corr) %in% c("H1_A1", "H2_A1")
  ]
)



cleanEx()
nameEx("generate_bounds")
### * generate_bounds

flush(stderr()); flush(stdout())

### Name: generate_bounds
### Title: Compute p-value boundaries of the parametric MTP method with
###   overall alpha spending for all hypotheses
### Aliases: generate_bounds

### ** Examples

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



cleanEx()
nameEx("generate_corr")
### * generate_corr

flush(stderr()); flush(stdout())

### Name: generate_corr
### Title: Generate correlation matrix based on event counts
### Aliases: generate_corr

### ** Examples

# Build the transition matrix
m <- matrix(c(
  0, 0.5, 0.5,
  0.5, 0, 0.5,
  0.5, 0.5, 0
), nrow = 3, byrow = TRUE)
# initialize weights
w <- c(1 / 3, 1 / 3, 1 / 3)

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



cleanEx()
nameEx("generate_event_table")
### * generate_event_table

flush(stderr()); flush(stdout())

### Name: generate_event_table
### Title: Generate table of event counts from ADSL and ADTTE datasets
### Aliases: generate_event_table

### ** Examples

library(dplyr)

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



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
