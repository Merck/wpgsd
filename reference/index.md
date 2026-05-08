# Package index

## S7 Classes and Constructors

Core S7 data structures for event tables and correlation matrices

- [`CorrelationMatrix()`](https://merck.github.io/wpgsd/reference/CorrelationMatrix.md)
  : CorrelationMatrix S7 Class
- [`EventTable()`](https://merck.github.io/wpgsd/reference/EventTable.md)
  : EventTable S7 Class
- [`as_correlation_matrix()`](https://merck.github.io/wpgsd/reference/as_correlation_matrix.md)
  : Convert matrix to CorrelationMatrix object
- [`as_event_table()`](https://merck.github.io/wpgsd/reference/as_event_table.md)
  : Convert Data to EventTable
- [`print.CorrelationMatrix`](https://merck.github.io/wpgsd/reference/print.CorrelationMatrix.md)
  : Print method for CorrelationMatrix

## Event Table Generation

Functions for generating event count tables from clinical trial data

- [`generate_event_table()`](https://merck.github.io/wpgsd/reference/generate_event_table.md)
  : Generate table of event counts from ADSL and ADTTE datasets
- [`generate_event_table_()`](https://merck.github.io/wpgsd/reference/generate_event_table_.md)
  : This function creates a table summarizing event counts based on
  specified hypotheses and user input data. It can handle two types of
  analysis: one comparing experimental groups to a common control and
  another analyzing the overlap of populations.
- [`generate_event_table_cc()`](https://merck.github.io/wpgsd/reference/generate_event_table_cc.md)
  : This function generates a table of events for given experimental
  arms and a control group based on specified hypotheses.
- [`generate_event_table_ol()`](https://merck.github.io/wpgsd/reference/generate_event_table_ol.md)
  : This function generates a table of events for specified populations
  based on the provided hypotheses.

## Correlation Matrix Computation

Functions for computing correlation matrices from event data

- [`generate_corr()`](https://merck.github.io/wpgsd/reference/generate_corr.md)
  : Generate correlation matrix based on event counts
- [`generate_corr_s7()`](https://merck.github.io/wpgsd/reference/generate_corr_s7.md)
  : Generate S7 CorrelationMatrix from EventTable or event data
- [`compute_correlations()`](https://merck.github.io/wpgsd/reference/compute_correlations.md)
  : Compute correlations from event data
- [`gen_corr()`](https://merck.github.io/wpgsd/reference/gen_corr.md) :
  Convert correlation data frame to correlation matrix

## Statistical Testing and Analysis

Core statistical procedures for group sequential designs

- [`calc_seq_p()`](https://merck.github.io/wpgsd/reference/calc_seq_p.md)
  : Calculate sequential p-values for interaction/elementary hypothesis
- [`closed_test()`](https://merck.github.io/wpgsd/reference/closed_test.md)
  : Perform closed testing procedure
- [`generate_bounds()`](https://merck.github.io/wpgsd/reference/generate_bounds.md)
  : Compute p-value boundaries of the parametric MTP method with overall
  alpha spending for all hypotheses

## Data Manipulation and Subsetting

Functions for working with event tables and correlation matrices

- [`subset_event_table()`](https://merck.github.io/wpgsd/reference/subset_event_table.md)
  : Subset EventTable by Analysis or Hypotheses
- [`subset_correlation_matrix()`](https://merck.github.io/wpgsd/reference/subset_correlation_matrix.md)
  : Extract correlation submatrix

## Data Validation

Functions for validating and checking data integrity

- [`check_event_data()`](https://merck.github.io/wpgsd/reference/check_event_data.md)
  : Check event data for correlation computation
- [`validate_event_data_core()`](https://merck.github.io/wpgsd/reference/validate_event_data_core.md)
  : Core Event Data Validation Function
- [`validate_event_table_data()`](https://merck.github.io/wpgsd/reference/validate_event_table_data.md)
  : Validate EventTable Data Format

## Utility Functions

Internal utility functions for numerical computations

- [`find_astar()`](https://merck.github.io/wpgsd/reference/find_astar.md)
  : Utility function for root-finding to compute crossing probabilities
  with the overall alpha spending approach
- [`find_xi()`](https://merck.github.io/wpgsd/reference/find_xi.md) :
  Utility function for root-finding to compute inflation factor xi with
  the separate alpha spending approach

## Package Information

Package documentation and tidy evaluation helpers

- [`wpgsd`](https://merck.github.io/wpgsd/reference/wpgsd-package.md)
  [`wpgsd-package`](https://merck.github.io/wpgsd/reference/wpgsd-package.md)
  : wpgsd: Weighted Parametric Group Sequential Design
- [`tidyeval`](https://merck.github.io/wpgsd/reference/tidyeval.md)
  [`enquo`](https://merck.github.io/wpgsd/reference/tidyeval.md)
  [`enquos`](https://merck.github.io/wpgsd/reference/tidyeval.md)
  [`.data`](https://merck.github.io/wpgsd/reference/tidyeval.md)
  [`:=`](https://merck.github.io/wpgsd/reference/tidyeval.md)
  [`as_name`](https://merck.github.io/wpgsd/reference/tidyeval.md)
  [`as_label`](https://merck.github.io/wpgsd/reference/tidyeval.md) :
  Tidy eval helpers
