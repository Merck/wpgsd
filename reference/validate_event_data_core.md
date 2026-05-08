# Core Event Data Validation Function

Shared validation logic for event data across different validation
contexts. Supports three validation levels with increasing strictness.

## Usage

``` r
validate_event_data_core(
  data,
  validation_level = c("basic", "strict", "s7"),
  return_errors = FALSE
)
```

## Arguments

- data:

  A data.frame or tibble to validate

- validation_level:

  Character string specifying validation level:

  - "basic": Basic structure and type validation

  - "strict": Strict validation for correlation computation

  - "s7": Full validation for S7 EventTable objects

- return_errors:

  Logical; if TRUE, returns character vector of errors instead of
  stopping on first error

## Value

If return_errors=FALSE: invisible(TRUE) on success, stops on error. If
return_errors=TRUE: NULL on success, character vector of errors on
failure.

## Details

**Basic level:**

- Required columns (H1, H2, Analysis, Event) present

- All columns are numeric

- Hypothesis indices (H1, H2) are positive

- Analysis numbers are positive

- Event counts are non-negative

**Strict level (includes basic plus):**

- H1 \<= H2 for all rows (correlation computation requirement)

- Unique combinations of H1, H2, Analysis

- Sequential hypothesis and analysis indices starting from 1

- Multiple analyses required

- Diagonal entries exist for all off-diagonal entries

**S7 level (includes strict plus):**

- Event counts non-decreasing across analyses for fixed H1, H2

- Diagonal entries have Event \>= corresponding off-diagonal entries

- Allows H1 \> H2 (more flexible than strict)
