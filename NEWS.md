# wpgsd 0.3.0

## New Features

- **Dunnett-type dose-finding vignette**: Added new vignette demonstrating Dunnett-type group sequential design with 2 experimental arms vs. common control and 3 analyses (2 interims + final), exercising correlation computation for K > 2.

## Improvements

- Completed S7 wrapper function elimination and validation refactoring.
- Organized help files into logical sections for better usability.
- Removed empty `adj-seq-p-simplified.Rmd` vignette that caused UNKNOWN TITLE in pkgdown navigation.

## Bug Fixes

- Fixed `generate_corr()` correlation computation for K > 2 analyses: within-hypothesis and between-hypotheses loops now correctly enumerate all analysis pairs.
- Fixed S7 `EventTable` validator to allow single-analysis and single-hypothesis event tables (e.g., after subsetting).
- Fixed `validate_event_data_core()` crash when non-numeric data is passed to `floor()` checks.
- Fixed inconsistent error message formatting in diagonal entry validation (`paste` → `paste0`).
- Corrected test expectations to match actual validation error messages.

# wpgsd 0.2.0

## Major Features

- **S7 Class System Integration**: Complete implementation of S7 classes for enhanced type safety and validation
  - `EventTable` class with robust validation for event data structures
  - `CorrelationMatrix` class with symmetry and positive definiteness validation
  - Improved `generate_corr()` function with S7 class support

## Code Quality Improvements

### Validation System Refactoring
- **Eliminated ~80% code duplication** between `check_event_data()`, `validate_event_table_data()`, and `EventTable` validator method through centralized `validate_event_data_core()` function
- **Improved validation consistency** with three validation levels: "basic", "strict", and "s7" to support different use cases
- **Enhanced error handling** with clearer, more specific error messages for validation failures
- **Relaxed Event value requirements** to allow non-integer values (e.g., 100.5 events) while maintaining H1, H2, and Analysis as positive integers

### S7 Class Implementation Improvements  
- **Removed redundant wrapper functions** `new_event_table()` and `new_correlation_matrix()` following S7 best practices
- **Enhanced S7 class documentation** with comprehensive parameter descriptions, validation details, and usage examples
- **Improved API consistency** by using direct S7 class constructors (`EventTable()`, `CorrelationMatrix()`) throughout codebase
- **Updated all examples and tests** to use proper S7 constructor patterns instead of wrapper functions

### Testing and Documentation Updates
- **Updated test suite** to reflect validation changes and S7 constructor usage
- **Regenerated package documentation** with roxygen2 to remove deprecated wrapper function documentation
- **Enhanced code maintainability** through consolidated validation logic and cleaner S7 implementation

## Bug Fixes and Improvements

- Fixed correlation matrix validation tolerance issues (improved numerical precision handling with 1e-12 tolerance)
- Resolved non-ASCII character issues in documentation for better portability
- Added missing `@export` tags for proper function exports
- Enhanced roxygen2 documentation with clearer parameter descriptions

## Documentation and Vignettes

- **Significantly improved `adj-seq-p` vignette**:
  - Reduced code repetition by ~80% through systematic helper functions
  - Enhanced readability while maintaining technical accuracy
  - Added comprehensive multiplicity strategy visualization
  - Improved mathematical notation and explanations
  - Added proper citations (@zhao2025adjusted)
- Updated correlation calculation vignette with S7 class examples

## Package Infrastructure

- Enhanced unit tests with 132+ passing test cases
- Improved package build process with better error handling
- Updated NAMESPACE with proper exports
- Enhanced pkgdown documentation site generation

# wpgsd 0.1.0

- Initial release.

The wpgsd package is now available on GitHub, install it with

```
remotes::install_github("Merck/wpgsd")
```

If you prefer to use a specific version, install it with

```
remotes::install_github("Merck/wpgsd@v0.1.0")
```

where `v0.1.0` is the GitHub release version number.
