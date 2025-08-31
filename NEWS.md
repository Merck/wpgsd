# wpgsd 0.2.0

## Major Features

### 🆕 S7 Class System
- **NEW**: Introduced modern S7 classes for enhanced type safety and validation
- **EventTable S7 Class**: Type-safe representation of event count data with automatic validation
  - Validates data structure, types, and mathematical constraints
  - Provides computed properties (`n_hypotheses`, `n_analyses`)
  - Professional formatted output with summary statistics
- **CorrelationMatrix S7 Class**: Enhanced correlation matrix objects with validation
  - Ensures mathematical properties (symmetry, positive definiteness)
  - Proper column ordering and labeling
  - Type-safe storage with metadata

### 🔧 Enhanced Functions
- **NEW**: `generate_corr_s7()` - S7-enhanced version of `generate_corr()`
  - Requires EventTable S7 input for type safety
  - Returns CorrelationMatrix S7 object with validation
  - Column ordering matches original `generate_corr()` (Analysis then Hypothesis)
  - Enhanced error messages and debugging support
- **NEW**: `new_event_table()` - Constructor for EventTable S7 objects
- **NEW**: `new_correlation_matrix()` - Constructor for CorrelationMatrix S7 objects
- **NEW**: `as_event_table()`, `as_correlation_matrix()` - Conversion utilities
- **NEW**: S7 subsetting methods: `subset_event_table()`, `subset_correlation_matrix()`

### 📊 Enhanced Correlation Computation
- **IMPROVED**: More robust correlation calculation with `compute_correlations()`
- **IMPROVED**: Enhanced input validation and error handling
- **NEW**: Comprehensive validation functions: `validate_event_table_data()`, `check_event_data()`

## Documentation Improvements

### 📚 Enhanced Vignettes
- **UPDATED**: "Correlated test statistics" vignette with S7 examples and author addition
- **IMPROVED**: Mathematical typesetting and formatting in "Quickstart guide"
- **NEW**: Comprehensive S7 class examples and comparisons

### 🌐 Better Organization
- **IMPROVED**: Reorganized function reference with logical groupings:
  - S7 Classes (Enhanced Interface)
  - Core WPGSD Functions
  - Event Table Generation
  - Internal/Advanced Functions
  - Utilities and Helpers
- **IMPROVED**: Enhanced discoverability of new S7 functionality

## Backward Compatibility
- **MAINTAINED**: All existing functions continue to work unchanged
- **MAINTAINED**: Existing workflows remain fully supported
- **ENHANCED**: Optional migration path to S7 classes for improved robustness

## Testing and Quality
- **ENHANCED**: Comprehensive test suite with 105+ tests covering all functionality
- **NEW**: Dedicated S7 class test suites ensuring type safety and validation
- **IMPROVED**: Mathematical validation and edge case testing

## Installation
Install the latest version from GitHub:

```r
remotes::install_github("Merck/wpgsd@v0.2.0")
```

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
