# wpgsd 0.2.0

## Major Features

- **S7 Class System Integration**: Complete implementation of S7 classes for enhanced type safety and validation
  - `EventTable` class with robust validation for event data structures
  - `CorrelationMatrix` class with symmetry and positive definiteness validation
  - Improved `generate_corr()` function with S7 class support

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
