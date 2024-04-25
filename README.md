# wpgsd <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/Merck/wpgsd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/wpgsd/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Merck/wpgsd/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/wpgsd?branch=main)
<!-- badges: end -->

Weighted parametric group sequential design (WPGSD) allows one to take advantage
of the known correlation structure in constructing efficacy bounds to control
family-wise error rate (FWER) for a group sequential design. Here correlation
may be due to common observations in nested populations, due to common
observations in overlapping populations, or due to common observations
in the control arm.

## Installation

The easiest way to get wpgsd is to install from CRAN:

```r
install.packages("wpgsd")
```

Alternatively, to use a new feature or get a bug fix,
you can install the development version of wpgsd from GitHub:

```r
# install.packages("remotes")
remotes::install_github("Merck/wpgsd")
```

## Current limitations

There are some limitations that are currently being addressed.
Please use the package with caution in production environments.

- The current implementation may have limitations when handling more
  complex scenarios beyond what has been demonstrated.
- The API is subject to potential breaking changes as it is currently
  being reviewed and refactored.
- Further validation is needed to ensure the reliability of the package.
- More documentation is expected in the future releases.

## References

Anderson, K. M., Guo, Z., Zhao, J., & Sun, L. Z. (2022).
A unified framework for weighted parametric group sequential design.
_Biometrical Journal_, 64(7), 1219--1239.

BibTeX entry:

```
@article{anderson2022unified,
  title     = {A unified framework for weighted parametric group sequential design},
  author    = {Anderson, Keaven M and Guo, Zifang and Zhao, Jing and Sun, Linda Z},
  journal   = {Biometrical Journal},
  volume    = {64},
  number    = {7},
  pages     = {1219--1239},
  year      = {2022},
  publisher = {Wiley Online Library}
}
```
