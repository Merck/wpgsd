# wpgsd

<!-- badges: start -->
[![R-CMD-check](https://github.com/Merck/wpgsd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/wpgsd/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Merck/wpgsd/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/wpgsd?branch=main)
<!-- badges: end -->

## Installation

Install the development version of wpgsd from GitHub:

```r
remotes::install_github("Merck/wpgsd")
```

## Objective

Weighted parametric group sequential design (WPGSD) allows one to take advantage
of the known correlation structure in constructing efficacy bounds to control
family-wise error rate (FWER) for a group sequential design. Here correlation
may be due to common observations in nested populations, due to common
observations in overlapping populations, or due to common observations
in the control arm.

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
