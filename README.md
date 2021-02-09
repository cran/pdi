# pdi

[![R build status](https://github.com/jasenfinch/pdi/workflows/R-CMD-check/badge.svg)](https://github.com/jasenfinch/pdi/actions)
[![Codecov test coverage](https://codecov.io/gh/jasenfinch/pdi/branch/master/graph/badge.svg)](https://codecov.io/gh/jasenfinch/pdi?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/pdi)](https://CRAN.R-project.org/package=pdi)
[![downloads](https://cranlogs.r-pkg.org/badges/pdi)](https://cran.r-project.org/package=pdi)
[![DOI](https://zenodo.org/badge/201299021.svg)](https://zenodo.org/badge/latestdoi/201299021)

> **Phenotypic index measures for oak decline severity**

Oak declines are complex disease syndromes that consist of many visual indicators that include aspects of tree size, crown condition and trunk condition. This can cause difficulty in the manual classification of symptomatic and non-symptomatic trees from what is in reality a broad spectrum of oak tree health condition.

Two oak decline indexes have been developed to quantitatively describe and differentiate oak decline syndromes in *Quercus robur*. These include:

**Phenotypic Decline Index (PDI)** - a measure of overall decline severity, scoring trees between 0 and 1. More severely declining oak trees having a score closer to 1.

**Decline Acuteness Index (DAI)** - a measure to differentiate between Chronic Oak Decline (COD) and Acute Oak Decline with a score between -1 and 1. Acutely declining trees having a score closer to 1 and chronically declining trees having a score closer to -1.

This package provides the tools generate these decline indexes using machine learning algorithm random forest.

The methodology is outlined in the article:

[Finch, J.P., Brown, N., Beckmann, M., Denman, S. and Draper, J., 2021. Index measures for oak decline severity using phenotypic descriptors. Forest Ecology and Management, 485, p.118948.](https://doi.org/10.1016/j.foreco.2021.118948)

### Installation

The package can be installed from CRAN using:

``` r
install.packages('pdi')
```

Or this development version of the package can be installed from GitHub:

``` r
devtools::install_github('jasenfinch/pdi',build_vignettes = TRUE)
```

### Learn more

The package documentation can be browsed online at <https://jasenfinch.github.io/pdi/>. 

If this is your first time using `pdi` see the [Generating phenotypic decline indexes using the pdi package](https://jasenfinch.github.io/pdi/articles/pdi-example.html) vignette for information on how to get started.

If you believe you've found a bug in `pdi`, please file a bug (and, if
possible, a [reproducible example](https://reprex.tidyverse.org)) at
<https://github.com/jasenfinch/pdi/issues>.
