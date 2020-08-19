# pdi

[![R build status](https://github.com/jasenfinch/pdi/workflows/R-CMD-check/badge.svg)](https://github.com/jasenfinch/pdi/actions)
[![Codecov test coverage](https://codecov.io/gh/jasenfinch/pdi/branch/master/graph/badge.svg)](https://codecov.io/gh/jasenfinch/pdi?branch=master)

> **Phenotypic index measures for oak decline severity**

Oak declines are complex disease syndromes that consist of many visual indicators that include aspects of tree size, crown condition and trunk condition. This can cause difficulty in the manual classification of symptomatic and non-symptomatic trees from what is in reality a broad spectrum of oak tree health condition.

Two oak decline indexes have been developed to quantitatively describe and differentiate oak decline syndromes in *Quercus robur*. These include:

**Phenotypic Decline Index (PDI)** - a measure of overall decline severity, scoring trees between 0 and 1. More severely declining oak trees having a score closer to 1.

**Decline Acuteness Index (DAI)** - a measure to differentiate between Chronic Oak Decline (COD) and Acute Oak Decline with a score between -1 and 1. Acutely declining trees having a score closer to 1 and chronically declining trees having a score closer to -1.

This package provides the tools generate these decline indexes using machine learning algorithm random forest.

Install the package from GitHub:

```
devtools::install_github('jasenfinch/pdi',build_vignettes = TRUE)
```

See the vignette for how to generate a phenotypic decline index (PDI) for oak decline severity using example data:

```
vignette('pdi-example',package = 'pdi')
```
