[![Travis-CI Build Status](https://travis-ci.org/ISAAKiel/quantAAR.svg?branch=master)](https://travis-ci.org/ISAAKiel/quantAAR) [![Coverage Status](https://img.shields.io/codecov/c/github/ISAAKiel/quantAAR/master.svg)](https://codecov.io/github/ISAAKiel/quantAAR?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/quantAAR)](http://cran.r-project.org/package=quantAAR)

# quantAAR

`quantAAR` contains tidy wrappers and useful utility function for common applications of exploratory statistics in archaeology:

**Wrapper functions:**

- `tidyca` does **Correspondence Analysis** via `ca::ca`
- `tidypca` does **Principal Components Analysis** via `stats::prcomp`
- `tidyseriation` reorders data with **Seriation** via `seriation::seriate`

**Utility functions:**

- `itremove` iteratively removes all rows and columns of a dataframe with less than a given number of non zero elements
- `booleanize` reduces the numeric values of a dataframe to boolean presence-absence indicators

**Data:**

- `matuskovo`: A dataset containing graves (objects) and grave attributes (variables) of a burial site from the Unetice culture in western Slovakia

## Installation

`quantAAR` is currently not on [CRAN](http://cran.r-project.org/), but you can use [devtools](http://cran.r-project.org/web/packages/devtools/index.html) to install the development version. To do so:

    if(!require('devtools')) install.packages('devtools')
    devtools::install_github('ISAAKiel/quantAAR')

## Licence

`quantAAR` is released under the [GNU General Public Licence, version 2](http://www.r-project.org/Licenses/GPL-2). Comments and feedback are welcome, as are code contributions.
