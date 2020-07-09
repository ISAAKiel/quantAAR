[![Project Status: Suspended – Initial development has started, but there has not yet been a stable, usable release; work has been stopped for the time being but the author(s) intend on resuming work.](https://www.repostatus.org/badges/latest/suspended.svg)](https://www.repostatus.org/#suspended)
[![Travis-CI Build Status](https://travis-ci.org/ISAAKiel/quantAAR.svg?branch=master)](https://travis-ci.org/ISAAKiel/quantAAR) [![Coverage Status](https://img.shields.io/codecov/c/github/ISAAKiel/quantAAR/master.svg)](https://codecov.io/github/ISAAKiel/quantAAR?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/quantAAR)](http://cran.r-project.org/package=quantAAR)

# quantAAR

`quantAAR` contains tidy wrappers and useful utility function for common applications of exploratory statistics in archaeology:

**Wrapper functions:**

- *Correspondence Analysis*: `ca.ca_ca` (*`ca::ca`*), `ca.vegan_cca` (*`vegan::cca`*)
- *Principal Components Analysis*: `pca.stats_prcomp` (*`stats::prcomp`*), `pca.vegan_rda` (*`vegan::rda`*)
- *Seriation*: `seriation.seriation_seriate` (*`seriation::seriate`*), `seriation.tabula_seriate` (*`tabula::seriate`*)
- *Principal Curves*: `principal_curve.princurve_principal_curve` (*`princurve::principal_curve`*)

**Data preparation functions:**

- `itremove` iteratively removes all rows and columns of a dataframe with less than a given number of non zero elements
- `booleanize` reduces the numeric values of a dataframe to boolean presence-absence indicators

**Data:**

- `matuskovo` + `matuskovo_material`: A dataset containing graves (objects) and grave attributes (variables) of a burial site from the Unetice culture in western Slovakia

### Installation

`quantAAR` is currently not on [CRAN](http://cran.r-project.org/), but you can use [devtools](http://cran.r-project.org/web/packages/devtools/index.html) to install the development version. To do so:

    if(!require('devtools')) install.packages('devtools')
    devtools::install_github('ISAAKiel/quantAAR')

### Licence

`quantAAR` is released under the [GNU General Public Licence, version 2](http://www.r-project.org/Licenses/GPL-2). Comments and feedback are welcome, as are code contributions.
