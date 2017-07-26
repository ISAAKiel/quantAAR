[![Travis-CI Build Status](https://travis-ci.org/ISAAKiel/quantAAR.svg?branch=master)](https://travis-ci.org/ISAAKiel/quantAAR) [![Coverage Status](https://img.shields.io/codecov/c/github/ISAAKiel/quantAAR/master.svg)](https://codecov.io/github/ISAAKiel/quantAAR?branch=master)

quantAAR
--------

R Library for Quantitative Analysis in Archaeology

#### Released version

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/quantAAR)](http://cran.r-project.org/package=quantAAR)

`quantAAR` is a collection of functions useful for archaeologists.

Just started...

Licence
-------

`quantAAR` is released under the [GNU General Public Licence, version 2](http://www.r-project.org/Licenses/GPL-2). Comments and feedback are welcome, as are code contributions.

Installation
------------

`quantAAR` is currently not on [CRAN](http://cran.r-project.org/), but you can use [devtools](http://cran.r-project.org/web/packages/devtools/index.html) to install the development version. To do so:

    if(!require('devtools')) install.packages('devtools')
    library(devtools)
    install_github('ISAAKiel/quantAAR')
    
To install with vignettes:

    install_github('ISAAKiel/quantAAR', build_vignettes = TRUE, force = TRUE)