
<!-- README.md is generated from README.Rmd. Please edit that file -->
tbltools
========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tbltools)](https://cran.r-project.org/package=tbltools) [![packageversion](https://img.shields.io/badge/Package%20version-0.4.7.9999-orange.svg?style=flat-square)](/commits) [![Last-changedate](https://img.shields.io/badge/last%20change-2018--02--23-yellowgreen.svg)](/commits) [![Build Status](https://travis-ci.org/KopfLab/tbltools.svg?branch=master)](https://travis-ci.org/KopfLab/tbltools) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/KopfLab/tbltools?branch=master&svg=true)](https://ci.appveyor.com/project/KopfLab/tbltools)

About
-----

The [tbltools](https://kopflab.github.io/tbltools/) package is a collection of tools for team-based-learning (TBL) style flipped classrooms.

Installation
------------

You can install [tbltools](https://kopflab.github.io/tbltools/) from github with the devtools package. If you do not already have many of the dependencies installed, this may take a moment.

``` r
# install.packages("devtools") 
devtools::install_github("KopfLab/tbltools")
```

Usage
-----

### iRAT/tRAT generation

The [tbltools](https://kopflab.github.io/tbltools/) package provides functionality to easily generate Readiness Assessment Tests from a collection of questions and answers and any IF-AT answer key. For an example of how to use this functionality, see the vignette on [Generating a Readiness Assessment Test](https://kopflab.github.io/tbltools/articles/readiness_assessment_tests.html).

### Peer Evaluations

The [tbltools](https://kopflab.github.io/tbltools/) package also provides functionality to easily set up a peer evaluation server based on a student roster and team assignments and then evaluate the collected data. For an example of how to use this functionality, see the vignette on [Peer Evaluations](https://kopflab.github.io/tbltools/articles/peer_evaluations.html).
