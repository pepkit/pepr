
[![Build Status](https://travis-ci.org/pepkit/pepr.svg?branch=master)](https://travis-ci.org/pepkit/pepr)
[![Coverage Status](https://coveralls.io/repos/github/pepkit/pepr/badge.svg?branch=dev)](https://coveralls.io/github/pepkit/pepr?branch=dev&service=github)
[![PEP compatible](http://pepkit.github.io/img/PEP-compatible-green.svg)](http://pepkit.github.io)

`pepr` is not yet released, but has been submitted to CRAN and should be available for a standard installation shortly. Complete documentation and API for the `pepr` R package is at [code.databio.org/pepr](http://code.databio.org/pepr/).

# The `pepr` package: Portable Encapsulated Projects in R

`pepr` is an R package for reading [Portable Encapsulated Projects](http://pep.databio.org/en/2.0.0/), or **PEP**s, in R. If you describe your project (configuration and samples) according to this format, you can load all project metadata into R using the `pepr` package. To test `pepr`, you can try loading one of the [example PEPs](https://github.com/pepkit/example_peps).


## Quick start:

Install from GitHub:

```R
devtools::install_github("pepkit/pepr")
```

Load a project and explore metadata like this:

```R
library("pepr"")
cfgPath = system.file(
    "extdata",
    paste0("example_peps-master"),
    "example_basic",
    "project_config.yaml",
    package = "pepr"
  )
p = Project(file = cfgPath)

sampleTable(p)
config(p)
```
