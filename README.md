[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/pepr)](http://cranlogs.r-pkg.org/badges/grand-total/pepr)
[![CRAN](http://www.r-pkg.org/badges/version-last-release/pepr)](http://www.r-pkg.org/badges/version-last-release/pepr)
![R-CMD-check](https://github.com/pepkit/pepr/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/pepkit/pepr/branch/master/graph/badge.svg)](https://codecov.io/gh/pepkit/pepr)
[![PEP compatible](http://pepkit.github.io/img/PEP-compatible-green.svg)](http://pepkit.github.io)

# The `pepr` package: Portable Encapsulated Projects in R

`pepr` is an R package for reading [Portable Encapsulated Projects](http://pep.databio.org/en/2.0.0/), or **PEP**s, in R. If you describe your project (configuration and samples) according to this format, you can load all project metadata into R using the `pepr` package. To test `pepr`, you can try loading one of the [example PEPs](https://github.com/pepkit/example_peps).

Complete documentation and API for the `pepr` R package is at [code.databio.org/pepr](http://code.databio.org/pepr/).


## Quick start:

Install from [CRAN](https://cran.rstudio.com/web/packages/pepr/index.html):

```R
install.packages("pepr")
```

Load a project and explore metadata like this:

```R
library("pepr")
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
