
[![Build Status](https://travis-ci.org/pepkit/pepr.svg?branch=master)](https://travis-ci.org/pepkit/pepr) 
[![PEP compatible](http://pepkit.github.io/img/PEP-compatible-green.svg)](http://pepkit.github.io)

`pepr` is not yet released, but we are planning to do so shortly. Complete documentation and API for the `pepr` R package is at [code.databio.org/pepr](http://code.databio.org/pepr/).

# The `pepr` package: Portable Encapsulated Projects in R

`pepr` is an R package for reading [Portable Encapsulated Projects](https://pepkit.github.io/), or **PEP**s, in R. If you describe your project (configuration and samples) according to this format, you can load all project metadata into R using the `pepr` package. To test `pepr`, you can try loading one of the [example PEPs](https://pepkit.github.io/docs/example_PEPs/).


## Quick start:

Install from GitHub:

```R
devtools::install_github("pepkit/pepr")
```

Load a project and explore metadata like this:

```R
library('pepr')
p = Project(file = "~/code/microtest/config/microtest_config.yaml")

samples(p)
config(p)
```
