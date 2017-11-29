# The `pepr` package: Portable Encapsulated Projects in R

`pepr` is an R package for reading Portable Encapsulated Projects. Portable Encapsulated Projects (PEP) are sample-heavy datasets that subscribe to the [standard PEP metadata definition](https://pepkit.github.io/docs/home/) (see some [examples of PEPs](https://pepkit.github.io/docs/example_PEPs/)). If you describe your project (configuration and samples) according to this format, you can load all project metadata into R using the `pepr` package. These projects can also be used for any PEP-compatible pipeline (see [this list of compatible pipelines](https://github.com/pepkit/hello_looper/blob/master/looper_pipelines.md)).

`pepr` is currently in _alpha_ mode and should not be used production projects. It is made available for conceptual and testing purposes only.

## Documentation

Complete documentation and vignettes can be found at http://code.databio.org/pepr/.

## Install

```R
devtools::install_github("pepkit/pepr")
```

## Quick start:

```R
library('pepr')
p = Project(file = "~/code/microtest/config/microtest_config.yaml")

samples(p)
config(p)

p@config
p@samples
```

