# The `pepr` package: Portable Encapsulated Projects in R

`pepr` is an R package for reading Portable Encapsulated Projects. Portable Encapsulated Projects (PEP) are sample-heavy projects that subscribe to the [standard PEP project definition](http://looper.readthedocs.io/en/latest/define-your-project.html). An example project formatted in this way is in the [microtest repository](https://github.com/epigen/microtest). If you describe your project (configuration and samples) according to this format, you can load all project metadata into R using the `pepr` package. These projects can also be used for any PEP-compatible pipeline (see [this list of compatible pipelines](https://github.com/pepkit/hello_looper/blob/master/looper_pipelines.md)).

`pepr` is currently in _alpha_ mode and should not be used production projects. It is made available for conceptual and testing purposes only.

## install

```R
devtools::install_github("pepkit/pepr")
```

## try it:

```R
library('pepr')
p = Project(file = "~/code/microtest/config/microtest_config.yaml")

samples(p)
config(p)

p@config
p@samples
```
