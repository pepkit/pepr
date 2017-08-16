# pepr

R package for read Portable Encapsulated Projects (under construction).

The PEP format is defined here: http://looper.readthedocs.io/en/latest/define-your-project.html

If you describe your project (configuration and samples) according to this format, you can load all project metadata into R using the `pepr` package.

An example project formatted in this way is in the [microtest repository](https://github.com/epigen/microtest).

These projects can also be used for any PEP-compatible pipeline (see [this list of compatible pipelines](https://github.com/pepkit/hello_looper/blob/master/looper_pipelines.md)).

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
