# pepr

R package for read Portable Encapsulated Projects (under construction).


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
