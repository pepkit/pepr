# example_peps

This repository contains examples of [Portable Encapsulated Projects](http://pepkit.github.io). Explore the examples interactively with `python` or `R`:


## Python

Your basic python workflow uses the [peppy](http://github.com/pepkit/peppy) package and starts out like this:

```{python}
import peppy
proj1 = peppy.Project("example1/project_config.yaml")
```

These Jupyter notebooks show you how to explore these examples interactively in python. You can view the rendered notebooks right here:

* [Basic peppy tutorial](tutorial.ipynb) - use `peppy` to load up a minimal example PEP.
* [Sample subannotation](subannotation.ipynb) - how to use subannotations.

If you want to run them interactively with `jupyter`, just start it on the command line like this:

```
jupyter notebook subannotation.ipynb
```



## R

Your basic `R` workflow uses the [pepr](http://github.com/pepkit/pepr) package and starts like this:

```{r}
library('pepr')
p = pepr::Project("example1/project_config.yaml")
```

More detailed R vignettes are available as part of the [documentation for the pepr package](http://code.databio.org/pepr).

## Looper

These projects can also be run through any command-line tool (such as a pipeline) using [looper](https://github.com/pepkit/looper). To see a complete example of a PEP and a looper-compatible pipeline, visit the [hello looper repository](https://github.com/pepkit/hello_looper).
