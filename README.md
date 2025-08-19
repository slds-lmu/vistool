
<!-- README.md is generated from README.Rmd. Please edit this file -->

# vistool

<!-- badges: start -->

[![r-cmd-check](https://github.com/slds-lmu/vistool/actions/workflows/r-cmd-check.yaml/badge.svg)](https://github.com/slds-lmu/vistool/actions/workflows/r-cmd-check.yaml)
<!-- badges: end -->

The goal of `vistool` is to visualize optimization traces and aid in
teaching optimization-related concepts.

## Installation

You can install the development version of vistool from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("slds-lmu/vistool")
```

Please note that surface visualization features (`plotly` backend) rely
on [`plotly`](https://plotly.com/r/) which in turn relies on certain
functionality provided by Python packages, accessed via
[`reticulate`](https://rstudio.github.io/reticulate/).

### Optional: Setup for Saving Plotly Plots

The `$save()` functionality for surface plots uses `plotly::save_image()`
internally, which in turn relies on the Python package `kaleido`.

`vistool` (via `reticulate >= 1.41`) declares this requirement on load
using `py_require("kaleido")`. The first time Python is needed, an
ephemeral, cached Python environment is provisioned automatically with
`kaleido`—no manual Miniconda setup required for typical users.

If you prefer to manage Python yourself, ensure `kaleido` is installed
in the Python environment that `reticulate` uses. Example:

``` r
install.packages("reticulate")
# OPTIONAL: point reticulate to a specific python before loading vistool
# Sys.setenv(RETICULATE_PYTHON = "/path/to/python")
reticulate::py_install("kaleido")
```

## Example

``` r
library(vistool)
library(mlr3verse)
#> Loading required package: mlr3
```

This example shows how to visualize the prediction surface of an SVM on
the `pima` task included in `mlr3`:

``` r
# Create an example task, add missing data imputation and select 2 features
task = tsk("pima")
task = po("imputemean")$train(list(task))[[1]]
task$select(c("insulin", "mass"))

# Select example learner
learner = lrn("classif.svm", predict_type = "prob")

# Create 2D ggplot2 visualization
vis_2d = as_visualizer(task, learner = learner)
vis_2d$plot()
```

<img src="man/figures/README-example-1.png" width="100%" />

For interactive exploration, you can create surface visualizations with
`plotly`:

``` r
# Create surface visualization for interactive plotly surface plot
vis_surface = as_visualizer(task, learner = learner, type = "surface")

# Define a 3D scene
vis_surface$set_scene(x = 1.4, y = 1.4, z = 1.4)

# View interactively
vis_surface$plot()
```

![](man/figures/demo_1.png)

Save static version as png:

``` r
# only works if Python kaleido package is installed
# see installation instructions above for setting up plotly save functionality
vis_surface$save("man/figures/demo_1.png", width = 500, height = 500)
```

## Contributing

For anyone interested in contributing to `vistool`, please see the
[Developer Reference](DEVELOPMENT.md).

## Resources

For visualization of…

- [Loss
  Functions](https://slds-lmu.github.io/vistool/articles/loss_functions.html)
- [Model
  Predictions](https://slds-lmu.github.io/vistool/articles/model.html)
- [Objective
  Functions](https://slds-lmu.github.io/vistool/articles/objective.html)
