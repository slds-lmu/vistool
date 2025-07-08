
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vistool

<!-- badges: start -->

[![r-cmd-check](https://github.com/slds-lmu/vistool/actions/workflows/r-cmd-check.yaml/badge.svg)](https://github.com/slds-lmu/vistool/actions/workflows/r-cmd-check.yaml)
<!-- badges: end -->

The goal of `vistool` is to visualize optimization traces and aid in
teaching optimization-related concepts.

## Backend Selection with as_visualizer()

The `as_visualizer()` function provides intelligent automatic backend
selection between two visualization backends:

### ggplot2 Backend (Default)

- **When**: Used for `type = "1d"` and `type = "2d"` visualizations
- **Pros**: Fast rendering, publication-ready static plots, no
  additional dependencies
- **Best for**: Quick exploration, static reports, publications

### plotly Backend (Interactive)

- **When**: Used for `type = "surface"` visualizations (2D functions
  only)
- **Pros**: Interactive 3D surface plots, zoom/pan/rotate capabilities,
  hover information
- **Cons**: Requires Python dependencies for saving (see installation
  section)
- **Best for**: Interactive exploration, presentations, web applications

### Automatic Selection Rules

- **1D data**: Always uses ggplot2 (`Visualizer1D*`)
- **2D data**: Defaults to ggplot2 (`Visualizer2D*`), but you can choose
  plotly with `type = "surface"` (`VisualizerSurface*`)
- **Manual override**: Specify `type = "2d"` for ggplot2 or
  `type = "surface"` for plotly on 2D data

## Installation

You can install the development version of vistool from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("slds-lmu/vistool")
```

Please note that surface visualization features (plotly backend) rely on
[`plotly`](https://plotly.com/r/) which in turn relies on certain
functionality provided by Python packages, accessed via reticulate.

**For basic usage**: The plotly backend works out of the box for
interactive viewing.

**For saving plots**: The `$save()` functionality for surface plots uses
`plotly::save_image()` internally, which requires the `kaleido` Python
package. ggplot2 visualizations can be saved using standard ggplot2
methods without additional dependencies.

### Optional: Setup for Saving Plotly Plots

The following instructions are provided by `?plotly::save_image` and
assume you do not have `miniconda` installed already:

``` r
install.packages('reticulate')
reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')
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
task <- tsk("pima")
task <- po("imputemean")$train(list(task))[[1]]
task$select(c("insulin", "mass"))

# Select example learner
learner <- lrn("classif.svm", predict_type = "prob")

# Create 2D ggplot2 visualization (default for 2-feature tasks)
vis_2d <- as_visualizer(task, learner) # or explicitly: type = "2d"
vis_2d$plot()
```

<img src="man/figures/README-example-1.png" width="100%" />

For interactive exploration, you can create surface visualizations with
plotly:

``` r
# Create surface visualization for interactive plotly surface plot
vis_surface <- as_visualizer(task, learner, type = "surface")

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

For developers interested in contributing to `vistool`, please see the
[Developer Reference](DEVELOPMENT.md).

## Resources

For visualization of…

- [Loss
  Functions](https://slds-lmu.github.io/vistool/articles/loss_functions.html)
- [Model
  Predictions](https://slds-lmu.github.io/vistool/articles/model.html)
- [Objective
  Functions](https://slds-lmu.github.io/vistool/articles/objective.html)
