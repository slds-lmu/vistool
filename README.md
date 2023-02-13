
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Vistool

## Usage

``` r
devtools::load_all()

# Get objective to define the "problem":
obj = tfun_dict$get("TF_banana")

# Define optimizers based on the objective and conduct 1000 steps:
oo1 = OptimizerGD$new(obj, x_start = c(0.6, 0.6), step_size = 0.001, id = "GD x0=(0.6, 0.6)")
oo1$optimize(steps = 1000)

oo2 = OptimizerGD$new(obj, x_start = c(0, 0), step_size = 0.001, id = "GD x0=(0, 0)")
oo2$optimize(steps = 1000)

oo3 = OptimizerGD$new(obj, x_start = c(0.4, 1), step_size = 0.001, id = "GD x0=(0.4, 1)")
oo3$optimize(steps = 1000)

oo4 = OptimizerMomentum$new(obj, x_start = c(0, 0), step_size = 0.001)
oo4$optimize(steps = 1000)

oo5 = OptimizerNAG$new(obj, x_start = c(0, 0), step_size = 0.001)
oo5$optimize(steps = 1000)

# Optimization traces are stored in the archive:
head(oo5$archive)

# The Visualizer class just requires the object and sets everything such as
# limits automatically (if not you will get a message):
viz = Visualizer$new(obj)

# The two potential visualizations are surface and contour plots. The "base"
# layer is defined by `$initLayerContour()` or `$initLayerSurface()`:
viz$initLayerSurface()

# Creating the figure is done with `$plot()`:
viz$plot()
```

![](Readme_files/fig1.png)

``` r
# To quickly add the optimization traces use `$addLayerOptimizationTrace(opt)`:
viz$addLayerOptimizationTrace(oo1) # The color is randomly generated if not defined
viz$addLayerOptimizationTrace(oo2, line_color = "rgb(46,139,87)")
viz$addLayerOptimizationTrace(oo3, line_color = "rgb(220,20,60)")
viz$addLayerOptimizationTrace(oo4)
viz$addLayerOptimizationTrace(oo5)

viz$plot()
```

![](Readme_files/fig2.png)

``` r
# If traces overlap, we can add an offset to slightly shift them:
viz$initLayerSurface()

viz$addLayerOptimizationTrace(oo1)
viz$addLayerOptimizationTrace(oo2, line_color = "rgb(46,139,87)", offset = c(-0.01, -0.01, -0.01))
viz$addLayerOptimizationTrace(oo3, line_color = "rgb(220,20,60)")
viz$addLayerOptimizationTrace(oo4, offset = c(0.01, 0.01, -0.01))
viz$addLayerOptimizationTrace(oo5, offset = c(0.01, 0.01, 0.01))

viz$plot()
```

![](Readme_files/fig3.png)

``` r
# Setting the scene and layout can be done with `$setScene()` and `setLayout()`:
viz$setScene(1, -2, 1)
viz$setLayout(legend = list(
  orientation = "h",   # show entries horizontally
  xanchor = "center",  # use center of legend as anchor
  x = 0.5))            # legend = list()

viz$plot()

# Save a plot can also easily be done with `save()`
viz$save("myfigure.png")
```

![](Readme_files/fig4.png)

``` r
# Create animations with `$animate()` (Note: Just the frames are stored,
# you have to put them together by yourself):
viz$animate(nframes = 24L, view_start = list(x = 1, y = -2, z = 1),
  view_end = list(x = 0.5, y = -2, z = 1))
# Use, e.g., ImageMagick: `convert -delay 20 -loop 0 animation/*.png mygif.gif`
```

![](Readme_files/gif1.gif)

## Example: Linear Model loss and optimization

``` r
# Define the linear model loss function:
mylm = function(x, Xmat, y) {
  l2norm(Xmat %*% x - y)
}

# Use the iris dataset with response `Sepal.Width` and feature `Petal.Width`:
Xmat = model.matrix(~ Petal.Width, data = iris)
y = iris$Sepal.Width

# Create a new object:
obj_lm = Objective$new(id = "iris LM", fun = mylm, xdim = 2,  Xmat = Xmat, y = y, minimize = TRUE)

# Optimize with two different start points:
oo1 = OptimizerGD$new(obj_lm, x_start = c(0, -0.05), step_size = 0.001)
oo1$optimize(steps = 100)

oo2 = OptimizerMomentum$new(obj_lm, x_start = c(-0.05, 0), step_size = 0.001)
oo2$optimize(steps = 100)

oo3 = OptimizerNAG$new(obj_lm, x_start = c(0, 0), step_size = 0.001)
oo3$optimize(steps = 100)

# Visualize, this time with contour lines and custom limits:
viz = Visualizer$new(obj_lm, x1limits = c(-0.5, 5), x2limits = c(-3.2, 2.8))

viz$initLayerContour()

viz$addLayerOptimizationTrace(oo1)
viz$addLayerOptimizationTrace(oo2)
viz$addLayerOptimizationTrace(oo3)

viz$setLayout(legend = list(orientation = "h", xanchor = "center", x = 0.5))
viz$plot()

viz$animate(nframes = 24L)
# Use, e.g., ImageMagick: `convert -delay 10 -loop 0 animation/*.png mygif.gif`
```

![](Readme_files/fig_lm.png) ![](Readme_files/gif2.gif)

## All 2D Objectives

#### GoldsteinPrice

![](Readme_files/GoldsteinPrice-gg.png)

#### GoldsteinPriceLog

![](Readme_files/GoldsteinPriceLog-gg.png)

#### ackley

![](Readme_files/ackley-gg.png)

#### banana

![](Readme_files/banana-gg.png)

#### beale

![](Readme_files/beale-gg.png)

#### borehole

![](Readme_files/borehole-gg.png)

#### branin

![](Readme_files/branin-gg.png)

#### currin1991

![](Readme_files/currin1991-gg.png)

#### easom

![](Readme_files/easom-gg.png)

#### franke

![](Readme_files/franke-gg.png)

#### hump

![](Readme_files/hump-gg.png)

#### lim2002

![](Readme_files/lim2002-gg.png)

#### quad_peaks

![](Readme_files/quad_peaks-gg.png)

#### quad_peaks_slant

![](Readme_files/quad_peaks_slant-gg.png)

#### sinumoid

![](Readme_files/sinumoid-gg.png)

#### waterfall

![](Readme_files/waterfall-gg.png)

#### zhou1998

![](Readme_files/zhou1998-gg.png)
