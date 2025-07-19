
# vistool 0.2.1

* `as_visualizer(..., type = "surface")`, initializes a surface plot directly (similar to the other types).
* Split classification loss functions into probability and score-based losses; probability-based losses can display curves for either the positive, negative or both classes.


# vistool 0.2.0

* Switched to `ggplot2` for the visualization of loss functions, model predictions and objectives (all 2D, non-interactive); the `plotly` implementation remains intact for interactive surface visualizations (in 3D, `type = "surface"` in `as_visualizer()`). The `as_visualizer()` interface, `save` functions and various other things were reworked to match this.
* Includes new loss functions (e.g., pinball loss); loss functions can now take arguments (e.g., `quantile`) and be combined in one plot.
* Fixed numerous bugs for surface visualizations, expanded and corrected documentation.
* Further includes an extensive testing suite (see `tests/testthat`), a developer reference, and updated vignettes.


# vistool 0.1.0

* Remove `lim2002` optimizer as it's no longer exported by [`TestFunctions`](https://cran.r-project.org/package=TestFunctions).
* Add GPL3 license in line with e.g. `mlr3`.
* Start versioning.
