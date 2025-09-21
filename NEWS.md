# vistool 0.4.1
* `add_annotation`
* `AGENTS.md`

# vistool 0.4.0
* New `Hypothesis` R6 class and `hypothesis()` constructor: wrap user-defined functions for regression or classification (1D/2D), with optional link and domain.
* `as_visualizer()` now accepts a `hypothesis` argument for `Task` objects (mutually exclusive with `learner`), and supports direct visualization of `Hypothesis` objects (standalone, with required `domain`).
* Domain parameter (`domain`) allows specifying plotting limits for hypothesis-only visualizations.
* `as_visualizer(retrain = FALSE)` allows reusing existing fitted models.
* `lss()` now accepts `mlr3` measure keys (e.g., `regr.huber`).
* For 1D regression tasks, `add_points()` allows specifying a loss geometry for L1 (residuals) and L2 (squares) losses.
* Objective plotting: Plot limits now default to the objective's canonical bounds if present. If no bounds are available, explicit `x1_limits`/`x2_limits` are required. If user-specified limits exceed the canonical bounds, the plot is still drawn and a warning is issued.
* Automatic declaration of Python `kaleido` dependency via `reticulate::py_require()` (reticulate >= 1.41) to support saving surface plots without manual Miniconda setup.
* New theme system.

# vistool 0.3.2

* Added options to specify color and shape for training data of classification tasks.
* Added layers are now rendered in the order they were added.
* Various fixes.

# vistool 0.3.1

* All classes now use the deferred rendering architecture.
* Implemented `add_points()` for loss functions.
* Minor fixes.

# vistool 0.3.0

* Polished the general workflow and unified all customization options: 1. Initialize the Visualizer object (`as_visualizer()`), 2. Layering (`add_*`) methods, 3. Rendering (`plot()`); 2. and 3. can be chained (e.g., `vis$add_*$add_*$plot()`). Global settings that affect the entire plot are set in `plot()`. Settings for specific visual elements are set in the `add_*()` methods that create them. Defaults can be set at initialization in `as_visualizer()`.
* Layers are now only applied when calling `plot()` (instead of directly), improving predictability and allowing for greater customization.
* `as_visualizer(..., type = "surface")`, initializes a surface plot directly (similar to the other types); `as_visualizer` can now be used on lists of loss functions (as expected).
* Split classification loss functions into probability and score-based losses; probability-based losses can display curves for either the positive, negative or both classes.
* Reworked boundary functionality (`add_boundary()`): Now accepts vectors for multiple boundaries; works for 1D model visualizations (horizontal lines at y-values).
* `add_points()` for adding (ordered) points; with the possibility of annotating them for all `Visualizer` types.
* Unified custom contour functionality (surface plots) in separate `add_contours()` method.
* Split objective vignette into `objective_functions`, `optimization_traces` and `advanced_visualization`; swapped out the `banana` objective for `GoldsteinPriceLog` and `boston_housing` for `california_housing`.
* Fixed bugs (optimization & traces).


# vistool 0.2.0

* Switched to `ggplot2` for the visualization of loss functions, model predictions and objectives (all 2D, non-interactive); the `plotly` implementation remains intact for interactive surface visualizations (in 3D, `type = "surface"` in `as_visualizer()`). The `as_visualizer()` interface, `save` functions and various other things were reworked to match this.
* Includes new loss functions (e.g., pinball loss); loss functions can now take arguments (e.g., `quantile`) and be combined in one plot.
* Fixed numerous bugs for surface visualizations, expanded and corrected documentation.
* Further includes an extensive testing suite (see `tests/testthat`), a developer reference, and updated vignettes.


# vistool 0.1.0

* Remove `lim2002` optimizer as it's no longer exported by [`TestFunctions`](https://cran.r-project.org/package=TestFunctions).
* Add GPL3 license in line with e.g. `mlr3`.
* Start versioning.
