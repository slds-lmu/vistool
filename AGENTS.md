# Agent instructions for vistool

## Project snapshot
- `as_visualizer()` (`R/as_visualizer.R`) picks an R6 visualizer based on the input type (mlr3 task + learner, `Objective`, `LossFunction`) and dimensionality.
- ggplot2 backends (`VisualizerModel`, `VisualizerObj`, `VisualizerLossFuns`) render static 1D/2D plots; plotly backends (`VisualizerSurface*`) cover interactive 2D surfaces only.

## Core architecture
- `R/Visualizer.R` owns theme resolution, layer storage via `private$store_layer()`, auto color assignment, and `save()` caching.
- Child classes must follow deferred rendering: call `super$plot(...)`, render stored layers with private helpers, then `self$resolve_layer_colors()`; ggplot classes cache the returned plot in `private$.last_plot`, plotly classes mutate `private$.plot`.
- Loss functions stay 1D; objectives/tasks are 1D or 2D; `type = "surface"` is valid only for 2D objectives/models.

## Layer system
- Add methods (`add_points`, `add_boundary`, `add_optimization_trace`, etc.) enqueue layer specs; retrieve them with `private$get_layers_by_type()` inside dedicated `private$render_*` helpers.
- Use `color = "auto"` to pull from the active palette; always call `self$resolve_layer_colors()` after layers are rendered.

## Theme & styling
- Theme precedence: layer overrides > `plot(theme = ...)` > `set_theme()` > `options(vistool.theme = ...)` default.
- Build themes with `vistool_theme()` (`R/theme.R`) and helpers like `get_vistool_color()` / `get_continuous_colorscale()`.

## Developer workflow
- Standard loop: `devtools::load_all()`, edit, `devtools::test()`; run `devtools::check()` before release/PRs.
- Refresh docs with `devtools::document()` and `devtools::build_readme()`; vignettes in `vignettes/` compile via `devtools::build_vignettes()`.
- Tests live in `tests/testthat/`; plotly/Python-dependent cases skip on CI using `skip_on_ci()` or `skip_if_not_installed("plotly")`.

## Integration notes
- Surface visualizations depend on `plotly` + `reticulate`; `$save()` calls `plotly::save_image()` and expects Python `kaleido` (auto via `py_require("kaleido")`, but document manual setup if interacting with reticulate envs).
- mlr3 tasks/learners supply model data; objectives and losses are defined under `R/Objective*.R` and `R/LossFunction*.R`.

## Conventions
- R6 generators keep CamelCase filenames (`VisualizerSurfaceObj.R`); functions/fields remain snake_case.
- Constructors accept computational knobs (limits, padding, grid size); styling belongs in themes or explicit layer args.
- Reuse roxygen templates from `man-roxygen/`; `NAMESPACE` updates flow from `devtools::document()`.
- Titles, headings and subheadings should use __Sentence case__.

## When extending
- Mirror the layer storage/render split for new layer types and add support for both ggplot and plotly variants when applicable.
- Update dispatch logic in `as_visualizer()` if new visualizer combinations are introduced; ensure dimension checks stay intact.
- Add regression tests alongside existing `tests/testthat/test-plot-*` files, asserting layer properties rather than rendered pixels.
