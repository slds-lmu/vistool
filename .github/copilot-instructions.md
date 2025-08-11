# Copilot Coding Agent Instructions for vistool

Goal: Help contributors work productively in this R package for visualizing optimization traces. Keep changes consistent with R6 patterns, rendering pipeline, and theme system.

Big picture
- Entry point: `as_visualizer()` (R/as_visualizer.R) selects the R6 visualizer class based on input and dimensionality.
- Backends: ggplot2 for 1D/2D (`VisualizerModel`, `VisualizerObj`, `VisualizerLossFuns`), plotly for interactive 2D surfaces (`VisualizerSurface*`). Loss functions are 1D only; “surface” is for 2D Models/Objectives only.
- Data sources: mlr3 Tasks/Learners for model plots; custom `Objective` and `LossFunction` classes for function plots.

Architecture essentials (files under R/)
- Base: `Visualizer` (R/Visualizer.R) handles theme resolution, layer storage, color assignment, caching, and save() for ggplot/plotly.
- ggplot2 classes: `VisualizerModel.R`, `VisualizerObj.R`, `VisualizerLossFuns.R`.
- plotly classes: `VisualizerSurface.R`, `VisualizerSurfaceModel.R`, `VisualizerSurfaceObj.R`.
- Selection rules (R/as_visualizer.R): Tasks need `learner`; LossFunction supports only `1d`; Objectives/Tasks must be 1D or 2D; `type = "surface"` only valid for 2D Models/Objectives.

Deferred rendering pattern (do this in every plot implementation)
- Always call `super$plot(...)` first to resolve effective theme and store render args.
- Render class-specific layers, then call `self$resolve_layer_colors()`.
- ggplot2: build and return a ggplot object; set `private$.last_plot` for save().
- plotly: modify `private$.plot` and return it; `save()` uses plotly image export.

Theme and styling rules
- Precedence: layer-specific > plot(theme = ...) override > instance `set_theme()` > package default (`options(vistool.theme = ...)`).
- Use `color = "auto"` to draw from the active palette; `merge_theme()` and `get_pkg_theme_default()` in R/theme.R control defaults.

Layer system and add_* methods
- Add methods store specs (not draw immediately). Example: `Visualizer$add_points(...)` stores a "points" layer; rendering happens in private helpers for ggplot/plotly.
- When adding new layers: (1) store via a single `private$store_layer(type, spec)`, (2) implement type-specific render helpers for ggplot and/or plotly, (3) respect theme and auto-colors.

Developer workflows (R/devtools)
- Load/install/test: `devtools::load_all()`, `devtools::install_deps(dependencies = TRUE)`, `devtools::test()`, `devtools::check()`.
- Docs/site: `devtools::document()`, `devtools::build_readme()`, `pkgdown::build_site()`.
- Tests live in `tests/testthat/`; skip plotly-dependent saves on CI when Python/kaleido is missing.

Integrations and deps
- plotly + reticulate: `save()` for surface plots uses `plotly::save_image()`; requires Python `kaleido` (see README.md for setup).
- mlr3 for Tasks/Learners; checkmate for argument validation; roxygen2 with templates in `man-roxygen/`.

Conventions
- R6: instantiate with `$new()`, copy with `$clone()`.
- Naming: suffixes `Model`, `Obj`, `LossFuns`, and `Surface` indicate specialization and backend.
- Keep constructors computational (limits, grids, padding); styling belongs in `set_theme()`, `plot(theme=...)`, or layer args.

Examples (common patterns)
- `as_visualizer(task, learner)$plot()` for 2D ggplot; `as_visualizer(task, learner, type = "surface")$plot()` for interactive surface; `as_visualizer(obj, type = "1d")$plot()` for 1D objectives; `as_visualizer(list(loss1, loss2))$plot()` for losses.

CI/CD
- Workflows in `.github/workflows/` run R CMD check and pkgdown. Tests that need plotly/Python are skipped if deps are absent.

Unsure about a pattern? Check `DEVELOPMENT.md` for architecture and examples, or inspect the corresponding class in R/ for a reference implementation.
