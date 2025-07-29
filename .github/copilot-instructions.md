# Copilot Coding Agent Instructions for vistool

## Project Overview
- **vistool** is an R package for visualizing optimization traces and teaching optimization concepts.
- Core visualizations: 1D/2D (ggplot2), 2D surface (plotly, interactive), and loss/model/objective visualizations.
- Main entry point: `as_visualizer()` (see `R/as_visualizer.R`) auto-selects the appropriate visualizer class based on input.
- Core workflow: (1) Initialize with `as_visualizer()`, (2) add layers using `add_*` methods, (3) `plot()`; optionally `save()` or `animate()`

## Architecture & Key Components
- **R6 OOP**: All visualizers and core objects (e.g., `Visualizer`, `Visualizer1D`, `Visualizer2D`, `VisualizerSurface`, `LossFunction`, `Objective`, `Optimizer`) are R6 classes in `R/`.
- **Visualizer classes**:
  - `Visualizer1D*`, `Visualizer2D*`: ggplot2-based
  - `VisualizerSurface*`: plotly-based, for interactive 3D/2D
  - Suffixes `Model`, `Obj`, `LossFuns` indicate specialization for models, objectives, or loss functions
- **Customization options**: Global settings that affect the entire plot belong in `plot()`. Settings for specific visual elements belong in the `add_*()` methods that create them. Defaults are set (at initialization) in `as_visualizer()`.
- **Templates**: Common roxygen2 doc blocks are in `man-roxygen/` and referenced via `@template`.
- **Tests**: All public R6 methods and S3 generics should have corresponding tests in `tests/testthat/`.

## Developer Workflow
- Use `devtools` for all development tasks:
  - `devtools::load_all()` — load package for development
  - `devtools::install_deps(dependencies = TRUE)` — install dependencies
  - `devtools::test()` — run all tests
  - `devtools::check()` — full package check
  - `devtools::document()` — update documentation
  - `pkgdown::build_site()` — build/preview documentation website
- For plotly `$save()` support, ensure Python `kaleido` is installed via `reticulate` (see README for setup).

## Project Conventions
- **R6 classes**: Always use `new()` for instantiation, `clone()` for copying (inherited from R6, not user-defined).
- **Documentation**: Use roxygen2 with templates for consistency. All exported functions/classes must be documented with examples.
- **Naming**: Suffixes (`1D`, `2D`, `Surface`, `Model`, `Obj`, `LossFuns`) indicate type and specialization.
- **Visualization selection**: Use `as_visualizer()` for user-facing API; do not instantiate visualizer classes directly in user code.
- **Surface plots**: Use plotly for interactive 3D/2D; saving requires Python `kaleido`.

## Integration & External Dependencies
- **plotly**: For interactive surface plots; requires Python `kaleido` for saving images.
- **reticulate**: Used to bridge R and Python for plotly image export.
- **mlr3**: Used in examples and for model-based visualizations.
- **checkmate**: Used for argument validation throughout the codebase.

## Examples
- See `README.Rmd` and vignettes in `vignettes/` for usage patterns and best practices.
- Example: `as_visualizer(task, learner, type = "surface")` creates an interactive surface visualizer for a 2D task.

## CI/CD
- GitHub Actions workflows in `.github/workflows/` run R CMD check and tests on push/PR.
- Tests requiring Python/plotly are skipped on CI if dependencies are missing.

---
If any conventions or workflows are unclear, please consult `DEVELOPMENT.md` or ask for clarification.
