# vistool visualizer refactor plan: What vs. How

This document is the implementation blueprint to: (1) keep constructors/as_visualizer() strictly about what to plot, (2) move how to plot (styling) into plot() and per-layer add_*() methods, and (3) resolve styling once per render from a single effective theme with clear precedence.

The goal is minimal churn with a clean, composable API that preserves the deferred rendering design.


## Principles (to lock in)

- Separation of concerns
  - What to plot: domain objects and computational parameters (task, learner, losses/objective, limits/bounds, padding, n_points, grids).
  - How to plot: palette, text sizes, theme, alpha/opacity, widths, point sizes, legend position, grid styling.
- Single source of truth for styling
  - Styling is resolved once per render from an effective theme with precedence:
    Layer explicit style > plot(theme = …) > instance theme > package defaults.
- Deferred rendering remains
  - Keep storing unresolved layer specs. Auto colors and other defaults are resolved at plot() so palette/theme changes propagate without rewriting layers.


## Target API shape

- Constructors and as_visualizer(): only “what to plot”
  - Accept only objects and computational parameters: tasks/learners, objectives/losses, limits/bounds, padding, n_points, sampling grids.
  - Remove all style args (default_color_palette, default_text_size, default_theme, default_alpha, default_line_width, default_point_size, opacity, colorscale, show_title).
- Instance theme: set_theme(theme)
  - Expose a small, typed theme object (see Theme model below) and a setter that merges with package defaults.
  - Optionally a getter: theme().
  - Remove initialize_defaults() and any defaults field; replace with private$.theme (set_theme) and private$.effective_theme (computed in plot()).
- plot() only takes styling as overrides
  - plot(theme = NULL, …labels/limits…); theme is a partial list that overrides the instance theme for this render only.
  - Continue to accept plot labels/limits and legend toggles in plot() (functional parameters). All appearance knobs should be inside theme, but we’ll keep labels/limits in plot() for clarity and minimal churn.
- add_*() layer methods: local style, defaulting to auto/NULL
  - Keep style args on add_*() (color/alpha/size/shape/linetype/linewidth/etc.).
  - Use color = "auto" and other style args = NULL by default. Resolve at plot() using effective theme unless explicitly set.
- save() saves the last rendered plot
  - If available, save the last rendered plot (private$.last_plot). Otherwise, render with current instance theme (plot() with no overrides) and save.
  - Optionally add render() to build and store the plot without saving.


## Theme model

- Function: vistool_theme(
    palette = "viridis",
    text_size = 11,
    theme = "minimal",            # ggplot-like theme id
    alpha = 0.8,
    line_width = 1.2,
    point_size = 2,
    legend_position = "right",
    show_grid = TRUE,
    grid_color = "gray90",
    background = "white"           # ggplot background fill
  ) -> named list

- Validation helper: assert_vistool_theme(theme)
- Merge helper: merge_theme(base, override) using mlr3misc::insert_named
- Package default: options(vistool.theme = vistool_theme()) set in .onLoad if not already present
- Effective theme resolution (at plot start):
  - effective_theme = merge_theme(package_default, self$theme())
  - if plot(theme = override) provided: effective_theme = merge_theme(effective_theme, override)


## Cross-cutting implementation steps

1) Add theme utilities
- New file R/theme.R with:
  - vistool_theme(), assert_vistool_theme(), merge_theme(), get_pkg_theme_default(), set_pkg_theme_default()
- zzz.R: .onLoad sets options(vistool.theme) if unset; export vistool_theme

2) Modify base Visualizer
- Remove initialize_defaults() and public$field defaults
- Add public methods:
  - set_theme(theme), theme() getter
- plot():
  - New signature: plot(theme = NULL, plot_title = NULL, plot_subtitle = NULL, x_lab = NULL, y_lab = NULL, z_lab = NULL, x_limits = NULL, y_limits = NULL, z_limits = NULL, show_legend = TRUE, show_title = TRUE, legend_position = c("right","top","bottom","left","none"))
  - At start: resolve private$.effective_theme from package default, instance theme, and theme override
  - Store non-style render params (labels/limits/toggles) in private$.render_params
- Color resolution:
  - get_auto_color_with_palette() uses private$.effective_theme$palette
  - resolve_layer_colors() remains, but read theme from private$.effective_theme
- add_points_to_ggplot/add_points_to_plotly:
  - If size/alpha are NULL, fill from effective theme (point_size/alpha)
- apply_ggplot_theme/apply_ggplot_color_scale:
  - Accept effective theme; compute title_size = text_size + 2 if needed
- save(filename, …):
  - If private$.last_plot exists, save that (respecting backend); else build and save
  - Optionally allow save(rebuild = TRUE) if needed later (out of scope for minimal churn)
- Private state:
  - .theme (instance-level), .effective_theme (per-render), .render_params (labels/limits/toggles), .last_plot (last built plot), .plot (current in-progress plot)

3) Remove style args from constructors and as_visualizer
- as_visualizer.*: drop default_* args; do not call initialize_defaults(); pass only computational params to constructors
- VisualizerLossFuns$new: drop default_* args
- VisualizerSurface(Model|Obj)$initialize: drop opacity/colorscale/show_title from args; these are style -> resolved from theme/plot

4) Update each Visualizer to use effective theme
- Replace private$.plot_settings usages with private$.effective_theme for style, and private$.render_params for labels/limits/toggles
- Ensure every plot() stores the returned plot as private$.last_plot and returns it
- Ensure “auto” continues to resolve late based on effective theme palette

5) Keep deferred architecture
- Do not resolve layer colors/styles on add_*(); keep “auto”/NULL until plot()
- Maintain private$.layers_to_add, .color_index; reset color index at each plot()

6) Precedence rules verification
- During rendering:
  - If a layer spec sets an explicit value (e.g., color != "auto", alpha not NULL) -> use it
  - Else fallback to private$.effective_theme for defaults
  - For plotly surfaces, colorscale = get_continuous_colorscale(effective_theme$palette)


## Per-file change list (minimal diffs)

- R/theme.R (new)
  - Implement vistool_theme(), assert_vistool_theme(), merge_theme()
  - Export vistool_theme in NAMESPACE

- R/zzz.R
  - .onLoad: options(vistool.theme = vistool_theme()) if NULL
  - Ensure import/export lines OK (no functional change to imports)

- R/color_management.R
  - No breaking changes; keep get_vistool_color(), get_continuous_colorscale()
  - Optionally add small helpers to map ggplot theme ids if needed (not required)

- R/Visualizer.R
  - Remove public$field defaults and initialize_defaults()
  - Add public: set_theme(theme), theme() getter
  - plot(): new signature with theme = NULL override; compute private$.effective_theme and store private$.render_params; reset .color_index; DO NOT build plot here (subclasses do); return invisible(self) or keep returning self for chain, but subclasses should return plot object and set private$.last_plot
  - resolve_layer_colors(): unchanged, but get_auto_color_with_palette() uses effective_theme$palette
  - save(): use private$.last_plot if present; otherwise render and store, then save
  - add_points_to_ggplot()/add_points_to_plotly(): if size/alpha NULL, fill from effective theme
  - apply_ggplot_theme(): use effective theme values (text_size, background, show_grid, grid_color)
  - apply_ggplot_color_scale(): use effective theme palette

- R/as_visualizer.R
  - Remove default_* args from generics and S3 methods
  - Stop calling initialize_defaults()
  - Only pass computational params to constructors (limits, padding, n_points, learner/task/objective, etc.)

- R/VisualizerModel.R
  - No constructor style args; unchanged validate/compute grid
  - In init_1d_plot()/init_2d_plot():
    - Use eff = private$.effective_theme and rp = private$.render_params
    - ggplot: use geom_raster alpha = eff$alpha; color scales from eff$palette
    - theme application via apply_ggplot_theme with eff values
    - labels/limits from rp
  - plot(): call super$plot(theme = …) to set state, then build, resolve colors, set private$.last_plot, return plot

- R/VisualizerObj.R
  - Same treatment as VisualizerModel.R
  - 1D line width: use eff$line_width if appropriate (optional now; can remain fixed and revise later)

- R/VisualizerSurface.R
  - Remove opacity/colorscale/show_title from initialize signature and fields; keep plot_lab/x1_lab/x2_lab/z_lab
  - init_layer_contour()/init_layer_surface(): take opacity from eff$alpha; colorscale from get_continuous_colorscale(eff$palette); layout text sizes from eff$text_size; show_title from rp$show_title
  - plot(flatten = FALSE, layout = NULL, scene = NULL, theme = NULL, …labels/limits…): resolve effective theme via super$plot(); reinit traces only when type or colorscale changes; store private$.last_plot

- R/VisualizerSurfaceModel.R
  - initialize(): drop opacity/colorscale/show_title params; pass only grid/zmat/labels to super$initialize()
  - render_training_data_layer(): resolve "auto" color with get_auto_color_with_palette(); for size use eff$point_size when unspecified
  - render_boundary_layer(): unchanged; consider linewidth/color deriving from theme in a follow-up (optional)

- R/VisualizerSurfaceObj.R
  - initialize(): drop opacity/colorscale/show_title params; pass only grid/zmat/labels to super$initialize()
  - add_optimization_trace(): keep color = "auto" default
  - render_optimization_trace_layer(): resolve auto color using theme palette; widths may use eff$line_width if we want to harmonize

- R/VisualizerLossFuns.R
  - initialize(): drop default_* params and calls to initialize_defaults()
  - plot(): accept theme override via ...; call super$plot(); keep existing label logic; ensure ggplot theme parameters draw from effective theme; store private$.last_plot
  - render_loss_curves(): prefer colors consistent with theme palette for multiple losses (optional; keep current logic initially to minimize churn)

- man-roxygen/ and docs
  - Remove templates param_default_color_palette.R, param_default_text_size.R, param_default_theme.R from constructors and as_visualizer docs
  - Add @param theme to plot() docs where relevant (“partial theme override; see vistool_theme()”)
  - Document set_theme(), theme(), and vistool_theme()

- tests/testthat/
  - Update tests to new API (no constructor style args). Add new tests:
    - set_theme() stores instance theme and plot(theme=…) overrides it
    - Auto color respects precedence: layer explicit > plot(theme) > instance theme > package default
    - Changing instance theme changes auto colors without re-adding layers
    - save() saves last rendered plot (no re-render) and still works when nothing rendered yet
    - Surface: colorscale switches with palette; alpha from theme applied to surfaces and rasters


## Detailed behavior contracts

- set_theme(theme)
  - Inputs: partial named list; unknown fields ignored
  - Behavior: merge with package default into private$.theme
  - Returns: self (invisible)

- plot(theme = NULL, labels/limits…)
  - Inputs: theme is optional partial list; labels/limits are functional controls
  - Behavior: compute private$.effective_theme; reset color index; build plot in subclass; resolve “auto” in layers; store private$.last_plot; return plot

- save(filename, width, height, dpi)
  - If last_plot exists, save it; else call plot() to build then save; return self (invisible)


## Edge cases to handle

- Plot called without any layers: still return a valid base plot
- Large n_points: performance unchanged; no extra allocations in theme resolution
- Missing labels: use sensible defaults already in data structure
- Surface colorscale when theme$palette is invalid: fallback to "viridis"
- Legend position "none": hide legend in both ggplot2 and plotly
- Title sizing: title_size = text_size + 2 computed inside ggplot or plotly layout


## Migration notes (alpha; no deprecation warnings)

- Removed from constructors/as_visualizer(): default_color_palette, default_text_size, default_theme, default_alpha, default_line_width, default_point_size, opacity, colorscale, show_title.
- Use viz$set_theme(vistool_theme(...)) or viz$plot(theme = list(...)) to style.
- Per-layer style still supported via add_*(); set color = "auto" (default) to use theme palette.


## Work plan and sequencing

1) Add theme system (R/theme.R) + .onLoad default; wire exports
2) Base refactor in R/Visualizer.R (set_theme/theme, plot rework, save behavior); adapt color resolution to effective theme
3) Strip constructor style args from as_visualizer.* and Visualizer* constructors; remove initialize_defaults() calls
4) Update VisualizerModel/VisualizerObj/VisualizerLossFuns/VisualizerSurface*/ to read from effective theme and render params; set private$.last_plot
5) Update docs: roxygen for new functions and changed signatures
6) Update tests and run devtools::test() and devtools::check()
7) Polish: ensure examples in README/vignettes use set_theme() and plot(theme = …)

Each step keeps the package in a runnable state where possible.


## Quality gates and verification

- Build: R CMD check passes without NOTES due to docs/signature changes
- Lint/type: basic R CMD check warnings addressed
- Unit tests: add/adjust tests for theme precedence, auto color resolution, save()
- Manual smoke tests:
  - Model 1D/2D ggplot: verify theme palette affects auto colors and raster fill scales
  - Objective 2D surface (plotly): verify colorscale follows palette; alpha affects opacity
  - LossFuns 1D: verify plot(theme=…) changes text size and theme; add_points color auto resolves


## Follow-ups

- Consolidate color selection in loss functions to use get_vistool_color() for better consistency
- Add rebuild = TRUE to save() and expose render() explicitly
- Support a tiny global options helper: set_vistool_theme_default()
- Theming for boundary linewidth/linetype via theme fields (line_width, line_type defaults)
- Tighten validation for theme values via checkmate


## Quick checklist (for PR reviewers)

- [ ] No constructor style args remain across visualizers and as_visualizer()
- [ ] Base Visualizer has set_theme()/theme(), resolves effective theme inside plot()
- [ ] Precedence is enforced (layer > plot(theme) > instance theme > package)
- [ ] save() prefers last rendered plot
- [ ] Deferred layer resolution intact; "auto" resolves at render
- [ ] Docs and examples mention vistool_theme(), set_theme(), and plot(theme=…)
- [ ] Tests cover precedence and save behavior
