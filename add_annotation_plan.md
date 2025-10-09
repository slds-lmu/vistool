# `add_annotation()` Implementation Plan

## Goals
- Provide a deferred-rendering-friendly `add_annotation()` method shared by all `Visualizer` subclasses.
- Support plain text and math annotations with color, size, and placement controls suitable for 1D, 2D, and surface plots.
- Keep styling options focused: defer advanced typography or box styling to future work.

## Proposed API
```r
add_annotation(
  text,
  latex = FALSE,
  color = "auto",
  size = NULL,
  opacity = NULL,
  x = NULL,
  y = NULL,
  z = NULL,
  position = NULL,
  hjust = 0.5,
  vjust = 0.5
)
```

- `text`: single character string; error if empty.
- `latex`: when `TRUE`, treat `text` as LaTeX; ggplot uses `parse = TRUE`, plotly wraps with `$...$`.
- `color`: literal color or "auto" resolved via theme palette.
- `size`: numeric; defaults to effective theme `text_size` when `NULL`.
- `opacity`: numeric in `[0, 1]`; defaults to theme alpha when `NULL`.
- Coordinates:
  - Absolute via `x`, `y`, `z` (subset required depending on visualizer dimensionality).
  - Relative placement via `position = list(x = 0.8, y = 0.2, reference = "panel")` where `x`/`y`/`z` in `[0,1]` and `reference` one of `"panel"` (data coordinates) or `"figure"` (normalized panel space). Exactly one placement mode must be provided.
- Justification `hjust`/`vjust` follow ggplot semantics; mapped to plotly `align` variants.

## Layer Storage & Resolution
- `add_annotation()` validates inputs, and enqueues via `private$store_layer("annotation", spec)`.
- Extend `private$resolve_all_layer_colors()`:
  - resolve `color` when set to "auto".
- Ensure `resolve_layer_colors()` also fills default `size`/`opacity` from the effective theme if `NULL`.
- Annotations should be sorted by insertion order; no additional z-index handling yet.

## Rendering Hooks

### ggplot visualizers
1. Implement `private$add_annotations_to_ggplot(plot_obj, visualizer_type)`.
2. For each annotation:
   - Determine final coordinates:
     - If relative (`reference == "figure"`), transform to data coordinates using current axis limits (from render params or data range).
     - For 1D visualizers, infer missing `y` by taking baseline from effective theme or data midpoint.
   - Add text via `ggplot2::annotate("text", ...)` with `parse = latex`.
   - Apply `alpha = opacity`, `size`, `hjust`, `vjust`.
3. Make sure helper runs after other `add_*_ggplot` calls and before theme resolution finishes.

### plotly visualizers
1. Implement `private$add_annotations_to_plotly(plot_obj, visualizer_type)`.
2. For 2D (contour) plots:
   - Use `plotly::add_annotations()` with `xref/yref` derived from placement mode.
   - Pass `font = list(color = color, size = size)` and `opacity` via `alpha`; MathJax by wrapping `$text$` when `latex = TRUE`.
3. For surface (3D) plots:
   - Accumulate annotations under `plot_obj$x$layout$scene$annotations`.

### Integration Points
- Hook new helpers inside each subclass’ `plot()` implementation where layers are rendered:
  - `VisualizerObj`, `VisualizerModel`, `VisualizerLossFuns` (ggplot).
  - `VisualizerSurfaceObj`, `VisualizerSurfaceModel` (plotly).
- Ensure `private$.last_plot` caches include annotation output.

## Validation & Error Handling
- Check exclusive placement mode (absolute vs `position`).
- Validate coordinate completeness per dimensionality.
- Ensure relative coordinates lie in `[0, 1]`.

## Testing Strategy
1. **Storage tests:** confirm layer spec recorded and defaults filled.
2. **Color resolution:** verify `color = "auto"` applies palette to text.
3. **ggplot rendering:**
   - 1D, 2D: ensure annotation layer present with expected label and coordinates.
4. **plotly rendering:**
   - 2D contour: check layout annotations includes correct text, coordinates.
   - 3D surface: confirm scene annotations.
   - Skip tests if plotly missing.
5. **Validation failures:** wrong coordinate combos, out-of-range relative positions.

## Documentation & Release Notes
- Add roxygen docs beside `add_points()` in `R/Visualizer.R` (reference `@inheritParams` templates where appropriate; create `man-roxygen/param_annotation_position.R` if reuse needed).
- Generate Rd via `devtools::document()` (manual step noted).
- Update `NEWS.md` under current development entry describing new annotation support.
- Consider adding a small example a vignette snippet demonstrating text + LaTeX.
