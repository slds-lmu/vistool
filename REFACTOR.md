### Deferred Rendering

All visualizer classes (should) use a deferred rendering architecture:

- **Layer Storage**: All `add_*()` methods (e.g., `$add_points()`, `$add_contours()`, etc.) do not modify the plot directly. Instead, they call the private method `$store_layer(type, spec)` to save a specification of the layer (its type and parameters) to an internal list.
- **Rendering**: The `$plot()` method is responsible for rendering all stored layers. It iterates over the stored layer specifications and calls a private `$render_layer(layer)` method to add each layer to the plot object. This ensures that all visual properties (such as colors, alpha, etc.) are resolved at plot time, using the current global settings.
- **Customization**: Global settings (e.g., color palette, theme, text size) are set via `$plot()`. Layer-specific settings are passed to the relevant `add_*()` method and stored with the layer specification. Defaults are set at initialization.
- **Benefits**: This pattern ensures that plots are always up-to-date with the latest settings, supports re-plotting with different global options, and makes it easier to add, remove, or reorder layers.

init: private$plot_settings

### Color Management

vistool uses a unified color management system to ensure consistent colors across ggplot2 and plotly visualizations (`color_management.R`). The system supports both discrete colors (for points, traces, lines) and continuous color scales (for surfaces, contours).

**Integration with Deferred Rendering**: Colors marked as `"auto"` in layer specifications are resolved at plot time using the current `color_palette` setting. This ensures colors match the selected palette.

### Example Workflow

```r
viz <- as_visualizer(obj, type = "surface")
viz$add_points(points = my_points)
viz$add_contours(contours = my_contours)
# No plot is created yet!

viz$plot(color_palette = "grayscale")  # All layers are rendered now, also resolving colors
```

## OVERVIEW OF CURRENT STATE

### `plot()` Method Locations
- **Base class**: `Visualizer.R` (abstract)
- **Override heavy**: `Visualizer1D`, `Visualizer2D`, `VisualizerSurface`, `VisualizerLossFuns`
- **Override simple**: `Visualizer2DModel`, `Visualizer2DObj`, `VisualizerSurfaceObj`
- **Use parent**: `Visualizer1DModel`, `Visualizer1DObj`, `VisualizerSurfaceModel`

### `add_*()` Method Distribution
- **Base**: `add_points()` in `Visualizer.R`
- **Model classes**: `add_training_data()`, `add_boundary()`
- **Objective classes**: `add_optimization_trace()`
- **Surface objective**: `add_taylor()`, `add_hessian()`
- **Loss functions**: `add_points()` (specialized)
- **Surface**: `add_contours()`

## Issues Identified

### 1. **Inconsistent plot() Override Patterns**
- Some classes properly delegate to parent (good): `VisualizerSurfaceObj`
- Others completely override (sometimes necessary): `Visualizer1D`, `Visualizer2D`
- Some have minimal overrides (good): `Visualizer2DObj`

### 2. **Color Management Inconsistencies** 
- Most classes use `"auto"` color system correctly
- Some hardcoded colors remain in older implementations

**Changes Made:**
- **`Visualizer1D.R`**: Removed direct optimization trace rendering from `plot()` method
- **`Visualizer1DObj.R`**: Added proper `plot()` method that calls `super$plot()` first, then renders optimization traces using deferred system with dedicated private methods `render_optimization_trace_layers()` and `render_optimization_trace_layer()`
- **`Visualizer2DObj.R`**: Refactored existing `plot()` method to follow the same pattern - simplified to call `super$plot()` first, then use dedicated private rendering methods
- **Architecture**: All objective visualizers now follow the same consistent pattern as `VisualizerSurfaceObj`

### Medium Priority
2. **Standardize plot() override patterns** - prefer delegation where possible
3. **Review and clean up color management** - ensure all use unified system
4. **Add consistent error handling** for layer rendering failures

### Low Priority
5. **Documentation consistency** - ensure all add_* methods documented
6. **Test coverage** - verify deferred rendering in all classes
