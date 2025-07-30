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

### OVERVIEW OF CURRENT STATE

Based on analysis of all Visualizer classes, here's the current deferred rendering implementation status:

#### ✅ **Fully Implemented (Excellent Examples)**

- **`Visualizer.R`** - Perfect base implementation
  - ✅ `store_layer()`, `get_layer()`, `get_layers_by_type()` methods
  - ✅ `resolve_layer_colors()` for color resolution at plot time
  - ✅ `add_points()` uses deferred rendering correctly
  - ✅ Helper methods `add_points_to_ggplot()` and `add_points_to_plotly()`

- **`Visualizer1DObj.R`** - Good implementation
  - ✅ `add_optimization_trace()` uses `store_layer()` correctly
  - ✅ Plot method renders stored layers properly

- **`Visualizer2DObj.R`** - Good implementation  
  - ✅ `add_optimization_trace()` uses `store_layer()` correctly
  - ✅ Plot method iterates through stored layers and renders them

- **`VisualizerSurface.R`** - Good implementation
  - ✅ `add_contours()` uses `store_layer()` correctly
  - ✅ Plot method calls `render_stored_layers()`
    (render training data and boundary SHOULD BE MOVED TO VisualizerSurfaceModel)

- **`VisualizerSurfaceObj.R`** - Good implementation
  - ✅ `add_optimization_trace()` uses `store_layer()` correctly
  - ✅ Has `render_optimization_trace_layers()` and `render_optimization_trace_layer()` methods

- **`Visualizer1DModel.R`** - Good implementation
  - ✅ `add_training_data()` delegates to `add_points()` (deferred)
  - ✅ `add_boundary()` uses `store_layer()` correctly

- **`Visualizer2DModel.R`** - Good implementation
  - ✅ `add_training_data()` and `add_boundary()` use `store_layer()` correctly

- **`VisualizerSurfaceModel.R`** - Good implementation
  - ✅ `add_training_data()` and `add_boundary()` use `store_layer()` correctly 

- **`VisualizerSurfaceObj.R`** - Excellent implementation
  - ✅ `add_optimization_trace()` uses `store_layer()` correctly
  - ✅ Has `render_optimization_trace_layers()` and `render_optimization_trace_layer()` methods
  - ✅ `add_taylor()` uses `store_layer()` with `render_taylor_layers()` method
  - ✅ `add_hessian()` uses `store_layer()` with `render_hessian_layers()` method
  - ✅ Plot method renders all stored layers correctly

#### ❌ **Issues Identified**

- ✅ **COMPLETED: VisualizerLossFuns.R** - Fully refactored to use deferred rendering
  - ✅ Adopted deferred rendering pattern with `store_layer()` and `render_plot()`
  - ✅ Implemented custom `add_points()` method for loss function-specific point visualization
  - ✅ Added specialized layer rendering with hollow points and optional vertical lines
  - ✅ All plotting logic now uses stored plot settings from `private$.plot_settings`


#### 📋 **Priority Refactoring Tasks**

1. **✅ COMPLETED: Refactor VisualizerSurfaceObj legacy methods**:
   - ✅ Convert `add_layer_taylor()` and `add_layer_hessian()` to use `store_layer()`
   - ✅ Add corresponding render methods
   - ✅ Update method names to follow consistent naming (remove `_layer` prefix)

2. ✅ **COMPLETED: Enhance VisualizerLossFuns**:
   - ✅ Added `add_points()` method with automatic y-value inference from loss functions
   - ✅ Implemented deferred rendering pattern with `store_layer()`, `render_plot()`, and specialized layer rendering
   - ✅ Added hollow points with optional vertical lines to x-axis for visualizing loss magnitude

3. **Consider extending base classes**:
   - `Visualizer1D` and `Visualizer2D` could benefit from `add_lines()`, `add_annotations()`, etc.
