test_that("as_visualizer works for Objective with auto type selection", {
  # Test 1D objective (create a custom one since most TF functions are 2D)
  obj_1d = Objective$new(id = "test_1d", fun = function(x) x^2, xdim = 1, lower = -5, upper = 5)
  vis_1d = as_visualizer(obj_1d)
  expect_s3_class(vis_1d, "Visualizer1DObj")

  # Test 2D objective (auto should default to ggplot2)
  obj_2d = obj("TF_branin")
  vis_2d = as_visualizer(obj_2d)
  expect_s3_class(vis_2d, "Visualizer2DObj")

  # For objectives with 3D+ dimensions, auto selection should error
  obj_3d = Objective$new(
    id = "test_3d", fun = function(x) sum(x^2), xdim = 3,
    lower = c(-1, -1, -1), upper = c(1, 1, 1)
  )
  expect_error(as_visualizer(obj_3d), "Auto visualization only supports 1D and 2D objectives")
})

test_that("as_visualizer works for Objective with explicit type selection", {
  obj_2d = obj("TF_branin")

  # Test explicit 2D (ggplot2)
  vis_2d = as_visualizer(obj_2d, type = "2d")
  expect_s3_class(vis_2d, "Visualizer2DObj")

  # Test explicit surface for 2D objective (plotly)
  vis_surface = as_visualizer(obj_2d, type = "surface")
  expect_s3_class(vis_surface, "VisualizerSurfaceObj")
})

test_that("as_visualizer works for Task with auto type selection", {
  skip_if_not_installed("mlr3learners")

  # Test 1D task
  task_1d = tsk("mtcars")
  task_1d$select("gear")
  learner = lrn("regr.lm")

  vis_1d = as_visualizer(task_1d, learner = learner)
  expect_s3_class(vis_1d, "Visualizer1DModel")

  # Test 2D task (auto should default to ggplot2)
  task_2d = tsk("mtcars")
  task_2d$select(c("gear", "cyl"))

  vis_2d = as_visualizer(task_2d, learner = learner)
  expect_s3_class(vis_2d, "Visualizer2DModel")

  # Test task with >2 features (auto should error)
  task_3d = tsk("mtcars")
  task_3d$select(c("gear", "cyl", "hp"))

  expect_error(as_visualizer(task_3d, learner = learner), "Auto visualization only supports 1D and 2D tasks")
})

test_that("as_visualizer works for Task with explicit type selection", {
  skip_if_not_installed("mlr3learners")

  task_2d = tsk("mtcars")
  task_2d$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  # Test explicit 2D (ggplot2)
  vis_2d = as_visualizer(task_2d, learner = learner, type = "2d")
  expect_s3_class(vis_2d, "Visualizer2DModel")

  # Test explicit surface for 2D task (plotly)
  vis_surface = as_visualizer(task_2d, learner = learner, type = "surface")
  expect_s3_class(vis_surface, "VisualizerSurfaceModel")
})

test_that("as_visualizer works for LossFunction", {
  # Test LossFunction (should always create 1D visualizer)
  loss_func = lss("l2_se") # Use a valid loss function key
  y_pred = seq(-4, 4, length.out = 100)
  y_true = 1

  vis = as_visualizer(loss_func, y_pred = y_pred, y_true = y_true)
  expect_s3_class(vis, "VisualizerLossFuns")
})

test_that("as_visualizer parameter passing works correctly", {
  obj_2d = obj("TF_branin")

  # Test with custom limits and n_points
  vis = as_visualizer(obj_2d,
    x1_limits = c(-1, 1),
    x2_limits = c(-2, 2),
    padding = 0.1,
    n_points = 50L
  )

  expect_s3_class(vis, "Visualizer2DObj")
  # We can't easily test the internal parameters without accessing private fields
  # but we can at least verify the object was created successfully
})

test_that("as_visualizer error handling works", {
  # Create test objectives
  obj_1d = Objective$new(id = "test_1d", fun = function(x) x^2, xdim = 1, lower = -5, upper = 5)
  obj_2d = obj("TF_branin")

  # Test dimension mismatch errors
  expect_error(
    as_visualizer(obj_1d, type = "2d"),
    "2D and surface visualizations require an objective with exactly 2 dimensions"
  )

  expect_error(
    as_visualizer(obj_2d, type = "1d"),
    "1D visualization requires an objective with exactly 1 dimension"
  )

  expect_error(
    as_visualizer(obj_1d, type = "surface"),
    "2D and surface visualizations require an objective with exactly 2 dimensions"
  )

  # Test invalid type - the actual error message uses checkmate format
  expect_error(
    as_visualizer(obj_2d, type = "invalid"),
    "Must be element of set"
  )
})

test_that("as_visualizer automatic type selection follows documented behavior", {
  # Test that auto selection follows the documented rules:
  # 1D -> Visualizer1DObj (ggplot2)
  # 2D -> Visualizer2DObj (ggplot2)
  # 3D+ -> Error (must specify type explicitly)

  # 1D objective
  obj_1d = Objective$new(id = "test_1d", fun = function(x) x^2, xdim = 1, lower = -5, upper = 5)
  vis_1d_auto = as_visualizer(obj_1d) # type = "auto" is default
  expect_s3_class(vis_1d_auto, "Visualizer1DObj")

  # 2D objective (should default to ggplot2, not plotly)
  obj_2d = obj("TF_branin")
  vis_2d_auto = as_visualizer(obj_2d) # type = "auto" is default
  expect_s3_class(vis_2d_auto, "Visualizer2DObj") # Should be 2D ggplot2, not surface plotly
})

test_that("as_visualizer backend selection works correctly", {
  # Test 2D objective with different backends
  obj_2d = obj("TF_branin")

  # Test default 2D (ggplot2)
  vis_2d_ggplot = as_visualizer(obj_2d) # defaults to type = "2d"
  expect_s3_class(vis_2d_ggplot, "Visualizer2DObj")
  plot_2d = vis_2d_ggplot$plot()
  expect_s3_class(plot_2d, "ggplot")

  # Test explicit surface (plotly)
  vis_2d_plotly = as_visualizer(obj_2d, type = "surface")
  expect_s3_class(vis_2d_plotly, "VisualizerSurfaceObj")
  vis_2d_plotly$init_layer_surface()
  plot_surface = vis_2d_plotly$plot()
  expect_s3_class(plot_surface, "plotly")

  # Test 1D objective (always ggplot2)
  obj_1d = Objective$new(id = "test_1d", fun = function(x) x^2, xdim = 1, lower = -5, upper = 5)
  vis_1d = as_visualizer(obj_1d)
  expect_s3_class(vis_1d, "Visualizer1DObj")
  plot_1d = vis_1d$plot()
  expect_s3_class(plot_1d, "ggplot")
})

test_that("as_visualizer creates objects that can plot", {
  # Test that the created visualizers can actually plot
  obj_2d = obj("TF_branin")

  # Test 2D ggplot2 visualizer
  vis_2d = as_visualizer(obj_2d, type = "2d")
  plot_2d = vis_2d$plot()
  expect_s3_class(plot_2d, "ggplot")

  # Test surface plotly visualizer
  vis_surface = as_visualizer(obj_2d, type = "surface")
  vis_surface$init_layer_surface()
  plot_surface = vis_surface$plot()
  expect_s3_class(plot_surface, "plotly")
})

test_that("as_visualizer Task dimension validation works", {
  skip_if_not_installed("mlr3learners")

  task_1d = tsk("mtcars")
  task_1d$select("gear")
  task_2d = tsk("mtcars")
  task_2d$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  # Test dimension validation for tasks
  expect_error(
    as_visualizer(task_1d, learner = learner, type = "2d"),
    "2D and surface visualizations require a task with exactly 2 features"
  )

  expect_error(
    as_visualizer(task_2d, learner = learner, type = "1d"),
    "1D visualization requires a task with exactly 1 feature"
  )

  expect_error(
    as_visualizer(task_1d, learner = learner, type = "surface"),
    "2D and surface visualizations require a task with exactly 2 features"
  )

  # Test that tasks with >2 features fail in auto mode
  task_multi = tsk("mtcars")
  task_multi$select(c("gear", "cyl", "hp"))

  expect_error(
    as_visualizer(task_multi, learner = learner),
    "Auto visualization only supports 1D and 2D tasks"
  )
})

test_that("as_visualizer LossFunction type validation works", {
  loss_func = lss("l2_se")
  y_pred = seq(-4, 4, length.out = 100)
  y_true = 1

  # Test that LossFunction only accepts "auto" and "1d" types
  expect_error(
    as_visualizer(loss_func, type = "2d", y_pred = y_pred, y_true = y_true),
    "Must be element of set"
  )

  expect_error(
    as_visualizer(loss_func, type = "surface", y_pred = y_pred, y_true = y_true),
    "Must be element of set"
  )

  # Test that "auto" and "1d" work
  vis_auto = as_visualizer(loss_func, type = "auto", y_pred = y_pred, y_true = y_true)
  expect_s3_class(vis_auto, "VisualizerLossFuns")

  vis_1d = as_visualizer(loss_func, type = "1d", y_pred = y_pred, y_true = y_true)
  expect_s3_class(vis_1d, "VisualizerLossFuns")
})
