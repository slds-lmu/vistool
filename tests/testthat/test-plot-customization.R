test_that("plot customization parameters work for Visualizer1D", {
  # Create a simple 1D objective function
  obj = Objective$new(id = "test_1d", fun = function(x) x^2, xdim = 1, lower = -5, upper = 5)
  vis = as_visualizer(obj, type = "1d", n_points = 50)
  
  # Test custom title and subtitle
  p1 = vis$plot(plot_title = "Custom Title", plot_subtitle = "Custom Subtitle")
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("Custom Title", p1$labels$title))
  expect_true(grepl("Custom Subtitle", p1$labels$subtitle))
  
  # Test custom axis labels
  p2 = vis$plot(x_lab = "Input Variable", y_lab = "Output Value")
  expect_s3_class(p2, "ggplot")
  expect_equal(p2$labels$x, "Input Variable")
  expect_equal(p2$labels$y, "Output Value")
  
  # Test axis limits
  p3 = vis$plot(x_limits = c(-3, 3), y_limits = c(0, 10))
  expect_s3_class(p3, "ggplot")
  
  # Test grid customization
  p4 = vis$plot(show_grid = FALSE)
  expect_s3_class(p4, "ggplot")
  
  p5 = vis$plot(show_grid = TRUE, grid_color = "red")
  expect_s3_class(p5, "ggplot")
  
  # Test legend customization
  p6 = vis$plot(show_legend = FALSE)
  expect_s3_class(p6, "ggplot")
  
  p7 = vis$plot(legend_position = "top", legend_title = "Custom Legend")
  expect_s3_class(p7, "ggplot")
})

test_that("plot customization parameters work for Visualizer2D", {
  # Create a simple 2D objective function  
  obj = Objective$new(id = "test_2d", fun = function(x) x[1]^2 + x[2]^2, xdim = 2, lower = c(-3, -3), upper = c(3, 3))
  vis = as_visualizer(obj, type = "2d", n_points = 30)
  
  # Test custom title and subtitle
  p1 = vis$plot(plot_title = "2D Custom Title", plot_subtitle = "2D Custom Subtitle")
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("2D Custom Title", p1$labels$title))
  expect_true(grepl("2D Custom Subtitle", p1$labels$subtitle))
  
  # Test custom axis labels
  p2 = vis$plot(x_lab = "First Variable", y_lab = "Second Variable")
  expect_s3_class(p2, "ggplot")
  expect_equal(p2$labels$x, "First Variable")
  expect_equal(p2$labels$y, "Second Variable")
  
  # Test axis limits
  p3 = vis$plot(x_limits = c(-2, 2), y_limits = c(-2, 2))
  expect_s3_class(p3, "ggplot")
  
  # Test legend customization
  p4 = vis$plot(legend_title = "Function Value", legend_position = "bottom")
  expect_s3_class(p4, "ggplot")
  
  # Test grid customization
  p5 = vis$plot(show_grid = FALSE)
  expect_s3_class(p5, "ggplot")
})

test_that("plot customization parameters work for VisualizerSurface", {
  # Create a simple 2D objective function for surface plot
  obj = Objective$new(id = "test_surf", fun = function(x) x[1]^2 + x[2]^2, xdim = 2, lower = c(-3, -3), upper = c(3, 3))
  vis = as_visualizer(obj, type = "surface", n_points = 20)
  
  # Test custom title (surface plot)
  p1 = vis$plot(plot_title = "3D Custom Title")
  expect_true(inherits(p1, "plotly"))
  
  # Test custom axis labels (surface plot)
  p2 = vis$plot(x_lab = "X Variable", y_lab = "Y Variable", z_lab = "Z Value")
  expect_true(inherits(p2, "plotly"))
  
  # Test axis limits (surface plot)
  p3 = vis$plot(x_limits = c(-2, 2), y_limits = c(-2, 2), z_limits = c(0, 10))
  expect_true(inherits(p3, "plotly"))
  
  # Test legend customization
  p4 = vis$plot(show_legend = FALSE)
  expect_true(inherits(p4, "plotly"))
  
  # Test contour plot (flatten = TRUE) - note: legend_position not applicable to flattened surface
  p5 = vis$plot(flatten = TRUE, plot_title = "2D Contour")
  expect_true(inherits(p5, "plotly"))
})

test_that("plot customization parameters work for VisualizerLossFuns", {
  skip_if_not_installed("mlr3")
  
  # Create loss function visualizer
  loss = lss("l2_se")
  vis = VisualizerLossFuns$new(list(loss))
  
  # Test custom title and subtitle
  p1 = vis$plot(plot_title = "Loss Function", plot_subtitle = "L2 Squared Error")
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("Loss Function", p1$labels$title))
  expect_true(grepl("L2 Squared Error", p1$labels$subtitle))
  
  # Test custom axis labels
  p2 = vis$plot(x_lab = "Prediction Error", y_lab = "Loss Value")
  expect_s3_class(p2, "ggplot")
  expect_equal(p2$labels$x, "Prediction Error")
  expect_equal(p2$labels$y, "Loss Value")
  
  # Test axis limits
  p3 = vis$plot(x_limits = c(-2, 2), y_limits = c(0, 5))
  expect_s3_class(p3, "ggplot")
  
  # Test legend customization
  p4 = vis$plot(legend_position = "top", show_legend = TRUE)
  expect_s3_class(p4, "ggplot")
  
  # Test grid customization
  p5 = vis$plot(show_grid = FALSE)
  expect_s3_class(p5, "ggplot")
})

test_that("plot customization parameter validation works", {
  # Create a simple visualizer
  obj = Objective$new(id = "test_val", fun = function(x) x^2, xdim = 1, lower = -5, upper = 5)
  vis = as_visualizer(obj, type = "1d", n_points = 50)
  
  # Test invalid legend position
  expect_error(vis$plot(legend_position = "invalid"))
  
  # Test invalid axis limits (wrong length)
  expect_error(vis$plot(x_limits = c(1, 2, 3)))
  
  # Test invalid grid color (non-string)
  expect_error(vis$plot(grid_color = 123))
  
  # Test invalid show_grid (non-logical)
  expect_error(vis$plot(show_grid = "yes"))
  
  # Test invalid show_legend (non-logical)
  expect_error(vis$plot(show_legend = "true"))
})

test_that("parameter inheritance works in specialized visualizer classes", {
  skip_if_not_installed("mlr3")
  
  # Test with regression Model visualizer using mtcars task (regression) 
  task = tsk("mtcars")$select(c("wt"))  # 1D regression task using wt as feature
  learner = lrn("regr.featureless")
  learner$train(task)
  
  vis_model = as_visualizer(task, learner = learner, type = "1d")
  
  # Test that custom parameters are passed through
  p1 = vis_model$plot(plot_title = "Model Plot", x_lab = "Weight", show_grid = FALSE)
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("Model Plot", p1$labels$title))
  expect_equal(p1$labels$x, "Weight")
  
  # Test with 2D Model visualizer
  task_2d = tsk("mtcars")$select(c("wt", "hp"))  # 2D regression task
  vis_model_2d = as_visualizer(task_2d, learner = learner, type = "2d")
  
  p2 = vis_model_2d$plot(plot_title = "2D Model", legend_position = "bottom")
  expect_s3_class(p2, "ggplot")
  expect_true(grepl("2D Model", p2$labels$title))
})

test_that("show_title parameter works for all visualizer types", {
  # Test Visualizer1D
  obj_1d = Objective$new(id = "test_1d", fun = function(x) x^2, xdim = 1, lower = -5, upper = 5)
  vis_1d = as_visualizer(obj_1d, type = "1d", n_points = 50)
  
  # Test with title shown (default)
  p1 = vis_1d$plot(show_title = TRUE)
  expect_s3_class(p1, "ggplot")
  expect_false(is.null(p1$labels$title))
  
  # Test with title hidden
  p2 = vis_1d$plot(show_title = FALSE)
  expect_s3_class(p2, "ggplot")
  expect_true(is.null(p2$labels$title))
  
  # Test with custom title and show_title = FALSE (should still hide title)
  p3 = vis_1d$plot(plot_title = "Custom Title", show_title = FALSE)
  expect_s3_class(p3, "ggplot")
  expect_true(is.null(p3$labels$title))
  
  # Test with custom title and show_title = TRUE (should show custom title)
  p4 = vis_1d$plot(plot_title = "Custom Title", show_title = TRUE)
  expect_s3_class(p4, "ggplot")
  expect_equal(p4$labels$title, "Custom Title")
  
  # Test Visualizer2D
  obj_2d = Objective$new(id = "test_2d", fun = function(x) x[1]^2 + x[2]^2, xdim = 2, lower = c(-3, -3), upper = c(3, 3))
  vis_2d = as_visualizer(obj_2d, type = "2d", n_points = 30)
  
  # Test with title shown (default)
  p5 = vis_2d$plot(show_title = TRUE)
  expect_s3_class(p5, "ggplot")
  expect_false(is.null(p5$labels$title))
  
  # Test with title hidden
  p6 = vis_2d$plot(show_title = FALSE)
  expect_s3_class(p6, "ggplot")
  expect_true(is.null(p6$labels$title))
  
  # Test VisualizerSurface
  vis_surface = as_visualizer(obj_2d, type = "surface", n_points = 20)
  
  # Test with title shown (default) - just check it returns plotly object
  p7 = vis_surface$plot(show_title = TRUE)
  expect_true(inherits(p7, "plotly"))
  
  # Test with title hidden - just check it returns plotly object  
  p8 = vis_surface$plot(show_title = FALSE)
  expect_true(inherits(p8, "plotly"))
  
  # Test flattened surface (contour plot)
  p9 = vis_surface$plot(flatten = TRUE, show_title = TRUE)
  expect_true(inherits(p9, "plotly"))
  
  p10 = vis_surface$plot(flatten = TRUE, show_title = FALSE)
  expect_true(inherits(p10, "plotly"))
})

test_that("show_title parameter works for model visualizers", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("rpart")
  # Test Visualizer1DModel via inherited functionality
  task_1d = tsk("mtcars")$select("wt")  # 1D regression task
  learner = lrn("regr.rpart")  # Using rpart instead of lm
  learner$train(task_1d)
  
  vis_model_1d = as_visualizer(task_1d, learner = learner, type = "1d")
  
  # Test with title shown
  p1 = vis_model_1d$plot(show_title = TRUE)
  expect_s3_class(p1, "ggplot")
  expect_false(is.null(p1$labels$title))
  
  # Test with title hidden
  p2 = vis_model_1d$plot(show_title = FALSE)
  expect_s3_class(p2, "ggplot")
  expect_true(is.null(p2$labels$title))
  
  # Test Visualizer2DModel
  task_2d = tsk("mtcars")$select(c("wt", "hp"))  # 2D regression task
  learner$train(task_2d)
  vis_model_2d = as_visualizer(task_2d, learner = learner, type = "2d")
  
  # Test with title shown
  p3 = vis_model_2d$plot(show_title = TRUE)
  expect_s3_class(p3, "ggplot")
  expect_false(is.null(p3$labels$title))
  
  # Test with title hidden
  p4 = vis_model_2d$plot(show_title = FALSE)
  expect_s3_class(p4, "ggplot")
  expect_true(is.null(p4$labels$title))
})

test_that("show_title parameter works for loss function visualizers", {
  skip_if_not_installed("mlr3")
  # Create a simple binary classification task for loss function visualization
  task_classif = tsk("sonar")
  # Use a single loss function to avoid compatibility issues
  losses = list(lss("cross-entropy"))
  
  vis_loss = as_visualizer(losses, task = task_classif)
  
  # Test with title shown (default)
  p1 = vis_loss$plot(show_title = TRUE)
  expect_s3_class(p1, "ggplot")
  expect_false(is.null(p1$labels$title))
  
  # Test with title hidden
  p2 = vis_loss$plot(show_title = FALSE)
  expect_s3_class(p2, "ggplot")
  expect_true(is.null(p2$labels$title))
})

test_that("show_title default behavior works correctly", {
  # Test that default is TRUE (title shown)
  obj = Objective$new(id = "test", fun = function(x) x^2, xdim = 1, lower = -5, upper = 5)
  vis = as_visualizer(obj, type = "1d", n_points = 50)
  
  # When show_title is not specified, should default to TRUE
  p1 = vis$plot()
  expect_s3_class(p1, "ggplot")
  expect_false(is.null(p1$labels$title))
  
  # Explicitly setting show_title = TRUE should work the same
  p2 = vis$plot(show_title = TRUE)
  expect_s3_class(p2, "ggplot")
  expect_false(is.null(p2$labels$title))
  expect_equal(p1$labels$title, p2$labels$title)
})
