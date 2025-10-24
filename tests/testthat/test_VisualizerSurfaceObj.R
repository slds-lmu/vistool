test_that("VisualizerSurfaceObj creation works", {
  obj = obj("TF_branin")

  vis = VisualizerSurfaceObj$new(obj)

  expect_s3_class(vis, "VisualizerSurfaceObj")
  expect_s3_class(vis, "VisualizerSurface")
  expect_identical(vis$objective, obj)
})

test_that("VisualizerSurfaceObj with custom limits works", {
  obj = obj("TF_branin")

  vis = VisualizerSurfaceObj$new(
    obj,
    x1_limits = c(0, 5),
    x2_limits = c(0, 5),
    n_points = 20L
  )

  expect_s3_class(vis, "VisualizerSurfaceObj")
})

test_that("VisualizerSurfaceObj surface initialization works", {
  obj = obj("TF_branin")
  vis = VisualizerSurfaceObj$new(obj, n_points = 10L)
  vis$init_layer_surface()
  expect_true(!is.null(vis$plot()))
})

test_that("VisualizerSurfaceObj contour initialization works", {
  obj = obj("TF_branin")
  vis = VisualizerSurfaceObj$new(obj, n_points = 10L)
  p = vis$plot(flatten = TRUE)
  expect_true(!is.null(p))
})

test_that("VisualizerSurfaceObj optimization trace works", {
  skip_if_not_installed("mlr3learners")

  obj = obj("TF_branin")
  vis = VisualizerSurfaceObj$new(obj, n_points = 10L)
  vis$init_layer_surface()
  opt = OptimizerGD$new(obj, x_start = c(0, 0), lr = 0.01, print_trace = FALSE)
  opt$optimize(steps = 3L)
  expect_silent(vis$add_optimization_trace(opt))
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceObj input validation works", {
  obj_1d = Objective$new(
    id = "test_1d",
    fun = function(x) x^2,
    xdim = 1,
    lower = -1,
    upper = 1
  )

  expect_error(
    VisualizerSurfaceObj$new(obj_1d),
    "2-dimensional inputs"
  )
  obj_3d = Objective$new(
    id = "test_3d",
    fun = function(x) sum(x^2),
    xdim = 3,
    lower = c(-1, -1, -1),
    upper = c(1, 1, 1)
  )

  expect_error(
    VisualizerSurfaceObj$new(obj_3d),
    "2-dimensional inputs"
  )
})

test_that("VisualizerSurfaceObj with missing limits works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(NA, NA),
    upper = c(NA, NA)
  )
  expect_error(
    VisualizerSurfaceObj$new(obj),
    "Objective bounds not available; please specify both 'x1_limits' and 'x2_limits' explicitly."
  )
  vis = VisualizerSurfaceObj$new(
    obj,
    x1_limits = c(-2, 2),
    x2_limits = c(-2, 2)
  )
  expect_s3_class(vis, "VisualizerSurfaceObj")
})

test_that("VisualizerSurfaceObj padding works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1)
  )

  vis = VisualizerSurfaceObj$new(obj, padding = 0.2, n_points = 5L)
  vis$init_layer_surface()
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceObj scene setting works", {
  obj = obj("TF_branin")
  vis = VisualizerSurfaceObj$new(obj, n_points = 5L)
  vis$init_layer_surface()
  vis$set_scene(x = 1.2, y = 1.3, z = 1.4)
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceObj save functionality works", {
  skip_if_not_installed("webshot2")
  skip_if_not_installed("magick")
  skip_on_ci()

  obj = obj("TF_branin")
  vis = VisualizerSurfaceObj$new(obj, n_points = 5L)
  vis$init_layer_surface()
  tmp = tempfile(fileext = ".png")
  expect_no_error(vis$plot(plot_title = "$f(x)$", latex = list(title = TRUE)))
  expect_no_error(vis$save(tmp, width = 320, height = 240))
  expect_true(file.exists(tmp))

  info = magick::image_info(magick::image_read(tmp))
  expect_true(info$width <= 320)
  expect_true(info$height <= 240)
  unlink(tmp)
})

test_that("Plotly MathJax configuration respects vistool.mathjax option", {
  skip_if_not_installed("plotly")
  skip_on_ci()

  obj = obj("TF_branin")
  original = getOption("vistool.mathjax")
  on.exit(options(vistool.mathjax = original), add = TRUE)

  options(vistool.mathjax = "local")
  vis_local = VisualizerSurfaceObj$new(obj, n_points = 5L)
  vis_local$init_layer_surface()
  plot_local = vis_local$plot(plot_title = "$f(x)$", latex = list(title = TRUE))
  expect_true(is.null(plot_local$x$config$mathjax) || identical(plot_local$x$config$mathjax, "local"))
  expect_true(isTRUE(plot_local$x$config$typesetMath))

  custom_url = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
  supports_custom_mathjax = !inherits(
    try(plotly::config(plotly::plot_ly(), mathjax = custom_url), silent = TRUE),
    "try-error"
  )
  options(vistool.mathjax = custom_url)
  vis_custom = VisualizerSurfaceObj$new(obj, n_points = 5L)
  vis_custom$init_layer_surface()
  if (!supports_custom_mathjax) {
    expect_error(
      vis_custom$plot(plot_title = "$g(x)$", latex = list(title = TRUE)),
      regexp = "should be one of"
    )
  } else {
    plot_custom = vis_custom$plot(plot_title = "$g(x)$", latex = list(title = TRUE))
    expect_equal(plot_custom$x$config$mathjax, custom_url)
    expect_true(isTRUE(plot_custom$x$config$typesetMath))
  }
})

test_that("VisualizerSurfaceObj multiple optimization traces work", {
  obj = obj("TF_branin")
  vis = VisualizerSurfaceObj$new(obj, n_points = 10L)
  opt1 = OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(0, 0), lr = 0.01, print_trace = FALSE)
  opt2 = OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(1, 1), lr = 0.02, print_trace = FALSE)
  opt1$optimize(steps = 2L)
  opt2$optimize(steps = 2L)
  vis$add_optimization_trace(opt1, line_color = "red")
  vis$add_optimization_trace(opt2, line_color = "blue")
  p = vis$plot(flatten = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceObj custom contours parameter works", {
  skip_if_not_installed("plotly")
  obj = obj("TF_franke")
  vis = VisualizerSurfaceObj$new(obj, n_points = 10L)
  llower = vis$objective$limits_lower
  lupper = vis$objective$limits_upper
  ssize = (lupper - llower) / 10
  custom_contours = list(
    x = list(show = TRUE, start = llower[1], end = lupper[1], size = ssize[1], color = "red"),
    y = list(show = TRUE, start = llower[2], end = lupper[2], size = ssize[2], color = "blue")
  )

  vis$init_layer_surface(
    contours = custom_contours
  )
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceObj add_contours parameter validation", {
  skip_if_not_installed("plotly")

  obj = obj("TF_branin")
  vis = VisualizerSurfaceObj$new(obj, n_points = 5L)
  expect_error(vis$add_contours(contours = "invalid"), class = "simpleError")
  expect_silent(vis$add_contours(contours = NULL))
  expect_silent(vis$add_contours(contours = list()))
})

test_that("VisualizerSurfaceObj layer methods use deferred rendering", {
  obj = obj("TF_branin")
  vis = VisualizerSurfaceObj$new(obj, n_points = 10L)
  x0 = c(2.5, 7.5)
  expect_silent(vis$add_taylor(x0, degree = 1))
  expect_silent(vis$add_hessian(x0))
  layers = vis$.__enclos_env__$private$.layers_to_add
  expect_true(length(layers) >= 2) # Should have stored the taylor and hessian layers
  expect_silent(vis$plot())
  p = vis$plot()
  expect_true(!is.null(p))
})
