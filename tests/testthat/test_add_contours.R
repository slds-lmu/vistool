test_that("VisualizerObj add_contours works for 2D objectives", {
  skip_if_not_installed("ggplot2")

  o = obj("TF_branin")
  vis = as_visualizer(o, type = "2d", n_points = 50)

  expect_no_error(vis$add_contours(bins = 7))
  expect_no_error(vis$add_contours(binwidth = 1))

  p = vis$plot()
  expect_no_error({
    p
  })
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_s3_class(p, "ggplot")
    geom_names = vapply(p$layers, function(l) class(l$geom)[1], character(1))
    expect_true(any(grepl("Contour|Raster|Tile|GeomContour|GeomRaster|GeomTile", geom_names, ignore.case = TRUE)))
  }
})

test_that("VisualizerModel add_contours works for 2D tasks", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ggplot2")

  task = mlr3::tsk("iris")$select(c("Sepal.Length", "Sepal.Width"))
  learner = mlr3::lrn("classif.rpart")
  learner_pkgs = unique(learner$packages)
  for (pkg in learner_pkgs) {
    skip_if_not_installed(pkg)
  }

  vis = as_visualizer(task, learner = learner, type = "2d", n_points = 40)

  expect_no_error(vis$add_contours(bins = 8))
  expect_no_error(vis$add_contours(breaks = seq(4, 8, by = 0.5)))

  p = vis$plot()
  expect_no_error({
    p
  })
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_s3_class(p, "ggplot")
    # There should be at least one contour geom or raster/filled geom in layers
    geom_names = vapply(p$layers, function(l) class(l$geom)[1], character(1))
    expect_true(any(grepl("Contour|Raster|Tile|GeomContour|GeomRaster|GeomTile", geom_names, ignore.case = TRUE)))
  }
})

test_that("ggplot2 contours render beneath optimization traces", {
  skip_if_not_installed("ggplot2")

  o = obj("TF_branin")
  vis = as_visualizer(o, type = "2d", n_points = 30)

  opt = OptimizerGD$new(o, x_start = c(-1, 1), lr = 0.05, id = "gd", print_trace = FALSE)
  expect_no_error(opt$optimize(steps = 5L, minimize = TRUE))

  expect_no_error(vis$add_contours())
  expect_no_error(vis$add_optimization_trace(opt, add_marker_at = NULL, show_start_end = FALSE))

  p = vis$plot()
  contour_idx = which(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomContour") || (
      inherits(layer$geom, "GeomPath") &&
        isTRUE(layer$inherit.aes == FALSE) &&
        is.null(layer$mapping$colour) &&        {
        fixed_colour = layer$aes_params$colour
        if (is.null(fixed_colour)) fixed_colour = layer$geom_params$colour
        !is.null(fixed_colour)
      }
    )
  }, logical(1)))

  trace_idx = which(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomPath") &&
      !is.null(layer$mapping$colour) &&
      any(all.vars(layer$mapping$colour) == ".trace")
  }, logical(1)))

  expect_true(length(contour_idx) > 0)
  expect_true(length(trace_idx) > 0)
  expect_lt(max(contour_idx), min(trace_idx))
})

test_that("ggplot2 contours use reduced default linewidth", {
  skip_if_not_installed("ggplot2")

  o = obj("TF_branin")
  vis = as_visualizer(o, type = "2d", n_points = 30)

  expect_no_error(vis$add_contours(mode = "single"))

  p = vis$plot()
  contour_layers = Filter(function(layer) {
    inherits(layer$geom, "GeomContour")
  }, p$layers)

  expect_true(length(contour_layers) > 0)

  lw_values = vapply(contour_layers, function(layer) {
    lw = layer$aes_params$linewidth
    if (is.null(lw)) lw = layer$geom_params$linewidth
    lw
  }, numeric(1))

  base_lw = get_pkg_theme_default()$line_width
  expected_lw = if (is.null(base_lw)) NA_real_ else base_lw * 0.75

  if (is.na(expected_lw)) {
    expect_true(all(is.na(lw_values)))
  } else {
    expect_true(all(abs(lw_values - expected_lw) < 1e-8))
  }
})
