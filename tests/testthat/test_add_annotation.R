test_that("add_annotation enforces exclusive placement", {
  vis = as_visualizer(obj("TF_gaussian1", xdim = 1), type = "1d", x1_limits = c(-2, 2))
  expect_error(vis$add_annotation(text = "hi"), "Provide absolute coordinates")
  expect_error(vis$add_annotation(text = "hi", x = 0, position = list(x = 0.5)))
})

test_that("annotation defaults resolve from effective theme", {
  vis = as_visualizer(obj("TF_gaussian1", xdim = 1), type = "1d", x1_limits = c(-2, 2))
  vis$add_annotation(text = "optimum", x = 0)
  vis$plot()
  priv = vis$.__enclos_env__$private
  ann_layers = Filter(function(layer) layer$type == "annotation", priv$.layers_to_add)
  expect_length(ann_layers, 1)
  spec = ann_layers[[1]]$spec
  expect_false(identical(spec$color, "auto"))
  eff = priv$.effective_theme
  expect_equal(spec$size, eff$text_size)
  expect_equal(spec$opacity, eff$alpha)
})

test_that("relative annotations map to data coordinates for 1D plots", {
  vis = as_visualizer(obj("TF_gaussian1", xdim = 1), type = "1d", x1_limits = c(-4, 4))
  vis$add_annotation(text = "panel", position = list(x = 0.25, reference = "panel"))
  p = vis$plot()
  gb = ggplot2::ggplot_build(p)
  ann_data = gb$data[[length(gb$data)]]
  expect_equal(ann_data$x, -4 + 0.25 * (4 - (-4)), tolerance = 1e-6)
})

test_that("plotly contour annotations populate layout", {
  obj2d = obj("TF_branin")
  vis = as_visualizer(obj2d, type = "surface")
  vis$add_annotation(text = "contour", position = list(x = 0.1, y = 0.9, reference = "figure"))
  plt = vis$plot(flatten = TRUE)
  expect_true(length(plt$x$layout$annotations) >= 1)
  ann = plt$x$layout$annotations[[1]]
  expect_equal(ann$xref, "paper")
  expect_equal(ann$yref, "paper")
  expect_equal(ann$x, 0.1)
})

test_that("plotly surface annotations populate scene", {
  obj2d = obj("TF_branin")
  vis = as_visualizer(obj2d, type = "surface")
  vis$add_annotation(text = "surface", x = vis$grid$x1[1], y = vis$grid$x2[1], z = vis$zmat[1, 1])
  plt = vis$plot(flatten = FALSE)
  expect_true(length(plt$x$layout$scene$annotations) >= 1)
  ann = plt$x$layout$scene$annotations[[1]]
  expect_equal(ann$xref, "x")
  expect_equal(ann$yref, "y")
  expect_equal(ann$zref, "z")
})

test_that("plotly contour latex annotations keep mathjax text", {
  skip_if_not_installed("plotly")
  obj2d = obj("TF_branin")
  vis = as_visualizer(obj2d, type = "surface")
  vis$add_annotation(text = "$\\alpha$", latex = TRUE, x = vis$grid$x1[5], y = vis$grid$x2[10])
  plt = vis$plot(flatten = TRUE)
  ann = plt$x$layout$annotations[[length(plt$x$layout$annotations)]]
  expect_s3_class(ann$text, "TeX")
  expect_match(as.character(ann$text), "\\$\\\\alpha\\$")
})

test_that("plotly surface latex annotations keep mathjax text", {
  skip_if_not_installed("plotly")
  obj2d = obj("TF_branin")
  vis = as_visualizer(obj2d, type = "surface")
  vis$add_annotation(text = "$\\beta$", latex = TRUE, x = vis$grid$x1[3], y = vis$grid$x2[7], z = vis$zmat[3, 7])
  plt = vis$plot(flatten = FALSE)
  ann = plt$x$layout$scene$annotations[[length(plt$x$layout$scene$annotations)]]
  expect_s3_class(ann$text, "TeX")
  expect_match(as.character(ann$text), "\\$\\\\beta\\$")
})

test_that("plotly latex annotations configure mathjax", {
  skip_if_not_installed("plotly")
  obj2d = obj("TF_branin")

  vis2d = as_visualizer(obj2d, type = "surface")
  vis2d$add_annotation(text = "$\\gamma$", latex = TRUE, x = vis2d$grid$x1[4], y = vis2d$grid$x2[8])
  plt2d = vis2d$plot(flatten = TRUE)
  dep2d = vapply(plt2d$dependencies, `[[`, character(1), "name")
  expect_true("mathjax" %in% dep2d)

  vis3d = as_visualizer(obj2d, type = "surface")
  vis3d$add_annotation(text = "$\\theta$", latex = TRUE, x = vis3d$grid$x1[2], y = vis3d$grid$x2[5], z = vis3d$zmat[2, 5])
  plt3d = vis3d$plot(flatten = FALSE)
  dep3d = vapply(plt3d$dependencies, `[[`, character(1), "name")
  expect_true("mathjax" %in% dep3d)
})

test_that("latex annotations require latex2exp", {
  skip_if_not_installed("latex2exp")
  vis = as_visualizer(obj("TF_gaussian1", xdim = 1), type = "1d", x1_limits = c(-2, 2))
  expect_no_error(vis$add_annotation(text = "this is an alpha: $\alpha$", latex = TRUE, x = 0)$plot())
})
