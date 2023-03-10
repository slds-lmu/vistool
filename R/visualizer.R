#' Randomly generate colors
#' @description Helper function to generate RGB colors.
#' @param alpha (`numeric(1)`) The alpha value. If `!is.null` the used prefix is 'rgba' instead of 'rgb'.
#' @return A character of length one containing the RGB color.
#' @export
colSampler = function(alpha = NULL) {
  checkmate::assertNumber(alpha, lower = 0, upper = 1, null.ok = TRUE)
  r = sample(seq(0, 255), 1)
  g = sample(seq(0, 255), 1)
  b = sample(seq(0, 255), 1)

  if (is.null(alpha)) {
    rgb = "rgb"
  } else {
    rgb = "rgba"
  }
  clr = sprintf("%s(%s)", rgb, paste(c(r, g, b, alpha), collapse = ", "))
  return(clr)
}

#' Visualizer class
#'
#' This class is used to create visualizations/animations of optimization traces.
#' The used plotting backend is plotly (https://plotly.com/).
#' @export
Visualizer = R6::R6Class("Visualizer",
  public = list(
    #' @field objective (`Objective`) The objective which was optimized. This object
    #' is used to generate the surface/contour lines.
    objective = NULL,

    #' @field grid (`list()`) List with the `x1` and `x2` grid.
    grid = NULL,

    #' @field zmat (`matrix()`) The result of `objective$eval(x1, x2)` at each element of
    #' the cross product of `grid$x1` and `grid$x2`.
    zmat = NULL,

    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #' @param objective (`Objective`) The objective which was optimized. This object
    #' is used to generate the surface/contour lines.
    #' @param x1limits (`numeric(2)`) The x1 limits. Are set automatically if `auto_box = TRUE`.
    #' @param x2limits (`numeric(2)`) The x2 limits. Are set automatically if `auto_box = TRUE`.
    #' @param padding (`numeric(1)`) A margin that is added to x1limits and x2limits.
    #' The x1 margin is calculated by `max(x1lmits) - min(x1limits) * padding`.
    #' @param auto_box (`logical(1)`) If `TRUE`, the limits defined in `objective$limits_lower`
    #' and `objective$limits_upper` are used. Note: If the limits are not defined, `x1limits` and
    #' `x2limits` must be set.
    #' @param npoints (`integer(1)`) The number of generated point per dimension. Note that
    #' a grid of `npoints^2` values is generated and evaluated by `objective$eval(x)` to plot the surface.
    initialize = function(objective, x1limits = NULL, x2limits = NULL, padding = 0, auto_box = TRUE, npoints = 100L) {
      self$objective = checkmate::assertR6(objective, "Objective")
      checkmate::assertLogical(auto_box, len = 1L)
      checkmate::assertNumber(padding, lower = 0)
      checkmate::assertNumeric(x1limits, len = objective$xdim, null.ok = TRUE)
      checkmate::assertNumeric(x2limits, len = objective$xdim, null.ok = TRUE)
      if (objective$xdim != 2) {
        stop(sprintf("`Visualizer` requires 2-dimensional inputs, but `objective$xdim = %s`", objective$xdim))
      }
      checkmate::assertCount(npoints)

      ll = objective$limits_lower
      lu = objective$limits_upper

      invalid_limits = is.na(ll[1]) || is.na(lu[1])

      if (all(! is.null(x1limits), ! is.null(x2limits), auto_box)) {
        auto_box = FALSE
        if (! invalid_limits) {
          warning("Both limits are set, `auto_box = FALSE` is used and values in `objective$limits_lower` and `objecitve$limits_upper` are ignored")
        }
      }

      if (auto_box) {
        if ((! is.null(x1limits)) || (! is.null(x2limits))) {
          warning("`x1limits` / `x2limits` are ignored with `auto_box = TRUE`")
        }

        if (invalid_limits) {
          stop("Was not able to extract limits, please use `x1limits` and `x2limits`")
        }

        x1limits = c(ll[1], lu[1])
        x2limits = c(ll[2], lu[2])

        x1pad = (max(x1limits) - min(x1limits)) * padding
        x2pad = (max(x2limits) - min(x2limits)) * padding
      } else {
        if (is.null(x1limits) || is.null(x2limits)) {
          stop("Both, `x1limits` and `x2limits` are required")
        }
      }

      x1pad = (max(x1limits) - min(x1limits)) * padding
      x2pad = (max(x2limits) - min(x2limits)) * padding
      self$grid = list(x1 = unique(seq(min(x1limits) - x1pad, max(x1limits) + x1pad, length.out = npoints)),
        x2 = unique(seq(min(x2limits) - x2pad, max(x2limits) + x2pad, length.out = npoints)))

      self$zmat = outer(self$grid$x1, self$grid$x2, function(x, y) {
        xin = cbind(x, y)
        apply(xin, 1, function(x) self$objective$eval(x))
      })

      return(invisible(self))
    },

    #' @description Initialize the plot with contour lines.
    #' @param opacity (`numeric(1)`) Opacity of the layer.
    #' @param colorscale (`list()`) The coloring of the surface.
    #' @param ... Further arguments passed to `add_trace(...)`.
    initLayerContour = function(opacity = 0.8, colorscale = list(c(0, 1), c("rgb(176,196,222)", "rgb(160,82,45)")), ...) {
      checkmate::assertNumber(opacity, lower = 0, upper = 1)
      checkmate::assertList(colorscale)

      private$p_vbase = c(as.list(environment()), list(...))
      private$p_layer_primary = "contour"

      llp = list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)
      private$p_plot = plot_ly() %>% add_trace(
        name = self$objective$label,
        showlegend = FALSE,
        showscale = FALSE,
        x = llp$x,
        y = llp$y,
        z = t(llp$z),
        type = "contour",
        opacity = opacity,
        colorscale = colorscale,
        ...
      )
      if (! private$p_freeze_plot) {    # Used in animate to not overwrite the
        private$p_opts = list()         # plot over and over again when calling
        private$p_layer_arrow = list()  # `$initLayerXXX`.
      }

      return(invisible(self))
    },

    #' @description Initialize the plot as 3D surface.
    #' @param opacity (`numeric(1)`) Opacity of the layer.
    #' @param colorscale (`list()`) The coloring of the surface.
    #' @param ... Further arguments passed to `add_trace(...)`.
    initLayerSurface = function(opacity = 0.8, colorscale = list(c(0, 1), c("rgb(176,196,222)", "rgb(160,82,45)")), ...) {
      checkmate::assertNumber(opacity, lower = 0, upper = 1)
      checkmate::assertList(colorscale)

      private$p_vbase = c(as.list(environment()), list(...))
      private$p_layer_primary = "surface"

      llp = list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)
      private$p_plot = plot_ly() %>% add_trace(
        name = self$objective$label,
        showlegend = FALSE,
        showscale = FALSE,
        x = llp$x,
        y = llp$y,
        z = t(llp$z),
        type = "surface",
        opacity = opacity,
        colorscale = colorscale,
        ...
      )
      if (! private$p_freeze_plot) { # Used in animate to not overwrite the plot over and over again.
        private$p_opts = list()
        private$p_layer_arrow = list()
      }

      return(invisible(self))
    },

    #' @description Add an optimization trace.
    #' @param opt (`Optimizer`) The optimizer from which the archive is extracted and used to
    #' plot the trace.
    #' @param line_color (`character(1)`) The color of the trace.
    #' @param mcolor_out (`character(1)`) The outer line color of the marker.
    #' @param npoints (`integer(1)`) The number of used points from the archive. Default is `NULL`
    #' which means that all points are used. If set, a sequence from 1 to `nrow(opt$archive)` is
    #' created.
    #' @param npmax (`integer(1)`) The number of points used from the sequence `seq_len(nrow(opt$archive))[seq_len(npmax)]`
    #' @param name (`character(1)`) The name of the trace in the legend. Default is `NULL`
    #' which means that the name is pasted from `opt$id` and `objective$id`.
    #' @param offset (`numeric(3)`) Trace shift in direction (x, y, z).
    #' @param add_marker_at (`integer()`) Vector of iterations at which a marker is added.
    #' @param marker_shape (`character()`) Vector indicating the shape of the markers.
    #' if `length(marker_shape) == 1`, all markers get the same shape. The other option is to
    #' specify all markers individually by passing a vector of `length(add_marker_at)`.
    #' For a list of all shapes see `schema(F)$traces$XXX$attributes$marker$symbol$values` with `XXX` one of `scatter` or `scatter3d`. If `marker_shape = NA`, no marker
    #' are added.
    #' @param marker_color (`character()`) The colors for the markers.
    #' @param ... Further arguments passed to `add_trace(...)`.
    addLayerOptimizationTrace = function(opt, line_color = colSampler(), mcolor_out = "black",
      npoints = NULL, npmax = NULL, name = NULL, offset = NULL, add_marker_at = 1,
      marker_shape = "circle", marker_color = NULL, ...) {

      ## Basic asserts:
      private$checkInit()

      checkmate::assertR6(opt, "Optimizer")
      checkmate::assertCount(npoints, null.ok = TRUE)
      checkmate::assertCount(npmax, null.ok = TRUE)

      if (nrow(opt$archive) == 0) {
        stop("No optimization trace in `opt$archive`. Did you forget to call `opt$optimize(steps)`?")
      }

      checkmate::assertString(line_color)
      if (private$p_layer_primary == "contour") {
        checkmate::assertNumeric(offset, len = 2L, null.ok = TRUE)
        if (is.null(offset)) {
          offset = rep(0, 2)
        }
        offset[3] = 0
      }
      if (private$p_layer_primary == "surface") {
        checkmate::assertNumeric(offset, len = 3L, null.ok = TRUE)
        if (is.null(offset)) {
          offset = rep(0, 3)
        }
      }

      # Catch additional arguments:
      aargs = list(...)

      if (! private$p_freeze_plot) { # Just set by animate to not save the optimizer over and over again for each frame.
        private$p_opts = c(private$p_opts, list(c(as.list(environment()), aargs)))
      }

      # Assert marker styling:
      checkmate::assertIntegerish(add_marker_at, lower = 1, upper = nrow(opt$archive))
      if (is.null(marker_shape)) marker_shape = NA
      if (length(marker_shape) == 1) marker_shape = rep(marker_shape, length(add_marker_at))
      mvals = NULL
      if (private$p_layer_primary == "contour") {
        mvals = schema(F)$traces$scatter$attributes$marker$symbol$values
      }
      if (private$p_layer_primary == "surface") {
        mvals = schema(F)$traces$scatter3d$attributes$marker$symbol$values
      }
      invisible(lapply(marker_shape, function(marker_shape) checkmate::assertChoice(marker_shape, choices = c(mvals, NA))))

      if (is.null(marker_color)) marker_color = line_color
      if (length(marker_color) == 1) {
        marker_color = rep(marker_color, length(marker_shape))
      }
      checkmate::assertCharacter(marker_color, len = length(marker_shape), null.ok = TRUE)

      # Define marker coordinates for plotting:
      xmat = do.call(rbind, c(opt$archive$x_in[1], opt$archive$x_out))
      xmarkers = data.frame(x = xmat[, 1] + offset[1], y = xmat[, 2] + offset[2],
        z = c(opt$archive$fval_in[1], opt$archive$fval_out) + offset[3])
      if (is.null(npoints)) {
        npoints = nrow(xmarkers)
      }
      if (is.null(npmax)) {
        npmax = nrow(xmarkers)
        if (npmax > nrow(xmarkers)) {
          npmax = nrow(xmarkers)
        }
      }
      # Cut marker after npmax iterations (if specified):
      xmr = xmarkers[unique(round(seq(1, nrow(xmarkers), length.out = npoints))), ]
      xmr = xmr[seq_len(npmax), ]
      add_marker_at = add_marker_at[add_marker_at <= npmax]

      ptype = NULL
      if (is.null(name)) {
        name = paste0(opt$id, " on ", opt$objective$id)
      }

      # Define the plotting arguments as list, so we can call `do.call` with these arguments. This is
      # more comfortable due to just one call to `add_trace`. Additionally, it is easier to control
      # different default stylings (such as line width) and to recreate the layer with the stored arguments:
      if (private$p_layer_primary == "surface") {
        ptype = "scatter3d"
        pargs = list(
          name = name,
          x = xmr$x,
          y = xmr$y,
          z = xmr$z,
          marker = list(color = line_color, line = list(color = mcolor_out, width = 6)),
          line = list(color = line_color, width = 8))
      }
      if (private$p_layer_primary == "contour") {
        ptype = "scatter"
        pargs = list(
          name = name,
          x = xmr$x,
          y = xmr$y,
          marker = list(color = line_color, size = 12, line = list(color = mcolor_out, width = 2)),
          line = list(color = line_color, width = 2))
      }
      if (is.null(ptype)) {
        stop("No known plot mode")
      }
      pargs$type = ptype

      # Add optimization traces as lines to the plot:
      private$p_plot = do.call(add_trace, c(list(private$p_plot),
        mlr3misc::insert_named(mlr3misc::remove_named(pargs, "marker"), aargs),
        list(mode = "lines")))

      # Now add marker to the lines. Marker are added at `add_marker_at` iterations
      # and with the specified shape and color:
      pargs = list(
        x = xmarkers$x[add_marker_at],
        y = xmarkers$y[add_marker_at],
        z = xmarkers$z[add_marker_at],
        mode = "markers",
        type = ptype,
        marker = insert_named(pargs$marker, list(symbol = marker_shape, color = marker_color)),
        showlegend = FALSE)
      if (private$p_layer_primary == "contour") {
        pargs$z = NULL
      }
      private$p_plot = do.call(add_trace, c(list(private$p_plot), pargs))

      return(invisible(self))
    },

    #' @description Add a Taylor approximation (for 1 and 2 degrees).
    #' @param x0 (`numeric()) ` The point around which the approximation is done.
    #' @param degree (`integer(1)`) The degree of the approximation (only 1 and 2 is implemented).
    #' @param x1margin (`numeric(1)`) The "length" of the hyperplane in direction x1.
    #' @param x2margin (`numeric(1)`) The "length" of the hyperplane in direction x2.
    #' @param npoints_per_dim (`integer(1)`) Number of points per dimension for the plotting grid.
    #' @param zlim (`numeric(2)`) The limits for z. Can be helpful if the hyperplane
    #' has a huge z range and therefore the plot looks ugly.
    #' @param ... Additional parameter passed to `add_surface()`.
    addLayerTaylor = function(x0, degree = 2, x1margin = 0, x2margin = 0, npoints_per_dim = 20L, zlim = NULL, ...) {
      private$checkInit()
      if (private$p_layer_primary != "surface") stop("Atm just available for `surface`")
      checkmate::assertNumeric(x0, len = 2L)
      checkmate::assertIntegerish(degree, len = 1, lower = 1, upper = 2)
      checkmate::assertNumber(x1margin)
      checkmate::assertNumber(x2margin)

      #checkmate::assertNumeric(x1limits, len = 2L, null.ok = TRUE)
      #checkmate::assertNumeric(x2limits, len = 2L, null.ok = TRUE)

      #if (is.null(x1limits)) x1limits = x0[1] + c(-0.05, 0.05)
      #if (is.null(x2limits)) x2limits = x0[2] + c(-0.05, 0.05)

      f0 = self$objective$eval(x0)
      g = self$objective$grad(x0)
      h = self$objective$hess(x0)

      # Create box based on the gradient at x0:
      # - Normalize vector to length x1margin / 2
      # - Calculate perpendicular vector
      # - These vectors define the rotation
      gn = g / l2norm(g)
      gp = rbind(c(0, -1), c(1, 0)) %*% gn

      gn = gn * x1margin / 2
      gp = gp * x2margin / 2

      # - Create grid in (0,0) x (1,1) and rotate w.r.t. to gn and gp:
      rotation = cbind(gn, gp)
      square = as.matrix(expand.grid(x = seq(0, 1, len = npoints_per_dim), y = seq(0, 1, len = npoints_per_dim)))
      grid = square %*% rotation
      grid[, 1] = grid[, 1] - max(grid[, 1]) + (max(grid[, 1]) - min(grid[, 1])) / 2 + x0[1]
      grid[, 2] = grid[, 2] - max(grid[, 2]) + (max(grid[, 2]) - min(grid[, 2])) / 2 + x0[2]

      fapp = function(x) {
        out = f0 + crossprod(g, x - x0) * f0
        if (degree == 2) {
          out = out + 0.5 * f0 * t(x - x0) %*% h %*% (x - x0)
        }
        return(as.numeric(out))
      }
      fappV = function(x, y) {
        X = cbind(x = x, y = y)
        apply(X, 1, fapp)
      }
      z = outer(X = grid[,1], Y = grid[,2], FUN = function(x, y) fappV(x, y))
      if (! is.null(zlim)) {
        checkmate::assertNumeric(zlim, len = 2L)
        z[! between(z, zlim[1], zlim[2])] = NA
      }

      private$p_plot = private$p_plot %>% add_surface(x = grid[, 1], y = grid[, 2], z = t(z), showscale = FALSE, ...)
    },

    #' @description Add two "arrows" as eigenvectors of the Hessian.
    #' @param x0 (`numeric(2)`) The point at which the Hessian is calculated.
    #' @param x1length (`numeric(1)`) The length of the first eigenvector.
    #' @param x2length (`numeric(1)`) The length of the second eigenvector.
    #' @param ... Additional arguments passed to `add_trace`.
    addLayerHessian = function(x0, x1length = 0.1, x2length = 0.1, ...) {
      private$checkInit()
      checkmate::assertNumeric(x0, len = 2L)
      checkmate::assertNumber(x1length)
      checkmate::assertNumber(x2length)

      f0 = self$objective$eval(x0)
      h = self$objective$hess(x0)
      ev = eigen(h)$vectors

      v1 = ev[, 1]
      v2 = ev[, 2]

      if (private$p_layer_primary == "contour") {
        # Transpose x and y to macht contour:
        v1 = v1 * x1length + x0
        v2 = v2 * x2length + x0

        mx = c(v1[1], x0[1], v2[1])
        my = c(v1[2], x0[2], v2[2])

        private$p_plot = private$p_plot %>% add_trace(x = mx, y = my, mode = "lines", type = "scatter", showlegend = FALSE, ...)
      }
      if (private$p_layer_primary == "surface") {
        v1 = v1 * x1length + x0
        v2 = v2 * x2length + x0

        v0 = c(x0, f0)
        v1 = c(v1, f0)
        v2 = c(v2, f0)

        # Order is important to have the angle:
        marker = cbind(v1, v0, v2)

        private$p_plot = private$p_plot %>% add_trace(x = marker[1, ], y = marker[2, ], z = marker[3, ], mode = "lines", type = "scatter3d", showlegend = FALSE, ...)
      }
    },

    #' @description Set the layout of the plotly plot.
    #' @param ... Layout options directly passed to `layout(...)`.
    setLayout = function(...) {
      private$p_layout = list(...)
      private$p_plot = private$p_plot %>% layout(...)

      return(invisible(self))
    },

    #' @description Set the view for a 3D plot.
    #' @param x (`numeric(1)`) The view from which the "camera looks down" to the plot.
    #' @param y (`numeric(1)`) The view from which the "camera looks down" to the plot.
    #' @param z (`numeric(1)`) The view from which the "camera looks down" to the plot.
    setScene = function(x, y, z) {
      checkmate::assertNumber(x)
      checkmate::assertNumber(y)
      checkmate::assertNumber(z)

      private$checkInit()

      if (private$p_layer_primary != "surface") {
        stop("Scene can only be set for `surface` plots")
      }
      private$p_plot = private$p_plot %>%
        layout(scene = list(camera = list(eye = list(x = x, y = y, z = z))))

      return(invisible(self))
    },

    #' @description Return the plot and hence plot it or do further processing.
    plot = function() {
      return(private$p_plot)
    },

    #' @description Create an animation of `$plot()`.
    #' @param dir (`character(1)`) The directory in which all the images are saved.
    #' @param nframes (`integer(1)`) The number of frames.
    #' @param view_start (`list()`) The start view of the animation.
    #' @param view_end (`list()`) The end view of the animation.
    #' @param fext (`character(1)`) The file extension (default is `png`).
    #' @param stops (`integer()`) The step / iteration in the archives of the optimizers
    #' added by `$addLayerOptimizationTrace()` at which a frame is taken. Must have
    #' exact the same length as defined in `nframes`. By default, a sequence with
    #' equidistant points is generated for `stops`.
    #' @param ... Additional arguments passed to `$save(...)`.
    animate = function(dir = "animation", nframes = 10L, view_start = list(x = 1, y = 1, z = 1),
      view_end = list(x = 1, y = 1, z = 1), fext = "png", stops = NULL, ...) {

      checkmate::assertDirectory(dir)
      checkmate::assertCount(nframes)
      checkmate::assertList(view_start)
      checkmate::assertList(view_end)
      checkmate::assertIntegerish(stops, lower = 1, len = nframes, null.ok = TRUE)

      if (! all((names(view_start) == c("x", "y", "z")) | (names(view_end) == c("x", "y", "z")))) {
        stop("View needs `x`, `y`, and `z` coordinates.")
      }
      invisible(lapply(c(view_start, view_end), checkmate::assertNumber))

      views = list(
        x = seq(view_start$x, view_end$x, len = nframes),
        y = seq(view_start$y, view_end$y, len = nframes),
        z = seq(view_start$z, view_end$z, len = nframes))

      if (is.null(stops)) {
        maxcalls = max(vapply(private$p_opts, function(oo) nrow(oo$opt$archive), integer(1)))
        stops = unique(round(seq(1, maxcalls, len = nframes)))
      }

      plot_temp = private$p_plot
      private$p_freeze_plot = TRUE

      for (i in seq_len(nframes)) {

        is = stringr::str_pad(i, width = 4, pad = "0")
        fname = sprintf("%s/frame-%s.%s", dir, is, fext)

        if (private$p_layer_primary == "surface") {
          do.call(self$initLayerSurface, private$p_vbase)
        }
        if (private$p_layer_primary == "contour") {
          do.call(self$initLayerContour, private$p_vbase)
        }
        if (is.null(private$p_layer_primary)) {
          stop("No figure was created jet")
        }

        for (j in seq_along(private$p_opts)) {
          do.call(self$addLayerOptimizationTrace, mlr3misc::insert_named(private$p_opts[[j]], list(npmax = stops[i])))
        }

        if (private$p_layer_primary == "surface") {
          self$setScene(x = views$x[i], y = views$y[i], z = views$z[i])
        }
        do.call(self$setLayout, private$p_layout)

        self$save(fname, ...)
      }

      private$p_freeze_plot = FALSE
      private$p_plot = plot_temp

      message(sprintf("Files stored in '%s'. Use, e.g., ImageMagic (http://www.imagemagick.org/) with `convert -delay 20 -loop 0 %s/*.%s myimage.gif` to create gif with 20 ms frames.", dir, dir, fext))
    },

    #' @description Save the plot by using plotlys `orca()` function.
    #' @param ... Further arguments passed to `orca()`.
    save = function(...) {
      private$checkInit()
      orca(private$p_plot, ...)
    }
  ),
  private = list(
    # @field p_layer_primary (`character(1)`) The id of the primary layer. Used to determine
    # the trace setup.
    p_layer_primary = NULL,

    # @field p_layer_arrow (`list()`) Arguments passed to `$addLayerArrow()` to reconstruct the plot for animations.
    p_layer_arrow = list(),

    # @field p_plot (`plot_ly()`) The plot.
    p_plot = NULL,

    # @field p_opts (`list(Optimizer)`) List of optimizers used to add traces. Each `$initLayerXXX()`
    # resets this list. An optimizer is added after each call to `$addLayerOptimizationTrace()`.
    # this private field is exclusively used to create animations with `$animate()`.
    p_opts = list(),

    p_vbase = list(),

    p_layout = list(),

    # @field p_freeze_plot (`logical(1)`) Indicator whether to freeze saving the plot elements.
    p_freeze_plot = FALSE,

    checkInit = function() {
      if (is.null(private$p_plot)) {
        stop("Initialize plot with `initLayer*`")
      }
      return(invisible(TRUE))
    },
    checkInput = function(x) {
      if (private$p_layer_primary == "surface") {
        return(checkmate::assertNumeric(x, len = 3L))
      }
      if (private$p_layer_primary == "contour") {
        return(checkmate::assertNumeric(x, len = 3L))
      }
      stop("Error in `$checkInput()`")
    }
  )
)


if (FALSE) {

Visualizer = R6Class("Visualizer",
  public = list(
    obj = NULL,
    run_archs = NULL,
    x1lab = NULL,
    x2lab = NULL,
    flab = NULL,
    n_grid = NULL,
    x1lim = NULL,
    x2lim = NULL,
    logscale = NULL,

    #FIXME: use base instead of flag for logscale?
    initialize = function(obj, run_archs = NULL, x1lim = c(0, 1), x2lim = c(0, 1),
      n_grid = 50L, x1lab = "x1", x2lab = "x2", flab = "f", col = NULL, logscale = FALSE) {

      # FIXME: col arg in signature not used? typo?
      self$obj = obj
      assert_r6(obj, "Objective")
      if (is.data.table(run_archs))
        run_archs = list(run_archs)
      self$run_archs = assert_list(run_archs, types = "data.table")
      self$run_archs = run_archs
      self$x1lim = assert_numeric(x1lim, len = self$xdim)
      self$x2lim = assert_numeric(x2lim, len = self$xdim)
      self$n_grid = asInt(n_grid, lower = 2)
      self$x1lab = assert_string(x1lab)
      self$x2lab = assert_string(x2lab)
      self$flab = assert_string(flab)
      self$bg_2d_col = terrain_hcl
      self$logscale = assert_flag(logscale)
    },

    get_grid = function() {
      x1seq = seq(self$x1lim[1], self$x1lim[2], length = self$n_grid)
      x2seq = seq(self$x2lim[1], self$x2lim[2], length = self$n_grid)
      g = expand.grid(x1seq, x2seq) # changes var1 first
      fmat = apply(g, 1, self$obj$fun)
      # orders by col in result mat; this means: rows = x1, cols = x2
      fmat = matrix(fmat, self$n_grid, self$n_grid)
      if (self$logscale)
        fmat = log(fmat)
      list(x1seq = x1seq, x2seq = x2seq, fmat = fmat)
    },

    plot_rbase_contour = function(nlevels = 15) {
      # FIXME: reduce margins in plot?
      g = self$get_grid()

      #par(mar = c(4.1, 4.1, 1.1, 1.1))
      image(g$x1seq, g$x2seq, g$fmat, col = self$bg_2d_col, xlab = self$x1lab, ylab = self$x2lab)
      contour(g$x1seq, g$x2seq, g$fmat, nlevels = nlevels, add = TRUE)
      for (i in 1:length(self$run_archs)) {
        a = self$run_archs[[i]]
        for (j in 1:nrow(a)) {
          p = a$x[[j]]
          if (j > 1) {
            q = a$x[[j-1]]
            lines(c(p[1], q[1]), c(p[2], q[2]), col = i + 1)
            points(q[1], q[2], pch = 16, col = i + 1)
          }
          points(p[1], p[2], pch = 16, col = i + 1)
        }
      }
    },


    # label z axis
    plot_rbase_3dsurf = function(theta = 40, phi = 40, ticktype = "detailed",
      pers_lwd = 0.5, run_lwd = 3) {

      # FIXME: reduce margins in plot?
      g = self$get_grid()
      #par(mar = c(4.1, 4.1, 1.1, 1.1))
      m = nrow(g$fmat); n = ncol(g$fmat)
      zfacet = g$fmat[-1, -1] + g$fmat[-1, -n] + g$fmat[-m, -1] + g$fmat[-m, -n] # FIXME what happens here?
      # FIXME: check what happens here with col and doc
      facetcol = cut(zfacet, length(self$bg_2d_col))
      pmat = persp(g$x1seq, g$x2seq, g$fmat, col = self$bg_2d_col[facetcol], theta = theta, phi = phi,
        ticktype = ticktype, xlab = self$x1lab, ylab = self$x2lab, zlab = self$flab, lwd = pers_lwd)

      for (i in 1:length(self$run_archs)) {
        a = self$run_archs[[i]]
        for (j in 1:nrow(a)) {
          p = a$x[[j]]
          pf = a$fval[j]
          t3d1 = trans3d(p[1], p[2], pf, pmat)
          if(j > 1) {
            q = a$x[[j-1]]
            qf = a$fval[j-1]
            t3d2 = trans3d(q[1], q[2], qf, pmat)
            lines(c(t3d1$x, t3d2$x), c(t3d1$y, t3d2$y), lwd = run_lwd, col = i + 1)
            points(x = t3d2$x, y = t3d2$y, pch = 16, col = i + 1)
          }
          points(x = t3d1$x, y = t3d1$y, pch = 16, col = i + 1)
        }
      }
    },


    # FIXME: y und gradnorm zeigen. nochwas?
    plot_y_trace = function() {
      arch = self$run_archs[[1]]$fval
      plot(1, type="n", xlab="Steps", ylab="y",
         xlim=c(1, length(arch)), ylim=c(0,max(arch)))

      for (i in 1:length(self$run_archs)) {
        a = self$run_archs[[i]]
        points(x = 1:length(a$fval), y = a$fval, pch = 16, col = i + 1)
        lines(a$fval, pch=16, col=i+1)
      }

    }

  ),

  active = list(
    bg_2d_col = function(col) {
      if (missing(col)) return(private$.bg_2d_col)
      if (is.function(col)) # create n^2 size colmap we can index later
        col = col(self$n_grid * self$n_grid)
      private$.bg_2d_col = col
    }
  ),

  private = list(
    .bg_2d_col = NULL
  )
)

}
