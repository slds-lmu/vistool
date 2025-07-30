#' @title Visualize Objective as Interactive Surface
#'
#' @description
#' This class is used to create interactive surface visualizations and animations of optimization traces
#' for 2D objectives using plotly.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
VisualizerSurfaceObj <- R6::R6Class("VisualizerSurfaceObj",
  inherit = VisualizerSurface,
  public = list(

    #' @template field_objective
    objective = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_objective
    #' @template param_x1_limits
    #' @template param_x2_limits
    #' @template param_padding
    #' @template param_n_points
    #' @template param_opacity
    #' @template param_colorscale
    #' @template param_opacity
    #' @template param_colorscale
    #' @template param_show_title
    initialize = function(objective, x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L,
                          opacity = 0.8, colorscale = list(
                            c(0, "#440154"), c(0.25, "#3b528b"), c(0.5, "#21908c"), 
                            c(0.75, "#5dc863"), c(1, "#fde725")
                          ), show_title = TRUE) {
      self$objective <- checkmate::assert_r6(objective, "Objective")
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(padding)
      checkmate::assert_count(n_points)

      if (objective$xdim != 2) {
        mlr3misc::stopf("`VisualizerSurface` requires 2-dimensional inputs, but `objective$xdim = %s`", objective$xdim)
      }

      x1_limits <- x1_limits %??% c(objective$lower[1], objective$upper[1])
      x2_limits <- x2_limits %??% c(objective$lower[2], objective$upper[2])

      if (any(is.na(x1_limits)) || any(is.na(x2_limits))) {
        stop("Limits could not be extracted from the objective. Please use `x_limits`.")
      }

      x1_pad <- (x1_limits[2] - x1_limits[1]) * padding
      x2_pad <- (x2_limits[2] - x2_limits[1]) * padding

      grid <- list(
        x1 = unique(seq(x1_limits[1] - x1_pad, x1_limits[2] + x1_pad, length.out = n_points)),
        x2 = unique(seq(x2_limits[1] - x2_pad, x2_limits[2] + x2_pad, length.out = n_points))
      )

      zmat <- outer(grid$x1, grid$x2, function(x, y) {
        xin <- cbind(x, y)
        apply(xin, 1, function(x) self$objective$eval(x))
      })

      super$initialize(
        grid = grid,
        zmat = zmat,
        plot_lab = self$objective$label,
        x1_lab = "x1",
        x2_lab = "x2",
        z_lab = "y",
        opacity = opacity,
        colorscale = colorscale,
        show_title = show_title
      )

      # Initialize trace counter for consistent coloring
      private$.trace_count <- 0

      return(invisible(self))
    },

    #' @description
    #' Add an optimization trace.
    #'
    #' @param opt (`Optimizer`)\cr
    #'   The optimizer from which the archive is extracted and used to plot the trace.
    #' @param line_color (`character(1)`)\cr
    #'   The color of the trace.
    #' @param mcolor_out (`character(1)`)\cr
    #'   The outer line color of the marker.
    #' @param npoints (`integer(1)`)\cr
    #'   The number of used points from the archive.
    #'   Default is `NULL` which means that all points are used.
    #'   If set, a sequence from 1 to `nrow(opt$archive)` is created.
    #' @param npmax (`integer(1)`)\cr
    #'   The number of points used from the sequence `seq_len(nrow(opt$archive))[seq_len(npmax)]`
    #' @param name (`character(1)`)\cr
    #'   The name of the trace in the legend.
    #'   Default is `NULL` which means that the name is pasted from `opt$id` and `objective$id`.
    #' @param offset (`numeric(3)`)\cr
    #'   Trace shift in direction (x, y, z).
    #' @param add_marker_at (`integer()`)\cr
    #'   Vector of iterations at which a marker is added.
    #' @param marker_shape (`character()`)\cr
    #'   Vector indicating the shape of the markers.
    #'   If `length(marker_shape) == 1`, all markers get the same shape.
    #'   The other option is to specify all markers individually by passing a vector of `length(add_marker_at)`.
    #'   For a list of all shapes see `schema(F)$traces$XXX$attributes$marker$symbol$values` with `XXX` one of `scatter` or `scatter3d`.
    #'   If `marker_shape = NA`, no marker are added.
    #' @param marker_color (`character()`)\cr
    #'   The colors for the markers.
    #' @param ... Further arguments passed to `add_trace(...)`.
    add_optimization_trace = function(opt, line_color = NULL, mcolor_out = "black", npoints = NULL, npmax = NULL, name = NULL, offset = NULL, add_marker_at = 1, marker_shape = "circle", marker_color = NULL, ...) {
      checkmate::assert_r6(opt, "Optimizer")
      checkmate::assert_count(npoints, null.ok = TRUE)
      checkmate::assert_count(npmax, null.ok = TRUE)
      checkmate::assert_string(line_color, null.ok = TRUE)
      
      # Store layer specification without immediately resolving colors
      # Colors will be resolved in plot() when color_palette is known
      if (is.null(line_color)) {
        line_color <- "auto"  # Mark for automatic color assignment
      }
      
      # Store the optimization trace layer for later processing
      private$store_layer("optimization_trace", list(
        optimizer = opt,
        line_color = line_color,
        mcolor_out = mcolor_out,
        npoints = npoints,
        npmax = npmax,
        name = name,
        offset = offset,
        add_marker_at = add_marker_at,
        marker_shape = marker_shape,
        marker_color = marker_color,
        args = list(...)
      ))
      
      return(invisible(self))
    },

    #' @description
    #' Renders the surface plot with all added layers.
    #'
    #' @param ... (`any`)\cr
    #'   Additional arguments passed to the parent plot method.
    #'
    #' @return The plotly object.
    plot = function(...) {
      # Call parent plot method first to set up plot_settings and resolve colors
      super$plot(...)
      
      # Process stored optimization trace layers
      private$render_optimization_trace_layers()
      
      # Process stored Taylor approximation layers
      private$render_taylor_layers()
      
      # Process stored Hessian eigenvector layers
      private$render_hessian_layers()
      
      return(private$.plot)
    },

    #' @description
    #' Add a Taylor approximation (for 1 and 2 degrees).
    #'
    #' @param x0 (`numeric()) `\cr
    #'   The point around which the approximation is done.
    #' @param degree (`integer(1)`)\cr
    #'   The degree of the approximation (only 1 and 2 is implemented).
    #' @param x1margin (`numeric(1)`)\cr
    #'   The "length" of the hyperplane in direction x1.
    #' @param x2margin (`numeric(1)`)\cr
    #'   The "length" of the hyperplane in direction x2.
    #' @param npoints_per_dim (`integer(1)`)\cr
    #'   Number of points per dimension for the plotting grid.
    #' @param zlim (`numeric(2)`)\cr
    #'   The limits for z.
    #'   Can be helpful if the hyperplane as a huge z range and therefore the plot looks ugly.
    #' @param ... (`any`)\cr
    #'   Additional parameter passed to `add_surface()`.
    add_taylor = function(x0, degree = 2, x1margin = 0, x2margin = 0, npoints_per_dim = 20L, zlim = NULL, ...) {
      checkmate::assertNumeric(x0, len = 2L)
      checkmate::assertIntegerish(degree, len = 1, lower = 1, upper = 2)
      checkmate::assertNumber(x1margin)
      checkmate::assertNumber(x2margin)
      checkmate::assertIntegerish(npoints_per_dim, len = 1, lower = 5)
      checkmate::assertNumeric(zlim, len = 2L, null.ok = TRUE)

      # Store layer specification for later processing
      private$store_layer("taylor", list(
        x0 = x0,
        degree = degree,
        x1margin = x1margin,
        x2margin = x2margin,
        npoints_per_dim = npoints_per_dim,
        zlim = zlim,
        args = list(...)
      ))
      
      return(invisible(self))
    },

    #' @description
    #' Add two "arrows" as eigenvectors of the Hessian.
    #'
    #' @param x0 (`numeric(2)`)\cr
    #'   The point at which the Hessian is calculated.
    #' @param x1length (`numeric(1)`)\cr
    #'   The length of the first eigenvector.
    #' @param x2length (`numeric(1)`)\cr
    #'   The length of the second eigenvector.
    #' @param ... (`any`)\cr
    #'   Additional arguments passed to `add_trace`.
    add_hessian = function(x0, x1length = 0.1, x2length = 0.1, ...) {
      checkmate::assertNumeric(x0, len = 2L)
      checkmate::assertNumber(x1length)
      checkmate::assertNumber(x2length)

      # Store layer specification for later processing
      private$store_layer("hessian", list(
        x0 = x0,
        x1length = x1length,
        x2length = x2length,
        args = list(...)
      ))
      
      return(invisible(self))
    },

    #' @description
    #' Create an animation of `$plot()`.
    #'
    #' @param dir (`character(1)`)\cr
    #'   The directory in which all the images are saved.
    #' @param nframes (`integer(1)`)\cr
    #'   The number of frames.
    #' @param view_start (`list()`)\cr
    #'   The start view of the animation.
    #' @param view_end (`list()`)\cr
    #'   The end view of the animation.
    #' @param fext (`character(1)`)\cr
    #'   The file extension (default is `png`).
    #' @param stops (`integer()`)\cr
    #'   The step / iteration in the archives of the optimizers added by `$addLayerOptimizationTrace()` at which a frame is taken.
    #'   Must have exact the same length as defined in `nframes`.
    #'   By default, a sequence with equidistant points is generated for `stops`.
    #' @param ... (`any`)\cr
    #'   Additional arguments passed to `$save(...)`.
    animate = function(dir = "animation", nframes = 10L, view_start = list(x = 1, y = 1, z = 1),
                       view_end = list(x = 1, y = 1, z = 1), fext = "png", stops = NULL, ...) {
      checkmate::assertDirectory(dir)
      checkmate::assertCount(nframes)
      checkmate::assertList(view_start)
      checkmate::assertList(view_end)
      checkmate::assertIntegerish(stops, lower = 1, len = nframes, null.ok = TRUE)

      if (!all((names(view_start) == c("x", "y", "z")) | (names(view_end) == c("x", "y", "z")))) {
        stop("View needs `x`, `y`, and `z` coordinates.")
      }
      invisible(lapply(c(view_start, view_end), checkmate::assertNumber))

      views <- list(
        x = seq(view_start$x, view_end$x, len = nframes),
        y = seq(view_start$y, view_end$y, len = nframes),
        z = seq(view_start$z, view_end$z, len = nframes)
      )

      if (is.null(stops)) {
        maxcalls <- max(vapply(private$.opts, function(oo) nrow(oo$opt$archive), integer(1)))
        stops <- unique(round(seq(1, maxcalls, len = nframes)))
      }

      plot_temp <- private$.plot
      private$.freeze_plot <- TRUE

      for (i in seq_len(nframes)) {
        is <- stringr::str_pad(i, width = 4, pad = "0")
        fname <- sprintf("%s/frame-%s.%s", dir, is, fext)

        if (private$.layer_primary == "surface") {
          do.call(self$initLayerSurface, private$.vbase)
        }
        if (private$.layer_primary == "contour") {
          do.call(self$initLayerContour, private$.vbase)
        }
        if (is.null(private$.layer_primary)) {
          stop("No figure was created jet")
        }

        for (j in seq_along(private$.opts)) {
          do.call(self$addLayerOptimizationTrace, mlr3misc::insert_named(private$.opts[[j]], list(npmax = stops[i])))
        }

        if (private$.layer_primary == "surface") {
          self$setScene(x = views$x[i], y = views$y[i], z = views$z[i])
        }
        do.call(self$setLayout, private$.layout)

        self$save(fname, ...)
      }

      private$.freeze_plot <- FALSE
      private$.plot <- plot_temp

      message(sprintf("Files stored in '%s'. Use, e.g., ImageMagic (http://www.imagemagick.org/) with `convert -delay 20 -loop 0 %s/*.%s myimage.gif` to create gif with 20 ms frames.", dir, dir, fext))
    }
  ),
  private = list(
    .trace_count = 0,
    
    # Render stored optimization trace layers
    render_optimization_trace_layers = function() {
      # Get all stored optimization trace layers
      trace_layers <- private$get_layers_by_type("optimization_trace")
      
      if (length(trace_layers) == 0) {
        return()
      }
      
      for (trace_spec in trace_layers) {
        private$render_optimization_trace_layer(trace_spec)
      }
    },
    
    # Render a single optimization trace layer
    render_optimization_trace_layer = function(layer_spec) {
      opt <- layer_spec$optimizer
      line_color <- layer_spec$line_color
      mcolor_out <- layer_spec$mcolor_out
      npoints <- layer_spec$npoints
      npmax <- layer_spec$npmax
      name <- layer_spec$name
      offset <- layer_spec$offset
      add_marker_at <- layer_spec$add_marker_at
      marker_shape <- layer_spec$marker_shape
      marker_color <- layer_spec$marker_color
      aargs <- layer_spec$args

      # Resolve color if it's "auto"
      if (line_color == "auto") {
        private$.trace_count <- private$.trace_count + 1
        line_color <- private$get_auto_color_with_palette()
      }
      
      if (is.null(private$.plot)) private$.init_default_plot()

      if (nrow(opt$archive) == 0) {
        stop("No optimization trace in `opt$archive`. Did you forget to call `opt$optimize(steps)`?")
      }

      # Set offset defaults based on layer type
      if (private$.layer_primary == "contour") {
        checkmate::assertNumeric(offset, len = 2L, null.ok = TRUE)
        if (is.null(offset)) {
          offset <- rep(0, 2)
        }
        offset[3] <- 0
      }
      if (private$.layer_primary == "surface") {
        checkmate::assertNumeric(offset, len = 3L, null.ok = TRUE)
        if (is.null(offset)) {
          offset <- rep(0, 3)
        }
      }

      # Add to opts for animation support
      if (!private$.freeze_plot) {
        private$.opts <- c(private$.opts, list(c(layer_spec, list(line_color = line_color))))
      }

      # Assert marker styling:
      checkmate::assertIntegerish(add_marker_at, lower = 1, upper = nrow(opt$archive))
      if (is.null(marker_shape)) marker_shape <- NA
      if (length(marker_shape) == 1) marker_shape <- rep(marker_shape, length(add_marker_at))
      mvals <- NULL
      if (private$.layer_primary == "contour") {
        mvals <- plotly::schema(F)$traces$scatter$attributes$marker$symbol$values
      }
      if (private$.layer_primary == "surface") {
        mvals <- plotly::schema(F)$traces$scatter3d$attributes$marker$symbol$values
      }
      invisible(lapply(marker_shape, function(marker_shape) checkmate::assertChoice(marker_shape, choices = c(mvals, NA))))

      if (is.null(marker_color)) marker_color <- line_color
      if (length(marker_color) == 1) {
        marker_color <- rep(marker_color, length(marker_shape))
      }
      checkmate::assertCharacter(marker_color, len = length(marker_shape), null.ok = TRUE)

      # Define marker coordinates for plotting:
      xmat <- do.call(rbind, c(opt$archive$x_in[1], opt$archive$x_out))
      xmarkers <- data.frame(
        x = xmat[, 1] + offset[1], y = xmat[, 2] + offset[2],
        z = c(opt$archive$fval_in[1], opt$archive$fval_out) + offset[3]
      )
      if (is.null(npoints)) {
        npoints <- nrow(xmarkers)
      }
      if (is.null(npmax)) {
        npmax <- nrow(xmarkers)
        if (npmax > nrow(xmarkers)) {
          npmax <- nrow(xmarkers)
        }
      }
      # Cut marker after npmax iterations (if specified):
      xmr <- xmarkers[unique(round(seq(1, nrow(xmarkers), length.out = npoints))), ]
      xmr <- xmr[seq_len(npmax), ]
      add_marker_at <- add_marker_at[add_marker_at <= npmax]

      ptype <- NULL
      if (is.null(name)) {
        name <- paste0(opt$id, " on ", opt$objective$id)
      }

      # Define the plotting arguments as list, so we can call `do.call` with these arguments. This is
      # more comfortable due to just one call to `add_trace`. Additionally, it is easier to control
      # different default stylings (such as line width) and to recreate the layer with the stored arguments:
      if (private$.layer_primary == "surface") {
        ptype <- "scatter3d"
        pargs <- list(
          name = name,
          x = xmr$x,
          y = xmr$y,
          z = xmr$z,
          marker = list(color = line_color, line = list(color = mcolor_out, width = 6)),
          line = list(color = line_color, width = 8)
        )
      }
      if (private$.layer_primary == "contour") {
        ptype <- "scatter"
        pargs <- list(
          name = name,
          x = xmr$x,
          y = xmr$y,
          marker = list(color = line_color, size = 12, line = list(color = mcolor_out, width = 2)),
          line = list(color = line_color, width = 2)
        )
      }
      if (is.null(ptype)) {
        stop("No known plot mode")
      }
      pargs$type <- ptype

      # Add optimization traces as lines to the plot:
      private$.plot <- do.call(plotly::add_trace, c(
        list(private$.plot),
        mlr3misc::insert_named(mlr3misc::remove_named(pargs, "marker"), aargs),
        list(mode = "lines")
      ))

      # Now add marker to the lines. Marker are added at `add_marker_at` iterations
      # and with the specified shape and color:
      pargs <- list(
        x = xmarkers$x[add_marker_at],
        y = xmarkers$y[add_marker_at],
        z = xmarkers$z[add_marker_at],
        mode = "markers",
        type = ptype,
        marker = mlr3misc::insert_named(pargs$marker, list(symbol = marker_shape, color = marker_color)),
        showlegend = FALSE
      )
      if (private$.layer_primary == "contour") {
        pargs$z <- NULL
      }
      private$.plot <- do.call(plotly::add_trace, c(list(private$.plot), pargs))
    },
    
    # Render stored Taylor approximation layers
    render_taylor_layers = function() {
      # Get all stored Taylor layers
      taylor_layers <- private$get_layers_by_type("taylor")
      
      if (length(taylor_layers) == 0) {
        return()
      }
      
      for (taylor_spec in taylor_layers) {
        private$render_taylor_layer(taylor_spec)
      }
    },
    
    # Render a single Taylor approximation layer
    render_taylor_layer = function(layer_spec) {
      x0 <- layer_spec$x0
      degree <- layer_spec$degree
      x1margin <- layer_spec$x1margin
      x2margin <- layer_spec$x2margin
      npoints_per_dim <- layer_spec$npoints_per_dim
      zlim <- layer_spec$zlim
      aargs <- layer_spec$args
      
      if (is.null(private$.plot)) private$.init_default_plot()
      
      # Check if this is only available for surface mode
      if (private$.layer_primary != "surface") {
        stop("Taylor approximation is currently only available for surface plots")
      }
      
      f0 <- self$objective$eval(x0)
      g <- self$objective$grad(x0)
      h <- self$objective$hess(x0)

      # Create box based on the gradient at x0:
      # - Normalize vector to length x1margin / 2
      # - Calculate perpendicular vector
      # - These vectors define the rotation
      gn <- g / l2norm(g)
      gp <- rbind(c(0, -1), c(1, 0)) %*% gn

      gn <- gn * x1margin / 2
      gp <- gp * x2margin / 2

      # - Create grid in (0,0) x (1,1) and rotate w.r.t. to gn and gp:
      rotation <- cbind(gn, gp)
      square <- as.matrix(expand.grid(x = seq(0, 1, len = npoints_per_dim), y = seq(0, 1, len = npoints_per_dim)))
      grid <- square %*% rotation
      grid[, 1] <- grid[, 1] - max(grid[, 1]) + (max(grid[, 1]) - min(grid[, 1])) / 2 + x0[1]
      grid[, 2] <- grid[, 2] - max(grid[, 2]) + (max(grid[, 2]) - min(grid[, 2])) / 2 + x0[2]

      fapp <- function(x) {
        out <- f0 + crossprod(g, x - x0) * f0
        if (degree == 2) {
          out <- out + 0.5 * f0 * t(x - x0) %*% h %*% (x - x0)
        }
        return(as.numeric(out))
      }
      fappV <- function(x, y) {
        X <- cbind(x = x, y = y)
        apply(X, 1, fapp)
      }
      z <- outer(X = grid[, 1], Y = grid[, 2], FUN = function(x, y) fappV(x, y))
      if (!is.null(zlim)) {
        z[!between(z, zlim[1], zlim[2])] <- NA
      }

      private$.plot <- do.call(plotly::add_surface, c(list(private$.plot, x = grid[, 1], y = grid[, 2], z = t(z), showscale = FALSE), aargs))
    },
    
    # Render stored Hessian eigenvector layers
    render_hessian_layers = function() {
      # Get all stored Hessian layers
      hessian_layers <- private$get_layers_by_type("hessian")
      
      if (length(hessian_layers) == 0) {
        return()
      }
      
      for (hessian_spec in hessian_layers) {
        private$render_hessian_layer(hessian_spec)
      }
    },
    
    # Render a single Hessian eigenvector layer
    render_hessian_layer = function(layer_spec) {
      x0 <- layer_spec$x0
      x1length <- layer_spec$x1length
      x2length <- layer_spec$x2length
      aargs <- layer_spec$args
      
      if (is.null(private$.plot)) private$.init_default_plot()

      f0 <- self$objective$eval(x0)
      h <- self$objective$hess(x0)
      ev <- eigen(h)$vectors

      v1 <- ev[, 1]
      v2 <- ev[, 2]

      if (private$.layer_primary == "contour") {
        # Transpose x and y to match contour:
        v1 <- v1 * x1length + x0
        v2 <- v2 * x2length + x0

        mx <- c(v1[1], x0[1], v2[1])
        my <- c(v1[2], x0[2], v2[2])

        private$.plot <- do.call(plotly::add_trace, c(list(private$.plot, x = mx, y = my, mode = "lines", type = "scatter", showlegend = FALSE), aargs))
      }
      if (private$.layer_primary == "surface") {
        v1 <- v1 * x1length + x0
        v2 <- v2 * x2length + x0

        v0 <- c(x0, f0)
        v1 <- c(v1, f0)
        v2 <- c(v2, f0)

        # Order is important to have the angle:
        marker <- cbind(v1, v0, v2)

        private$.plot <- do.call(plotly::add_trace, c(list(private$.plot, x = marker[1, ], y = marker[2, ], z = marker[3, ], mode = "lines", type = "scatter3d", showlegend = FALSE), aargs))
      }
    }
  )
)
