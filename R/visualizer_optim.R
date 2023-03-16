#' @title Optimization Visualizer
#'
#' Visualizer for optimization traces
#'
#' This class is used to create visualizations/animations.
#' The used plotting backends are plotly (https://plotly.com/) and ggplot2
#' (https://ggplot2.tidyverse.org//).
#' @export
VisualizerOptim = R6::R6Class(
  "VisualizerOptim",
  inherit = Visualizer,
  public = list(
    
    #' @field objective (`Objective`) The objective which was optimized. 
    #' This object is used to generate the surface/contour lines.
    objective = NULL,
    
    initialize = function(
      objective,
      x1limits = NULL, 
      x2limits = NULL, 
      padding = 0, 
      auto_box = TRUE, 
      npoints = 100L
    ) {
      self$objective = checkmate::assertR6(objective, "Objective")
      checkmate::assertLogical(auto_box, len = 1L)
      checkmate::assertNumber(padding, lower = 0)
      checkmate::assertNumeric(x1limits, len = objective$xdim, null.ok = TRUE)
      checkmate::assertNumeric(x2limits, len = objective$xdim, null.ok = TRUE)
      if (objective$xdim > 2) {
        stop(
          sprintf(
            "1- or 2-dimensional inputs required, but `objective$xdim = %s`",
            objective$xdim
          )
        )
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
      self$grid = list(
        x1 = unique(
          seq(
            min(x1limits) - x1pad, max(x1limits) + x1pad, 
            length.out = npoints
          )
        ),
        x2 = unique(
          seq(
            min(x2limits) - x2pad, max(x2limits) + x2pad, length.out = npoints
          )
        )
      )

      self$zmat = outer(self$grid$x1, self$grid$x2, function(x, y) {
        xin = cbind(x, y)
        apply(xin, 1, function(x) self$objective$eval(x))
      })
      
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
          line = list(color = line_color, width = 8),
          inherit = FALSE
        )
      }
      if (private$p_layer_primary == "contour") {
        ptype = "scatter"
        pargs = list(
          name = name,
          x = xmr$x,
          y = xmr$y,
          marker = list(color = line_color, size = 12, line = list(color = mcolor_out, width = 2)),
          line = list(color = line_color, width = 2),
          inherit = FALSE
        )
      }
      if (is.null(ptype)) {
        stop("No known plot mode")
      }
      pargs$type = ptype
      
      # Add optimization traces as lines to the plot:
      private$p_plot = do.call(
        add_trace, 
        c(
          list(private$p_plot),
          mlr3misc::insert_named(
            mlr3misc::remove_named(pargs, "marker"), aargs
          ),
          list(mode = "lines")
        )
      )
      
      # Now add marker to the lines. Marker are added at `add_marker_at` iterations
      # and with the specified shape and color:
      pargs = list(
        x = xmarkers$x[add_marker_at],
        y = xmarkers$y[add_marker_at],
        z = xmarkers$z[add_marker_at],
        mode = "markers",
        type = ptype,
        marker = insert_named(
          pargs$marker, list(symbol = marker_shape, color = marker_color)
        ),
        showlegend = FALSE,
        inherit = FALSE
      )
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
        
        private$p_plot = private$p_plot %>% 
          add_trace(
            x = mx, 
            y = my, 
            mode = "lines", 
            type = "scatter", 
            showlegend = FALSE, 
            inherit = FALSE,
            ...
          )
        
      }
      if (private$p_layer_primary == "surface") {
        v1 = v1 * x1length + x0
        v2 = v2 * x2length + x0
        
        v0 = c(x0, f0)
        v1 = c(v1, f0)
        v2 = c(v2, f0)
        
        # Order is important to have the angle:
        marker = cbind(v1, v0, v2)
        
        private$p_plot = private$p_plot %>% 
          add_trace(
            x = marker[1, ], 
            y = marker[2, ], 
            z = marker[3, ], 
            mode = "lines", 
            type = "scatter3d", 
            showlegend = FALSE, 
            inherit = FALSE,
            ...
          )
      }
    },
    
    #' @description Set the layout of the plotly plot.
    #' @param ... Layout options directly passed to `layout(...)`.
    setLayout = function(...) {
      private$p_layout = list(...)
      private$p_plot = private$p_plot %>% layout(...)
      
      return(invisible(self))
    }
  
  ),
  
  private = list(
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
