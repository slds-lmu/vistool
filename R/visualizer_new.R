#' @title Visualizer
#'
#' Visualizer base class
#'
#' This class is used to create visualizations/animations.
#' The used plotting backends are plotly (https://plotly.com/) and ggplot2
#' (https://ggplot2.tidyverse.org//).
#' @export
Visualizer = R6::R6Class(
  "Visualizer",
  public = list(
    
    grid = list(),
    zmat = NULL,
    
    initialize = function() {
      stop("Abstract Visualizer class not implemented")
    },

    initLayerUnivariate = function(theme = theme_bw(), ...) {
      private$p_layer_primary = "line"
      private$p_check_grid_dims(self$grid, 1)
      private$p_check_zmat_dims(self$zmat, 0)
      llp = data.table(x = self$grid$x1, y = self$zmat)
      ggplot2::theme_set(theme)
      private$p_plot <- ggplot(llp, aes(x, y))
      return(invisible(self))
    },
    
    initLayerBivariate = function(
      type = "contour",
      opacity = 0.8, 
      colorscale = list(c(0, 1), c("rgb(176,196,222)", "rgb(160,82,45)")), 
      line = list(width = 1, color = "black"),
      ...
    ) {
      checkmate::assertNumber(opacity, lower = 0, upper = 1)
      checkmate::assertList(colorscale)
      checkmate::assertList(line)
      private$p_layer_primary = type
      private$p_vbase = c(as.list(environment()), list(...))
      private$p_check_grid_dims(self$grid, 2)
      private$p_check_zmat_dims(self$zmat, 2)
      llp = list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)
      private$p_plot = plot_ly() %>%
        add_trace(
          # name = self$objective$label,
          showlegend = FALSE,
          showscale = FALSE,
          x = llp$x,
          y = llp$y,
          z = t(llp$z),
          type = type,
          opacity = opacity,
          colorscale = colorscale,
          ...
        )
      if (type == "contour") {
        private$p_plot <- style(private$p_plot, line = line)
      }
      # Used in animate to not overwrite the plot over and over again when 
      # calling `$initLayerXXX`.
      if (!private$p_freeze_plot) {
        private$p_opts = list()     
        private$p_layer_arrow = list()
      }
      return(invisible(self))
    },
    
    setAxes = function() {
      # TODO
    },
    
    setLayout = function(...) {
      if (!(private$p_layer_primary %in% c("contour", "surface"))) {
        stop("Layout can only be set for `contour` and `surface` plots")
      }
      private$p_layout = list(...)
      private$p_plot = private$p_plot %>% 
        layout(...)
      return(invisible(self))
    },
    
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
    
    animate = function(
      dir = "animation", 
      nframes = 10L, 
      view_start = list(x = 1, y = 1, z = 1),
      view_end = list(x = 1, y = 1, z = 1), 
      fext = "png", 
      stops = NULL, 
      ...
    ) {
      checkmate::assertDirectory(dir)
      checkmate::assertCount(nframes)
      checkmate::assertList(view_start)
      checkmate::assertList(view_end)
      checkmate::assertIntegerish(
        stops, lower = 1, len = nframes, null.ok = TRUE
      )
      
      cxyz <- c("x", "y", "z")
      if (!all((names(view_start) == cxyz) | (names(view_end) == cxyz))) {
        stop("View needs `x`, `y`, and `z` coordinates.")
      }
      invisible(lapply(c(view_start, view_end), checkmate::assertNumber))
  
      views = list(
        x = seq(view_start$x, view_end$x, len = nframes),
        y = seq(view_start$y, view_end$y, len = nframes),
        z = seq(view_start$z, view_end$z, len = nframes))
      
      if (is.null(stops)) {
        maxcalls = max(
          vapply(private$p_opts, function(oo) nrow(oo$opt$archive), integer(1))
        )
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
          do.call(
            self$addLayerOptimizationTrace, 
            mlr3misc::insert_named(private$p_opts[[j]], list(npmax = stops[i]))
          )
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
    
    plot = function() {private$p_plot},
    
    save = function(...) {
      private$checkInit()
      if ("ggplot" %in% class(private$p_plot)) {
        ggsave(plot = private$p_plot, ...)
      }
      else if ("plotly" %in% class(private$p_plot)) {
        save_image(p = private$p_plot, ...)
      }
      else stop("No method to save image")
    }
    
  ),
  
  private = list(
    # @field p_layer_primary (`character(1)`) The id of the primary layer. 
    # Used to determine # the trace setup.
    p_layer_primary = NULL,
    # @field p_layer_arrow (`list()`) Arguments passed to `$addLayerArrow()` to 
    # reconstruct the plot for animations.
    p_layer_arrow = list(),
    # @field p_plot (`plot_ly()`) The plot.
    p_plot = NULL,
    # @field p_opts (`list()`) List of objects used to add traces. 
    # Each `$initLayerXXX()` resets this list. An object is added after each 
    # call to `$addLayerXXX()`. This private field is exclusively used to create
    # animations with `$animate()`.
    p_freeze_plot = FALSE,
    p_opts = list(),
    p_vbase = list(),
    p_layout = list(),
    checkInit = function() {
      if (is.null(private$p_plot)) stop("Initialize plot with `initLayer*`")
      return(invisible(TRUE))
    },
    p_set_limits_univariate = function() {
      # TODO
    },
    p_set_limits_bivariate = function() {
      # TODO
    },
    p_check_grid_dims = function(grid, d) {
      checkmate::assertIntegerish(d, lower = 1, upper = 2)
      checkmate::assertList(grid, "numeric")
      checkmate::assertTRUE(length(grid) == d)
    },
    p_check_zmat_dims = function(zmat, d) {
      checkmate::assertIntegerish(d, lower = 0, upper = 2)
      checkmate::assertNumeric(zmat)
      checkmate::assertTRUE(length(dim(zmat)) == d)
    }
  )
)


Tester = R6::R6Class(
  "Tester",
  inherit = Visualizer,
  public = list(
    grid = NULL,
    zmat = NULL,
    initialize = function(data) {
      self$grid = list(
        x1 = seq_len(nrow(data))
        # , x2 = seq_len(nrow(data))
      )
      self$zmat = data$y
      # self$zmat = outer(self$grid$x1, self$grid$x2, function(x, y) {
      #   xin = cbind(x, y)
      #   apply(xin, 1, function(x) sum(x))
      # })
      return(invisible(self))
    }
  )
)

foo = Tester$new(data.frame(x = rnorm(10), y = rnorm(10)))
foo$initLayerUnivariate()
foo$plot()
foo$save("foo.png", width = 3)
