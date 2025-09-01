#' @title Plotly export helpers using Python Kaleido
#'
#' @description
#' Internal helpers to convert plotly objects to JSON and export static images
#' using Python plotly.io + kaleido (v1+). Centralizes version checks and
#' reticulate py_require declarations.
#' @keywords internal
#' @name py_export
#' @noRd
NULL

# Ensure Python deps are declared and available. Errors if requirements unmet.
.vistool_py_require = function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("reticulate is required for exporting images via Kaleido.")
  }
  if (utils::packageVersion("reticulate") < "1.41.0") {
    stop("reticulate >= 1.41.0 is required.")
  }
  reticulate::py_require(
    packages = c("plotly>=6.1.1", "kaleido>=1.0.0"),
    python_version = ">=3.9,<3.13"
  )
}

# Convert a plotly object to a minimal figure JSON string
.vistool_plotly_to_json = function(p) {
  # Let plotly serialize its own special classes (e.g., zcolor) correctly
  plotly::plotly_json(p, pretty = FALSE)
}

# Write a plotly object to a static image using Kaleido v1
.vistool_write_plotly_image = function(p, filename, width = 800, height = 600, opts = NULL) {
  .vistool_py_require()

  # Build traces/layout and coerce colorscale into a named scale when needed
  built = plotly::plotly_build(p)
  traces = built$x$data
  if (length(traces)) {
    for (i in seq_along(traces)) {
      cs = traces[[i]]$colorscale
      if (!is.null(cs)) {
        # Keep scalar names and proper list-of-pairs as-is
        keep = (is.character(cs) && length(cs) == 1L) ||
          (is.list(cs) && length(cs) > 0 && is.list(cs[[1]]) && length(cs[[1]]) == 2)
        if (!keep) {
          # If vector/matrix of colors, convert to evenly spaced pairs
          if (!is.null(dim(cs))) cs = as.vector(cs)
          if (is.character(cs) && length(cs) > 1) {
            n = length(cs)
            denom = max(1, n - 1)
            traces[[i]]$colorscale = lapply(seq_len(n), function(j) list((j - 1) / denom, cs[[j]]))
          } else {
            # Fallback to a safe named scale
            traces[[i]]$colorscale = "viridis"
          }
        }
      }
      # Drop unsupported/animation fields that may appear in R objects
      if (!is.null(traces[[i]]$frame)) traces[[i]]$frame = NULL
      if (!is.null(traces[[i]]$frames)) traces[[i]]$frames = NULL
      if (!is.null(traces[[i]]$attrs)) traces[[i]]$attrs = NULL
    }
  }

  go = reticulate::import("plotly.graph_objects", delay_load = TRUE)
  kaleido = reticulate::import("kaleido", delay_load = TRUE)
  # Ensure a Chrome/Chromium is available for Kaleido; if missing, instruct user to install
  has_chrome = TRUE
  if (reticulate::py_has_attr(kaleido, "has_chrome")) {
    has_chrome = tryCatch(isTRUE(kaleido$has_chrome()), error = function(e) FALSE)
  } else {
    # Fallback probe: try a tiny export; failure likely means Chrome is unavailable
    tmp_png = tempfile(fileext = ".png")
    has_chrome = tryCatch(
      {
        probe_fig = go$Figure()
        kaleido$write_fig_sync(probe_fig, path = tmp_png, opts = list(width = 1L, height = 1L))
        TRUE
      },
      error = function(e) FALSE)
    if (has_chrome && file.exists(tmp_png)) unlink(tmp_png)
  }
  if (!has_chrome) {
    stop(
      paste(
        "Kaleido requires Chrome/Chromium for static image export, but none was found.",
        "Please install Google Chrome or Chromium and ensure it is discoverable by Kaleido."
      )
    )
  }
  sanitize_ranges = function(ax) {
    if (is.null(ax) || !is.list(ax)) return(ax)
    if (!is.null(ax$range)) {
      rng = ax$range
      if (is.null(rng) || length(rng) != 2) ax$range = NULL
    }
    ax
  }
  sanitize_layout = function(lo) {
    if (is.null(lo) || !is.list(lo)) return(lo)
    # 3D scene
    if (!is.null(lo$scene)) {
      sc = lo$scene
      sc$xaxis = sanitize_ranges(sc$xaxis)
      sc$yaxis = sanitize_ranges(sc$yaxis)
      sc$zaxis = sanitize_ranges(sc$zaxis)
      lo$scene = sc
    }
    # 2D axes (if present)
    lo$xaxis = sanitize_ranges(lo$xaxis)
    lo$yaxis = sanitize_ranges(lo$yaxis)

    # Drop empty lists recursively
    for (nm in names(lo)) {
      val = lo[[nm]]
      if (is.list(val) && length(val) == 0) lo[[nm]] = NULL
    }
    lo
  }
  lay = sanitize_layout(built$x$layout)
  fig_dict = list(data = traces, layout = lay)
  py_fig = go$Figure(fig_dict)
  k_opts = list(width = as.integer(width), height = as.integer(height))
  if (is.list(opts) && length(opts)) k_opts = utils::modifyList(k_opts, opts)
  kaleido$write_fig_sync(py_fig, path = filename, opts = k_opts)
  invisible(TRUE)
}
