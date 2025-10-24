#' @title Plotly export helpers via webshot2 + magick
#'
#' @description
#' Internal helper functions that persist plotly widgets either as self-contained
#' HTML files or rasterized images rendered through `webshot2` and processed with
#' `magick`. These utilities replace the previous Kaleido-based export path and
#' avoid Python dependencies.
#' @keywords internal
#' @name plotly_export
#' @noRd
NULL

.vistool_save_plotly_widget = function(widget, path, selfcontained = TRUE, background = "transparent", ...) {
  target_dir = dirname(path)
  if (nzchar(target_dir) && !dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  htmlwidgets::saveWidget(
    widget,
    file = path,
    selfcontained = selfcontained,
    background = background,
    ...
  )

  invisible(path)
}

.vistool_save_plotly_image = function(widget, path, width = 800, height = 600, options = list()) {
  background = if (!is.null(options$background)) options$background else "white"
  delay = if (!is.null(options$delay)) options$delay else 0.2
  cliprect = if (!is.null(options$cliprect)) options$cliprect else "viewport"
  zoom = if (!is.null(options$zoom)) options$zoom else 1
  timeout = if (!is.null(options$timeout)) options$timeout else NULL
  options$zoom = NULL
  options$timeout = NULL

  options$background = NULL
  options$delay = NULL
  options$cliprect = NULL

  tmp_dir = tempfile("vistool_plotly")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(
    {
      if (dir.exists(tmp_dir)) {
        unlink(tmp_dir, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE)

  tmp_html = file.path(tmp_dir, "plot.html")
  tmp_png = file.path(tmp_dir, "plot.png")

  htmlwidgets::saveWidget(
    widget,
    file = tmp_html,
    selfcontained = TRUE,
    background = background
  )

  base_args = list(
    url = normalizePath(tmp_html, winslash = "/"),
    file = tmp_png,
    vwidth = as.integer(width),
    vheight = as.integer(height),
    delay = delay,
    cliprect = cliprect,
    zoom = zoom
  )
  if (!is.null(timeout)) {
    base_args$timeout = timeout
  }

  webshot_args = utils::modifyList(base_args, options, keep.null = TRUE)

  encoded_url = utils::URLencode(base_args$url, reserved = FALSE)
  webshot_args$url = paste0("file://", encoded_url)
  do.call(webshot2::webshot, webshot_args)

  img = magick::image_read(tmp_png)
  img = magick::image_trim(img)

  target_dir = dirname(path)
  if (nzchar(target_dir) && !dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }
  magick::image_write(img, path = path)

  invisible(path)
}
