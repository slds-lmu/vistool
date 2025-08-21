#' @import checkmate
#' @import mlr3misc
#' @import data.table
#' @import rootSolve
#' @import TestFunctions
#' @import mlr3
#' @import paradox
#' @import colorspace
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_contour geom_contour_filled geom_path geom_raster
#' @importFrom ggplot2 labs theme scale_color_manual scale_color_discrete
#' @importFrom ggplot2 scale_linewidth_manual scale_linetype_manual element_blank
#' @importFrom ggplot2 scale_fill_viridis_c scale_fill_gradient scale_color_viridis_c theme_minimal theme_bw ggsave
#' @importFrom ggplot2 theme_classic theme_gray theme_light theme_dark theme_void element_text
#' @importFrom plotly plot_ly add_trace add_surface layout save_image schema
#' @importFrom data.table between
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stats optimize
#' @importFrom stringr str_pad
#' @importFrom ggsci pal_npg
#' @importFrom reticulate py_require
NULL

.onLoad = function(libname, pkgname) {
  # Initialize default theme if not set
  if (is.null(getOption("vistool.theme"))) {
    options(vistool.theme = vistool_theme())
  }

  # Declare Python dependency for plotly static image export (kaleido)
  # This uses reticulate >= 1.41's py_require() mechanism which will
  # (lazily) provision an ephemeral Python env via uv when Python is
  # first initialized, avoiding manual miniconda setup for typical users.
  if (requireNamespace("reticulate", quietly = TRUE) &&
    utils::packageVersion("reticulate") >= "1.41") {
    # Be conservative: wrap in try to avoid impacting package load
    try(reticulate::py_require("kaleido"), silent = TRUE)
  }
}

.onUnload = function(libpath) {
  options(vistool.theme = NULL)
}
