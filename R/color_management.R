#' @title Color management for vistool
#'
#' @description
#' Unified color management system for consistent colors across
#' ggplot2 and plotly visualizations. Supports both discrete colors
#' (for points, traces, lines) and continuous colorscales (for surfaces).
#'
#' @keywords internal

# Global color palettes and settings
.vistool_colors = list(
  # Default discrete color palette for traces, points, etc.
  discrete = c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  ),

  # Discrete colors for viridis-based palette
  discrete_viridis = c(
    "#440154", "#3b528b", "#21908c", "#5dc863", "#fde725",
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"
  ),

  # Discrete colors for plasma-based palette
  discrete_plasma = c(
    "#0d0887", "#ff7f0e", "#2ca02c", "#d62728", "#f0f921",
    "#1f77b4", "#7e03a8", "#cc4778", "#f89441", "#9467bd"
  ),

  # Discrete colors for grayscale palette
  discrete_grayscale = c(
    "#000000", "#404040", "#808080", "#c0c0c0", "#ffffff",
    "#1f1f1f", "#5f5f5f", "#9f9f9f", "#bfbfbf", "#dfdfdf"
  ),

  # Default continuous color scale for surfaces (viridis-like)
  viridis = list(
    c(0, "#440154"), c(0.25, "#3b528b"), c(0.5, "#21908c"),
    c(0.75, "#5dc863"), c(1, "#fde725")
  ),

  # Alternative continuous color scales
  plasma = list(
    c(0, "#0d0887"), c(0.25, "#7e03a8"), c(0.5, "#cc4778"),
    c(0.75, "#f89441"), c(1, "#f0f921")
  ),

  # Grayscale for monochrome plots
  grayscale = list(
    c(0, "#000000"), c(0.25, "#404040"), c(0.5, "#808080"),
    c(0.75, "#c0c0c0"), c(1, "#ffffff")
  )
)

#' Get consistent color from discrete palette
#'
#' @description
#' Returns a consistent color from the discrete palette based on index.
#' Used to ensure the same colors are used across different visualizers.
#'
#' @param index (`integer(1)`)\cr
#'   Index of the color to retrieve from the palette.
#' @param palette (`character(1)`)\cr
#'   Name of the color palette to use. Default is "discrete".
#' @param base_palette (`character(1)`)\cr
#'   Base palette name (e.g., "viridis", "plasma", "grayscale") to derive
#'   discrete colors from when palette is "discrete". Optional.
#' @return A character string containing the color in hex format.
#' @export
get_vistool_color = function(index, palette = "discrete", base_palette = NULL) {
  checkmate::assert_int(index, lower = 1)
  checkmate::assert_choice(palette, choices = names(.vistool_colors))

  # For discrete palettes, we can derive the palette name from base_palette
  if (palette == "discrete" && !is.null(base_palette)) {
    discrete_palette_name = paste0("discrete_", base_palette)
    if (discrete_palette_name %in% names(.vistool_colors)) {
      palette = discrete_palette_name
    }
  }

  colors = .vistool_colors[[palette]]

  if (grepl("^discrete", palette)) {
    # For discrete colors, cycle through the palette
    color_index = ((index - 1) %% length(colors)) + 1
    return(colors[color_index])
  } else {
    # For continuous scales, return the full scale definition
    return(colors)
  }
}

#' Convert hex color to plotly RGBA format
#'
#' @description
#' Converts a hex color to plotly-compatible RGBA format with specified alpha.
#'
#' @param hex_color (`character(1)`)\cr
#'   Hex color code (e.g., "#1f77b4").
#' @param alpha (`numeric(1)`)\cr
#'   Alpha transparency value between 0 and 1.
#' @return A character string in RGBA format.
#' @keywords internal
hex_to_rgba = function(hex_color, alpha = 1) {
  checkmate::assert_string(hex_color)
  checkmate::assert_number(alpha, lower = 0, upper = 1)

  # Remove # if present
  hex_color = gsub("^#", "", hex_color)

  # Convert to RGB
  r = as.numeric(paste0("0x", substr(hex_color, 1, 2)))
  g = as.numeric(paste0("0x", substr(hex_color, 3, 4)))
  b = as.numeric(paste0("0x", substr(hex_color, 5, 6)))

  return(sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha))
}

#' Validate color specification
#'
#' @description
#' Validates color input, ensuring it's a valid hex code or named color.
#'
#' @param color (`character(1)`)\cr
#'   Color specification. Can be hex code or named color.
#' @return A character string containing the validated color.
#' @importFrom grDevices colors
#' @keywords internal
validate_color = function(color) {
  checkmate::assert_string(color)

  # Validate color (basic check for hex or named colors)
  if (grepl("^#[0-9A-Fa-f]{6}$", color) || color %in% colors()) {
    return(color)
  }

  stop("Invalid color specification: ", color)
}

#' Get continuous colorscale for surface plots
#'
#' @description
#' Returns a continuous colorscale definition suitable for plotly surface plots
#' based on the specified color palette.
#'
#' @param palette (`character(1)`)\cr
#'   Name of the color palette. One of "viridis", "plasma", "grayscale".
#' @return A list defining the colorscale for plotly.
#' @export
get_continuous_colorscale = function(palette = "viridis") {
  checkmate::assert_choice(palette, choices = c("viridis", "plasma", "grayscale"))

  if (palette %in% names(.vistool_colors)) {
    return(.vistool_colors[[palette]])
  } else {
    # Fallback to viridis if palette not found
    warning("Palette '", palette, "' not found, using 'viridis'")
    return(.vistool_colors[["viridis"]])
  }
}
