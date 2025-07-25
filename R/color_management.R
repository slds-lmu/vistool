#' @title Color Management for vistool
#'
#' @description
#' Internal color management system to ensure consistent colors across
#' ggplot2 and plotly visualizations.
#'
#' @keywords internal

# Global color palettes and settings
.vistool_colors <- list(
  # Default discrete color palette for traces, points, etc.
  discrete = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
               "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),
  
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
#' @return A character string containing the color in hex format.
#' @export
get_vistool_color <- function(index, palette = "discrete") {
  checkmate::assert_int(index, lower = 1)
  checkmate::assert_choice(palette, choices = names(.vistool_colors))
  
  colors <- .vistool_colors[[palette]]
  
  if (palette == "discrete") {
    # For discrete colors, cycle through the palette
    color_index <- ((index - 1) %% length(colors)) + 1
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
hex_to_rgba <- function(hex_color, alpha = 1) {
  checkmate::assert_string(hex_color)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  
  # Remove # if present
  hex_color <- gsub("^#", "", hex_color)
  
  # Convert to RGB
  r <- as.numeric(paste0("0x", substr(hex_color, 1, 2)))
  g <- as.numeric(paste0("0x", substr(hex_color, 3, 4)))
  b <- as.numeric(paste0("0x", substr(hex_color, 5, 6)))
  
  return(sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha))
}

#' Generate auto color for new trace/element
#' 
#' @description
#' Automatically assigns a color from the discrete palette and increments
#' the color index. Used internally by add_* methods when color = "auto".
#' 
#' @param visualizer (`Visualizer`)\cr
#'   The visualizer object to get and update color index.
#' @return A character string containing the hex color.
#' @keywords internal
get_auto_color <- function(visualizer) {
  checkmate::assert_r6(visualizer, "Visualizer")
  
  # Get current color index, initialize if not set
  if (is.null(visualizer$.__enclos_env__$private$.color_index)) {
    visualizer$.__enclos_env__$private$.color_index <- 1
  }
  color_index <- visualizer$.__enclos_env__$private$.color_index
  
  # Get color from discrete palette
  color <- get_vistool_color(color_index, "discrete")
  
  # Increment color index for next use
  visualizer$.__enclos_env__$private$.color_index <- color_index + 1
  
  return(color)
}

#' Validate and process color specification
#' 
#' @description
#' Processes color input, handling "auto" assignment and validation.
#' 
#' @param color (`character(1)`)\cr
#'   Color specification. Can be "auto", hex code, or named color.
#' @param visualizer (`Visualizer`)\cr
#'   The visualizer object for auto color assignment.
#' @return A character string containing the processed color.
#' @importFrom grDevices colors
#' @keywords internal
process_color <- function(color, visualizer = NULL) {
  checkmate::assert_string(color)
  
  if (color == "auto") {
    if (is.null(visualizer)) {
      stop("Cannot use 'auto' color without visualizer object")
    }
    return(get_auto_color(visualizer))
  }
  
  # Validate color (basic check for hex or named colors)
  if (grepl("^#[0-9A-Fa-f]{6}$", color) || color %in% colors()) {
    return(color)
  }
  
  stop("Invalid color specification: ", color)
}
