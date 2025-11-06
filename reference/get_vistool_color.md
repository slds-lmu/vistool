# Get consistent color from discrete palette

Returns a consistent color from the discrete palette based on index.
Used to ensure the same colors are used across different visualizers.

## Usage

``` r
get_vistool_color(index, palette = "discrete", base_palette = NULL)
```

## Arguments

- index:

  (`integer(1)`)  
  Index of the color to retrieve from the palette.

- palette:

  (`character(1)`)  
  Name of the color palette to use. Default is "discrete".

- base_palette:

  (`character(1)`)  
  Base palette name (e.g., "viridis", "plasma", "grayscale") to derive
  discrete colors from when palette is "discrete". Optional.

## Value

A character string containing the color in hex format.
