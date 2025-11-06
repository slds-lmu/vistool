# Convert hex color to plotly RGBA format

Converts a hex color to plotly-compatible RGBA format with specified
alpha.

## Usage

``` r
hex_to_rgba(hex_color, alpha = 1)
```

## Arguments

- hex_color:

  (`character(1)`)  
  Hex color code (e.g., "#1f77b4").

- alpha:

  (`numeric(1)`)  
  Alpha transparency value between 0 and 1.

## Value

A character string in RGBA format.
