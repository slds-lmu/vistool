# ggplot2 theme matching vistool defaults

ggplot2 theme matching vistool defaults

## Usage

``` r
theme_vistool(theme = NULL, ...)
```

## Arguments

- theme:

  Optional vistool theme object. Falls back to the active
  [`vistool_theme()`](https://slds-lmu.github.io/vistool/reference/vistool_theme.md)
  (global default) when `NULL`.

- ...:

  Additional arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  to override defaults for a specific plot.

## Value

A [`ggplot2::theme`](https://ggplot2.tidyverse.org/reference/theme.html)
object that can be composed via `+` or passed to
[`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html).

## Examples

``` r
ggplot2::theme_set(theme_vistool())

ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() +
  theme_vistool(legend.position = "bottom")
```
