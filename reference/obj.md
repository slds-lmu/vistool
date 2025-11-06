# Retrieve objective functions

Retrieve an objective function from the dictionary.

## Usage

``` r
obj(.key, ...)
```

## Arguments

- .key:

  (`character(1)`)  
  Key passed to the respective
  [dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  to retrieve the object.

- ...:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named arguments passed to the constructor, or to be set as public
  field. See
  [`mlr3misc::dictionary_sugar_get()`](https://mlr3misc.mlr-org.com/reference/dictionary_sugar_get.html)
  for more details.
