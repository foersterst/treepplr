# Print the result of a TreePPL input check

Console method for objects returned by
[`tp_input_check()`](http://treeppl.org/treepplr/reference/tp_input_check.md).
A match is shown with a green tick; otherwise missing arguments are
shown in red, extra fields in yellow, and a pointer to
[`tp_expected_input()`](http://treeppl.org/treepplr/reference/tp_expected_input.md).

## Usage

``` r
# S3 method for class 'tpIcheck'
print(x, ...)
```

## Arguments

- x:

  An object of class `tpIcheck`.

- ...:

  Not used; present for compatibility with
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

`x`, invisibly.
