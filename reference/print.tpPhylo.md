# Print a TreePPL tree

Console method for objects returned by
[`tp_phylo_to_tpPhylo()`](http://treeppl.org/treepplr/reference/tp_phylo_to_tpPhylo.md).
Like ape's `print.phylo()`, it gives the number of tips and internal
nodes and the root age.

## Usage

``` r
# S3 method for class 'tpPhylo'
print(x, ...)
```

## Arguments

- x:

  An object of class `tpPhylo`.

- ...:

  Not used; present for compatibility with
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

`x`, invisibly.
