# Convert an R matrix into a TreePPL `Matrix[Real]`

TreePPL stores a matrix as `{"m": rows, "n": columns, "arr": values}`
with the values listed row by row. R stores matrices column by column,
so the matrix is transposed before it is flattened.

## Usage

``` r
tp_matrix_to_tppl(mat)
```

## Arguments

- mat:

  A numeric matrix.

## Value

A list with elements `m`, `n` and `arr`, ready for
[`tp_write_tppl_json()`](http://treeppl.org/treepplr/reference/tp_write_tppl_json.md).

## Examples

``` r
q <- matrix(c(-1, 1, 1, -1), nrow = 2)
tp_matrix_to_tppl(mat = q)
#> $m
#> [1] 2
#> 
#> $n
#> [1] 2
#> 
#> $arr
#> [1] -1  1  1 -1
#> 
```
