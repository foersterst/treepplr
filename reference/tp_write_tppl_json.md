# Write an R list to a TreePPL JSON input file

Writes `x` with the JSON settings TreePPL needs: length-1 vectors become
plain values (`"rho": 0.5`, not `"rho": [0.5]`), numbers keep their full
precision, and the file is indented so it is easy to read.

## Usage

``` r
tp_write_tppl_json(x, file)
```

## Arguments

- x:

  A named list, one element per model argument.

- file:

  Path of the JSON file to write.

## Value

`file`, invisibly.

## Details

Each top-level element of `x` must be named exactly like an argument of
the model function (see
[`tp_expected_input()`](http://treeppl.org/treepplr/reference/tp_expected_input.md)).
A vector of length 1 that must stay an array, such as an `Int[]` with a
single value, has to be wrapped in
[`I()`](https://rdrr.io/r/base/AsIs.html).

## Examples

``` r
if (FALSE) { # \dontrun{
phy <- ape::read.tree(text = "((A:4,B:4):6,(C:7,D:7):3);")
tp_write_tppl_json(
  x = list(tree = tp_phylo_to_tpPhylo(phy), rho = 0.5),
  file = "testdata_crbd.json"
)
} # }
```
