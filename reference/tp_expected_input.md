# List the input arguments a TreePPL model expects

Scans a `.tppl` file, finds its `model function` signature and reports
the name of every input argument together with the R object you should
supply for it. The result is printed to the console and also returned,
invisibly, so it can be stored and reused (for example to check a data
list before writing it to JSON).

## Usage

``` r
tp_expected_input(tppl)
```

## Arguments

- tppl:

  Path to a `.tppl` model file.

## Value

Invisibly, a list of class `tpEdata` with elements:

- `model_file`: the path given in `tppl`.

- `model_name`: the model's name, i.e. the file name without `.tppl`
  (`"qt"` for `qt.tppl`).

- `model_function`: the name of the `model function` inside the file,
  which can differ from `model_name` (`"qtbirds"` in `qt.tppl`).

- `expect_args`: character vector of argument names, in signature order.

- `expect_types`: character vector of the TreePPL type of each argument.

- `expect_class`: character vector of the matching R object.

Models without arguments return zero-length vectors.

## Details

TreePPL types are translated to R objects as follows:

|                                   |                     |
|-----------------------------------|---------------------|
| TreePPL type                      | R object            |
| `Real`                            | numeric             |
| `Int`                             | integer             |
| `Bool`                            | logical             |
| `String`                          | character           |
| `Real[]`, `Int[]`, ...            | numeric vector, ... |
| `Real[][]`, `Int[][]`, ...        | numeric matrix, ... |
| `Matrix[Real]`                    | matrix              |
| any recursive tree type           | phylo               |
| anything else (e.g. record types) | the TreePPL type    |

A type counts as a tree when it is `ClockTree` (TreePPL's built-in
tree), an alias of a tree type, or a type defined in the model file (or
in a file it imports with a relative path) whose fields refer to the
type itself, e.g. `Node {left: Tree, right: Tree, age: Real}`.

## See also

[`print.tpEdata()`](http://treeppl.org/treepplr/reference/print.tpEdata.md)
for the console output.

## Examples

``` r
if (FALSE) { # \dontrun{
tp_expected_input(tppl = "models/diversification/clads2.tppl")

k <- tp_expected_input(tppl = "models/diversification/clads2.tppl")
k$expect_args
#> [1] "tree" "rho"
} # }
```
