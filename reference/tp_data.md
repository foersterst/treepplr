# Write the JSON input file for a TreePPL model

`tp_data()` writes the R objects in `data` to a JSON file that a TreePPL
sampler can read, and checks the file against the model's arguments.
Pass the result to
[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md).

## Usage

``` r
tp_data(model, data, filename = NULL, check = TRUE, print = TRUE)
```

## Arguments

- model:

  Name of a model in the TreePPL model library (see
  [`tp_model_library()`](http://treeppl.org/treepplr/reference/tp_model_library.md)),
  e.g. `"crbd"`, or the path to a `.tppl` model file.

- data:

  A named list with one element per model argument.

- filename:

  Path of the JSON file to write. If `NULL` (default), the file is
  written to
  [`tp_tempdir()`](http://treeppl.org/treepplr/reference/tp_tempdir.md)
  and named after the model and a hash of `data`, so the same data
  always gets the same file name and different data a different one.

- check:

  Logical. If `TRUE` (default), check the written file against the model
  with
  [`tp_input_check()`](http://treeppl.org/treepplr/reference/tp_input_check.md)
  and print the result.

- print:

  Logical. If `TRUE` (default), print a summary of the input file: its
  path, the model and the arguments it expects.

## Value

Invisibly, a list of class `tpData` with elements:

- `data_path`: path to the JSON file.

- `model_name`: the model's name.

- `expect_args`: the model's argument names, in signature order.

- `expect_types`: the TreePPL type of each argument.

- `expect_class`: the R object expected for each argument.

Typing the object at the console prints its summary again (see
[`print.tpData()`](http://treeppl.org/treepplr/reference/print.tpData.md)).

## Details

Each element of `data` must be named exactly like an argument of the
model; the order does not matter. Use
[`tp_expected_input()`](http://treeppl.org/treepplr/reference/tp_expected_input.md)
to see the names and the R object each argument expects.

Trees must be given as `tpPhylo` objects, built from a `phylo` tree with
[`tp_phylo_to_tpPhylo()`](http://treeppl.org/treepplr/reference/tp_phylo_to_tpPhylo.md).
A `phylo` object in `data` is not converted automatically and stops
`tp_data()` with an error.

The file is written with the settings TreePPL needs: values of length 1
become plain JSON values, numbers keep full precision, and matrices are
written row by row.

## See also

[`tp_expected_input()`](http://treeppl.org/treepplr/reference/tp_expected_input.md)
for the arguments a model expects,
[`tp_phylo_to_tpPhylo()`](http://treeppl.org/treepplr/reference/tp_phylo_to_tpPhylo.md)
to build trees,
[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md) to run a
sampler on the file.

## Examples

``` r
if (FALSE) { # \dontrun{
phy <- ape::read.tree(text = "((A:4,B:4):6,(C:7,D:7):3);")

# Diversification model: a tree and the sampling fraction
d <- tp_data(
  model = "crbd",
  data = list(tree = tp_phylo_to_tpPhylo(phy), rho = 0.5)
)
d            # prints the summary again
d$data_path  # path to the JSON file

# Write to a chosen file, without the check or the summary
d <- tp_data(
  model = "crbd",
  data = list(tree = tp_phylo_to_tpPhylo(phy), rho = 0.5),
  filename = "crbd_input.json",
  check = FALSE,
  print = FALSE
)
} # }
```
