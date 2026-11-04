# Check a JSON input file against a TreePPL model

Compares the top-level fields of a JSON input file with the arguments of
the `model function` in a `.tppl` file, and reports the result in the
console. Use it before running a model to catch misspelled, missing or
extra inputs.

## Usage

``` r
tp_input_check(json, tppl)
```

## Arguments

- json:

  Path to a JSON input file, e.g. one written by
  [`tp_write_tppl_json()`](http://treeppl.org/treepplr/reference/tp_write_tppl_json.md).

- tppl:

  Path to the `.tppl` model file.

## Value

Invisibly, a list of class `tpIcheck` with elements:

- `json_file`, `model_file`: the paths given in `json` and `tppl`.

- `model_name`: the file name of `tppl` without `.tppl`.

- `valid`: `TRUE` if the JSON fields and the model arguments match
  exactly, `FALSE` otherwise.

- `missing`: model arguments that are not in the JSON file.

- `extra`: JSON fields that the model does not take.

- `expect_args`: all model arguments, in signature order.

- `fields`: all top-level JSON fields.

## Details

The check covers names only: it does not test whether each value has the
right type. See
[`tp_expected_input()`](http://treeppl.org/treepplr/reference/tp_expected_input.md)
for the R object each argument expects.

## See also

[`print.tpIcheck()`](http://treeppl.org/treepplr/reference/print.tpIcheck.md)
for the console output,
[`tp_expected_input()`](http://treeppl.org/treepplr/reference/tp_expected_input.md),
[`tp_write_tppl_json()`](http://treeppl.org/treepplr/reference/tp_write_tppl_json.md).

## Examples

``` r
if (FALSE) { # \dontrun{
tp_input_check(
  json = "json/testdata_qt.json",
  tppl = "models/pheno-mol/qt.tppl"
)

# Stop a script if the input does not match the model
if (!tp_input_check("input.json", "qt.tppl")$valid) {
  stop("Fix the input file first.")
}
} # }
```
