# Open a file in the editor

`tp_open_tppl()` opens a `.tppl` model file, so it can be checked and
edited. In RStudio or Positron it opens in an editor tab; elsewhere it
opens in the editor R is configured to use.

## Usage

``` r
tp_open_tppl(path)
```

## Arguments

- path:

  Path to the `.tppl` file, or just the model name if the model file is
  already in TreePPL's model library. Use
  [`tp_model_library()`](http://treeppl.org/treepplr/reference/tp_model_library.md)
  to check the models in TreePPL's model library (see Examples).

## Value

`path`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check models available in TreePPL's model library:
tp_model_library()

tp_open_tppl("clads2")

# Or use the path to a tppl file:
tp_open_tppl("fake_path_to/crbd.tppl")
} # }
```
