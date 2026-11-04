# Write a custom model to tp_tempdir()

`tp_write_model()` writes TreePPL code to a `.tppl` file in
[`tp_tempdir()`](http://treeppl.org/treepplr/reference/tp_tempdir.md).

## Usage

``` r
tp_write_model(model, model_file_name = "tmp_model_file")
```

## Arguments

- model:

  A string containing the entire TreePPL code.

- model_file_name:

  Name of the file to create, without extension.

## Value

The path to the file created.
