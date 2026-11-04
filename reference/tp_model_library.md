# Models implemented in treepplr

`tp_model_library()` lists all models in the TreePPL model library.

## Usage

``` r
tp_model_library()
```

## Value

A data frame of class `tp_model_library` with columns `category` and
`model_name`, ordered by category and then by model name. Printing the
object shows the models grouped by category. It can still be subset like
a regular data frame, for example `tp_model_library()$model_name`.
