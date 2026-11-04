# Temporary directory for running TreePPL

`tp_tempdir()` creates (if needed) and returns a normalized path to a
temporary directory where the executables can read and write files.

## Usage

``` r
tp_tempdir(temp_dir = NULL, sub = NULL)
```

## Arguments

- temp_dir:

  A path to be used, or `NULL` to use R's
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).

- sub:

  Optional name of a sub-directory inside `temp_dir`.

## Value

A normalized path ending with the system-dependent file separator.
