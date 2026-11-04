# TreePPL compiler options

`tp_treeppl_options()` reads the inference methods and options supported
by the TreePPL compiler (`tpplc --help`) and displays them in the
console. Inference methods are passed to the `method` argument of
[`tp_sampler()`](http://treeppl.org/treepplr/reference/tp_sampler.md),
compile options to the `...` of
[`tp_sampler()`](http://treeppl.org/treepplr/reference/tp_sampler.md),
and runtime options to the `...` of
[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md).

## Usage

``` r
tp_treeppl_options(what = c("all", "methods", "compile", "runtime"))
```

## Arguments

- what:

  Which information to show: `"all"` (inference methods, compile options
  and runtime options), `"methods"`, `"compile"` or `"runtime"`.

## Value

A data frame with columns `type` (`"methods"`, `"compile"` or
`"runtime"`), `argument`, `value` and `description` (invisibly). For
methods, `argument` is the method name (e.g., `"mcmc"`) and `value` is
`NA`; for options, `argument` is the argument name (e.g.,
`"subsample_size"`) and `value` the type of value expected: `"integer"`,
`"number"`, `"string"` or `"logical"` (flags without a value).

## Details

Options are shown in the form accepted by
[`tp_sampler()`](http://treeppl.org/treepplr/reference/tp_sampler.md)
and [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md), with
the type of value they expect. The type is translated from the
placeholder used in `tpplc --help`:

- `INT` or `N`: `<integer>`, e.g. `--subsample-size INT` is shown as
  `subsample_size = <integer>`;

- `FLOAT`: `<number>`;

- any other placeholder (`ARG`, `FILE`, `<option>`, ...): `<string>`;

- no placeholder (a flag without a value): `<logical>`, e.g.
  `subsample = <logical>`. Use `TRUE` to set the flag.

## Examples

``` r
if (FALSE) { # \dontrun{
tp_treeppl_options("runtime")

# the returned data frame can be searched
opts <- tp_treeppl_options("compile")
opts[grepl("resample", opts$argument), ]
} # }
```
