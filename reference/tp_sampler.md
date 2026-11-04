# Build a TreePPL sampler for a given model and inference method

`tp_sampler()` compiles a TreePPL model with the chosen inference method
and returns a sampler that can be run with
[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md). The model
is compiled once; the sampler can then be run many times, with different
data or runtime options, without compiling again.

## Usage

``` r
tp_sampler(model, method = "mcmc", print = TRUE, ...)
```

## Arguments

- model:

  One of three options:

  - the path to a `.tppl` file with the TreePPL code, OR

  - the name of a model in the TreePPL model library (see
    [`tp_model_library()`](http://treeppl.org/treepplr/reference/tp_model_library.md)),
    e.g. `"crbd"`, OR

  - a string containing the entire TreePPL code.

  These are tried in this order: an existing file first, then a library
  model, and otherwise the string is treated as code.

- method:

  Inference method to compile the model with. See
  `tp_treeppl_options("methods")` for all supported methods.

- print:

  Whether to print information about the sampler when it is created.

- ...:

  Compile options, as `name = value` pairs or a single list (see
  [`tp_list()`](http://treeppl.org/treepplr/reference/tp_list.md)). See
  `tp_treeppl_options("compile")` for all supported options. Names are
  written without `--`, and underscores are converted to hyphens (e.g.,
  `incremental_printing = TRUE` becomes `--incremental-printing`).
  Runtime options are not allowed here; pass them to
  [`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md) instead.

## Value

Invisibly, a list of class `tpSampler` with elements:

- `exe_path`: path to the compiled sampler executable.

- `model_path`: path to the `.tppl` file that was compiled.

- `method`: the inference method.

- `compile_opt`: the compile options used, as a named list.

## See also

[`tp_run()`](http://treeppl.org/treepplr/reference/tp_run.md) to run the
sampler, [`tp_list()`](http://treeppl.org/treepplr/reference/tp_list.md)
to collect options,
[`tp_treeppl_options()`](http://treeppl.org/treepplr/reference/tp_treeppl_options.md)
for the available methods and options.

## Examples

``` r
if (FALSE) { # \dontrun{
# Library model, SMC with aligned resampling and pruning
sampler <- tp_sampler(
  model = "crbd",
  method = "smc-bpf",
  resample = "align",
  prune = TRUE
)

# Your own model file, default method (MCMC)
sampler <- tp_sampler(model = "path/to/my_model.tppl")
} # }
```
